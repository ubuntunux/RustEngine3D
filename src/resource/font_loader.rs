use std::cmp::max;
use std::path::PathBuf;

use image::{DynamicImage, Rgba};
use rusttype::{Font, PositionedGlyph, Scale, point};

use crate::scene::font::{self, FontDataCreateInfo};
use crate::utilities::system;

/// Standard special symbols useful in game and UI contexts
pub const SPECIAL_SYMBOLS: [char; 30] = [
    '·', '…', '‘', '’', '“', '”', '~', '₩', '★', '☆', '▶', '◀', '▲', '▼', '※', '℃', '±', '×', '÷', '°', '○', '●', '◇',
    '◆', '□', '■', '♪', '♬', '♥', '♡',
];

/// Builds a comprehensive character set for Korean font atlas generation:
/// - Index 0: Space / fallback slot
/// - ASCII 32..=126 (95 characters)
/// - Hangul Compatibility Jamo 0x3131..=0x3163 (51 characters)
/// - Common Special Symbols (30 characters)
/// - Modern Hangul Syllables 0xAC00..=0xD7A3 (11,172 characters)
pub fn build_korean_character_set() -> String {
    let mut chars = String::with_capacity(12000);
    chars.push(' ');

    for cp in 32u32..=126u32 {
        if let Some(ch) = char::from_u32(cp) {
            chars.push(ch);
        }
    }

    for cp in 0x3131u32..=0x3163u32 {
        if let Some(ch) = char::from_u32(cp) {
            chars.push(ch);
        }
    }

    for sym in SPECIAL_SYMBOLS.iter() {
        chars.push(*sym);
    }

    for cp in 0xAC00u32..=0xD7A3u32 {
        if let Some(ch) = char::from_u32(cp) {
            chars.push(ch);
        }
    }

    chars
}

/// 1D Felzenszwalb-Huttenlocher Distance Transform (parabolic envelope)
fn edt_1d(f: &[f32], d: &mut [f32], n: usize, v: &mut [usize], z: &mut [f32]) {
    let mut k = 0usize;
    v[0] = 0;
    z[0] = -1e20;
    z[1] = 1e20;

    for q in 1..n {
        let mut s = ((f[q] + (q * q) as f32) - (f[v[k]] + (v[k] * v[k]) as f32)) / (2 * q - 2 * v[k]) as f32;
        while s <= z[k] {
            k -= 1;
            s = ((f[q] + (q * q) as f32) - (f[v[k]] + (v[k] * v[k]) as f32)) / (2 * q - 2 * v[k]) as f32;
        }
        k += 1;
        v[k] = q;
        z[k] = s;
        z[k + 1] = 1e20;
    }

    k = 0;
    for q in 0..n {
        while z[k + 1] < q as f32 {
            k += 1;
        }
        let dx = (q as isize - v[k] as isize) as f32;
        d[q] = dx * dx + f[v[k]];
    }
}

/// Computes 2D Euclidean Distance Transform for a single rectangular cell
fn compute_cell_edt(
    image: &image::RgbaImage,
    offset_x: u32,
    offset_y: u32,
    cell_w: u32,
    cell_h: u32,
    max_dist: f32,
    distance_field_image: &mut image::RgbaImage,
    v_buf: &mut [usize],
    z_buf: &mut [f32],
    f_grid: &mut [f32],
    d_grid: &mut [f32],
) {
    let mut has_fg = false;
    let cw = cell_w as usize;
    let ch = cell_h as usize;

    for py in 0..cell_h {
        for px in 0..cell_w {
            let pixel = image.get_pixel(offset_x + px, offset_y + py);
            let idx = py as usize * cw + px as usize;
            if pixel[3] > 128 {
                f_grid[idx] = 0.0;
                has_fg = true;
            } else {
                f_grid[idx] = 1e10;
            }
        }
    }

    if !has_fg {
        for py in 0..cell_h {
            for px in 0..cell_w {
                distance_field_image.put_pixel(offset_x + px, offset_y + py, Rgba([255, 255, 255, 0]));
            }
        }
        return;
    }

    // Pass 1: 1D EDT on each column
    let mut col_in = vec![0.0f32; ch];
    let mut col_out = vec![0.0f32; ch];
    for px in 0..cw {
        for py in 0..ch {
            col_in[py] = f_grid[py * cw + px];
        }
        edt_1d(&col_in, &mut col_out, ch, v_buf, z_buf);
        for py in 0..ch {
            d_grid[py * cw + px] = col_out[py];
        }
    }

    // Pass 2: 1D EDT on each row
    let mut row_in = vec![0.0f32; cw];
    let mut row_out = vec![0.0f32; cw];
    for py in 0..ch {
        for px in 0..cw {
            row_in[px] = d_grid[py * cw + px];
        }
        edt_1d(&row_in, &mut row_out, cw, v_buf, z_buf);
        for px in 0..cw {
            let dist_sq = row_out[px];
            let dist = dist_sq.sqrt();
            let norm_dist = (dist / max_dist).clamp(0.0, 1.0);
            let alpha = 255u8.saturating_sub((norm_dist.sqrt() * 255.0).min(255.0) as u8);
            distance_field_image.put_pixel(offset_x + px as u32, offset_y + py as u32, Rgba([255, 255, 255, alpha]));
        }
    }
}

/// Generates a font atlas texture and metadata from an arbitrary character sequence.
/// Supports true monospace positioning (half-width in central slot, full-width in full cell).
pub fn get_font_data_create_info_from_chars(
    font_source_file: &PathBuf,
    cell_size: u32,
    font_render_size: f32,
    font_data_name: &str,
    font_texture_file_path: &PathBuf,
    chars: &str,
) -> FontDataCreateInfo {
    log::info!("create font texture from chars: {:?}", font_texture_file_path);
    let font_data = system::load(font_source_file);
    let font = Font::try_from_bytes(font_data.get_ref()).expect("Error constructing Font");
    let colour = (255, 255, 255);

    let char_vec: Vec<char> = chars.chars().collect();
    let total_count = char_vec.len() as u32;
    let count_of_side = (total_count as f32).sqrt().ceil() as u32;

    let cell_w = cell_size;
    let cell_h = cell_size;
    let atlas_w = cell_w * count_of_side;
    let atlas_h = cell_h * count_of_side;
    let max_dist = (max(cell_w, cell_h) - 1) as f32;

    let scale = Scale::uniform(font_render_size);
    let v_metrics = font.v_metrics(scale);

    let mut raw_image = DynamicImage::new_rgba8(atlas_w, atlas_h).to_rgba8();

    for (idx, &ch) in char_vec.iter().enumerate() {
        if ch == ' ' {
            continue;
        }

        let grid_x = (idx as u32) % count_of_side;
        let grid_y = (idx as u32) / count_of_side;
        let cell_ox = grid_x * cell_w;
        let cell_oy = grid_y * cell_h;

        let glyph = font.glyph(ch).scaled(scale);
        let h_metrics = glyph.h_metrics();
        let advance_w = h_metrics.advance_width;
        let is_full = advance_w > font_render_size * 0.7;

        let px = if is_full {
            cell_ox as f32 + (cell_w as f32 - advance_w) * 0.5
        } else {
            let half_slot_w = cell_w as f32 * 0.5;
            cell_ox as f32 + (cell_w as f32 * 0.25) + (half_slot_w - advance_w) * 0.5
        };

        // Align baseline
        let py = cell_oy as f32 + (cell_h as f32 - font_render_size) * 0.5 + v_metrics.ascent;
        let pos_glyph = glyph.positioned(point(px, py));

        if let Some(bbox) = pos_glyph.pixel_bounding_box() {
            pos_glyph.draw(|gx, gy, v| {
                let img_x = bbox.min.x + gx as i32;
                let img_y = bbox.min.y + gy as i32;
                if img_x >= 0 && img_x < atlas_w as i32 && img_y >= 0 && img_y < atlas_h as i32 {
                    let old_alpha = raw_image.get_pixel(img_x as u32, img_y as u32)[3];
                    let new_alpha = (v * 255.0) as u8;
                    raw_image.put_pixel(
                        img_x as u32,
                        img_y as u32,
                        Rgba([colour.0, colour.1, colour.2, old_alpha.max(new_alpha)]),
                    );
                }
            });
        }
    }

    // Distance Field Computation
    if font::USE_DISTANCE_FIELD {
        let max_dim = max(cell_w, cell_h) as usize + 2;
        let mut v_buf = vec![0usize; max_dim];
        let mut z_buf = vec![0.0f32; max_dim + 1];
        let mut f_grid = vec![0.0f32; (cell_w * cell_h) as usize];
        let mut d_grid = vec![0.0f32; (cell_w * cell_h) as usize];

        let mut distance_field_image = DynamicImage::new_rgba8(atlas_w, atlas_h).to_rgba8();

        for y in 0..count_of_side {
            for x in 0..count_of_side {
                let offset_x = x * cell_w;
                let offset_y = y * cell_h;
                compute_cell_edt(
                    &raw_image,
                    offset_x,
                    offset_y,
                    cell_w,
                    cell_h,
                    max_dist,
                    &mut distance_field_image,
                    &mut v_buf,
                    &mut z_buf,
                    &mut f_grid,
                    &mut d_grid,
                );
            }
        }

        distance_field_image.save(font_texture_file_path).unwrap();
    } else {
        raw_image.save(font_texture_file_path).unwrap();
    }

    FontDataCreateInfo {
        _font_data_name: String::from(font_data_name),
        _range_min: 32,
        _range_max: 126,
        _text_count: total_count,
        _count_of_side: count_of_side,
        _font_size: cell_size as f32,
        _chars: String::from(chars),
    }
}

/// Legacy / Range-based font atlas generator using high-speed EDT algorithm.
pub fn get_font_data_create_info(
    font_source_file: &PathBuf,
    font_size: f32,
    padding: f32,
    font_data_name: &str,
    font_texture_file_path: &PathBuf,
    range_min: u32,
    range_max: u32,
) -> FontDataCreateInfo {
    log::info!("create font texture: {:?}", font_texture_file_path);
    let font_data = system::load(font_source_file);
    let font = Font::try_from_bytes(font_data.get_ref()).expect("Error constructing Font");
    let colour = (255, 255, 255);
    let scale = Scale::uniform(font_size);
    let v_metrics = font.v_metrics(scale);
    let count = (range_max - range_min) + 1;
    let count_of_side = (count as f32).sqrt().ceil() as u32;
    let glyphs_height = (v_metrics.ascent - v_metrics.descent + padding * 2.0).ceil() as u32;
    let mut glyphs_width = 0;
    let mut glyphs_list: Vec<Vec<PositionedGlyph>> = Vec::new();

    for text in range_min..(range_max + 1) {
        let ch = char::from_u32(text).unwrap_or('?');
        let mut text_str = String::new();
        text_str.push(ch);
        let glyphs: Vec<PositionedGlyph> =
            font.layout(&text_str, scale, point(padding, padding + v_metrics.ascent)).collect();
        let glyphs_first_bounding_box = glyphs.first().and_then(|g| g.pixel_bounding_box());
        let glyphs_last_bounding_box = glyphs.last().and_then(|g| g.pixel_bounding_box());
        match (glyphs_first_bounding_box, glyphs_last_bounding_box) {
            (Some(glyphs_first_bounding_box), Some(glyphs_last_bounding_box)) => {
                let min_x = glyphs_first_bounding_box.min.x;
                let max_x = glyphs_last_bounding_box.max.x;
                glyphs_width = max(glyphs_width, ((max_x - min_x) as f32 + padding * 2.0) as u32);
            }
            _ => {}
        }
        glyphs_list.push(glyphs);
    }

    let mut image = DynamicImage::new_rgba8(glyphs_width * count_of_side, glyphs_height * count_of_side).to_rgba8();
    let mut text_index = range_min;
    for y in 0..count_of_side {
        for x in 0..count_of_side {
            let glyphs = &glyphs_list[(x + y * count_of_side) as usize];
            for glyph in glyphs.iter() {
                if let Some(bounding_box) = glyph.pixel_bounding_box() {
                    glyph.draw(|px, py, v| {
                        let px = px + bounding_box.min.x as u32 + x * glyphs_width;
                        let py = py + bounding_box.min.y as u32 + y * glyphs_height;
                        image.put_pixel(px, py, Rgba([colour.0, colour.1, colour.2, (v * 255.0) as u8]))
                    });
                }
            }

            text_index += 1;
            if range_max < text_index {
                break;
            }
        }
        if range_max < text_index {
            break;
        }
    }

    if font::USE_DISTANCE_FIELD {
        let cell_width = image.width() / count_of_side;
        let cell_height = image.height() / count_of_side;
        let max_dist = (max(cell_width, cell_height) - 1) as f32;
        let mut distance_field_image = DynamicImage::new_rgba8(image.width(), image.height()).to_rgba8();

        let max_dim = max(cell_width, cell_height) as usize + 2;
        let mut v_buf = vec![0usize; max_dim];
        let mut z_buf = vec![0.0f32; max_dim + 1];
        let mut f_grid = vec![0.0f32; (cell_width * cell_height) as usize];
        let mut d_grid = vec![0.0f32; (cell_width * cell_height) as usize];

        for y in 0..count_of_side {
            for x in 0..count_of_side {
                let offset_x = x * cell_width;
                let offset_y = y * cell_height;
                compute_cell_edt(
                    &image,
                    offset_x,
                    offset_y,
                    cell_width,
                    cell_height,
                    max_dist,
                    &mut distance_field_image,
                    &mut v_buf,
                    &mut z_buf,
                    &mut f_grid,
                    &mut d_grid,
                );
            }
        }

        distance_field_image.save(font_texture_file_path).unwrap();
    } else {
        image.save(font_texture_file_path).unwrap();
    }

    FontDataCreateInfo {
        _font_data_name: String::from(font_data_name),
        _range_min: range_min,
        _range_max: range_max,
        _text_count: range_max - range_min + 1,
        _count_of_side: count_of_side,
        _font_size: font_size,
        _chars: String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regenerate_ubuntu_sans_mono_textures() {
        let root = PathBuf::from("..");
        let fonts = [
            ("UbuntuSansMono-Bold", "UbuntuSansMono-Bold_Basic_Latin"),
            ("UbuntuSansMono-Regular", "UbuntuSansMono-Regular_Basic_Latin"),
        ];

        for (font_file, font_data_name) in fonts {
            let font_source_path = root.join(format!("resources/engine_resources/externals/fonts/{}.ttf", font_file));
            let font_texture_path = root.join(format!(
                "resources/engine_resources/externals/textures/fonts/{}.png",
                font_data_name
            ));

            if font_source_path.exists() {
                let create_info = get_font_data_create_info(
                    &font_source_path,
                    font::FONT_SIZE as f32,
                    font::FONT_PADDING as f32,
                    font_data_name,
                    &font_texture_path,
                    32,
                    127,
                );
                assert_eq!(create_info._text_count, 96);
                assert_eq!(create_info._count_of_side, 10);
                assert!(font_texture_path.exists());
            }
        }
    }

    #[test]
    fn test_regenerate_korean_monospace_font_via_font_loader() {
        let root = if PathBuf::from("resources").exists() {
            PathBuf::from(".")
        } else {
            PathBuf::from("..")
        };
        let fonts = [
            ("Pretendard-Bold", "Pretendard-Bold_Korean"),
            ("D2Coding-Regular", "D2Coding-Regular_Korean"),
            ("NanumGothicCoding-Regular", "NanumGothicCoding-Regular_Korean"),
        ];

        let chars = build_korean_character_set();
        for (font_file, font_data_name) in fonts {
            let font_source_path = root.join(format!("resources/engine_resources/externals/fonts/{}.ttf", font_file));
            let font_texture_path = root.join(format!(
                "resources/engine_resources/externals/textures/fonts/{}.png",
                font_data_name
            ));
            let font_file_path = root.join(format!("resources/engine_resources/fonts/{}.font", font_data_name));

            if font_source_path.exists() {
                let create_info = get_font_data_create_info_from_chars(
                    &font_source_path,
                    32,
                    24.0,
                    font_data_name,
                    &font_texture_path,
                    &chars,
                );
                assert_eq!(create_info._font_data_name, font_data_name);
                assert!(create_info._text_count > 11000);
                assert_eq!(create_info._count_of_side, 107);
                assert!(font_texture_path.exists());

                let write_contents = serde_json::to_string(&create_info).expect("Failed to serialize.");
                std::fs::write(&font_file_path, write_contents).expect("Failed to write font file");
            }
        }
    }

    #[test]
    fn test_auto_regenerate_missing_font_texture() {
        use crate::resource::resource::EngineResources;
        let font_data_name = "UbuntuSansMono-Regular_Basic_Latin";
        let found = EngineResources::find_font_source_file(font_data_name);
        assert!(found.is_some(), "Must locate font source for UbuntuSansMono-Regular");

        let pretendard_found = EngineResources::find_font_source_file("Pretendard-Bold_Korean");
        assert!(
            pretendard_found.is_some(),
            "Must locate font source for Pretendard-Bold"
        );

        let test_texture_path = PathBuf::from("/tmp/test_auto_gen_font_texture.png");
        if test_texture_path.exists() {
            let _ = std::fs::remove_file(&test_texture_path);
        }

        let create_info = FontDataCreateInfo {
            _font_data_name: font_data_name.to_string(),
            _range_min: 32,
            _range_max: 127,
            _text_count: 96,
            _count_of_side: 10,
            _font_size: font::FONT_SIZE as f32,
            _chars: String::new(),
        };

        EngineResources::ensure_font_texture(font_data_name, &create_info, &test_texture_path);
        assert!(
            test_texture_path.exists(),
            "ensure_font_texture must auto-generate missing png"
        );
        let _ = std::fs::remove_file(&test_texture_path);

        let test_korean_path = PathBuf::from("/tmp/test_korean_auto_gen.png");
        if test_korean_path.exists() {
            let _ = std::fs::remove_file(&test_korean_path);
        }
        let korean_info = FontDataCreateInfo {
            _font_data_name: "Pretendard-Bold_Korean".to_string(),
            _range_min: 0,
            _range_max: 0,
            _text_count: 5,
            _count_of_side: 3,
            _font_size: 24.0,
            _chars: " 가나다".to_string(),
        };
        EngineResources::ensure_font_texture("Pretendard-Bold_Korean", &korean_info, &test_korean_path);
        assert!(
            test_korean_path.exists(),
            "ensure_font_texture must auto-generate missing Korean png"
        );
        let _ = std::fs::remove_file(&test_korean_path);
    }
}
