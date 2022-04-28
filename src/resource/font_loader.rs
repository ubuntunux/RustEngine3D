use std::cmp::max;
use std::path::PathBuf;

use nalgebra::{ self, Vector2 };
use image::{ DynamicImage, Rgba };
use rusttype::{ point, Font, Scale, PositionedGlyph };

use crate::renderer::font::{ self, FontDataCreateInfo };
use crate::utilities::system;

pub fn get_font_data_create_info(
    font_source_file: &PathBuf,
    font_size: f32,
    padding: f32,
    font_data_name: &str,
    font_texture_file_path: &PathBuf,
    range_min: u32,
    range_max: u32,
) -> FontDataCreateInfo {
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
        assert!(text < 256, "Not implemented for unicode.");
        let glyphs: Vec<PositionedGlyph> = font.layout(String::from_utf8(vec![text as u8]).unwrap().as_str(), scale, point(padding, padding + v_metrics.ascent)).collect();
        let glyphs_first_bounding_box = glyphs.first().unwrap().pixel_bounding_box();
        let glyphs_last_bounding_box = glyphs.last().unwrap().pixel_bounding_box();
        match (glyphs_first_bounding_box, glyphs_last_bounding_box) {
            (Some(glyphs_first_bounding_box), Some(glyphs_last_bounding_box)) => {
                let min_x = glyphs_first_bounding_box.min.x;
                let max_x = glyphs_last_bounding_box.max.x;
                glyphs_width = max(glyphs_width, ((max_x - min_x) as f32 + padding * 2.0) as u32);
            },
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

    // Todo : 80x80 Font Image -> 20x20 Large Distance Field
    if font::USE_DISTANCE_FIELD {
        let cell_width = image.width() / count_of_side;
        let cell_height = image.height() / count_of_side;
        let max_dist = (max(cell_width, cell_height) - 1) as f32;
        let mut distance_field_image = DynamicImage::new_rgba8(image.width(), image.height()).to_rgba8();
        for y in 0..count_of_side {
            for x in 0..count_of_side {
                for py in 0..cell_height {
                    for px in 0..cell_width {
                        let mut min_dist: f32 = max_dist;
                        let offset_x = x * cell_width;
                        let offset_y = y * cell_height;
                        let pos: Vector2<f32> = Vector2::new((offset_x + px) as f32, (offset_y + py) as f32);
                        for sy in 0..cell_height {
                            for sx in 0..cell_width {
                                let pixel = image.get_pixel(offset_x + sx, offset_y + sy);
                                let opacity: f32 = pixel[3] as f32 / 255.0;
                                if 0.5 < opacity {
                                    let src_pos: Vector2<f32> = Vector2::new((offset_x + sx) as f32, (offset_y + sy) as f32);
                                    let dist = nalgebra_glm::distance(&pos, &src_pos);
                                    if dist < min_dist {
                                        min_dist = dist;
                                    }
                                }
                            }
                        }
                        distance_field_image.put_pixel(offset_x + px, offset_y + py, Rgba([colour.0, colour.1, colour.2, 255 - 255u8.min(((min_dist / max_dist).sqrt() * 255.0) as u8)]));
                    }
                }
            }
        }

        distance_field_image.save(&font_texture_file_path).unwrap();
    } else {
        image.save(&font_texture_file_path).unwrap();
    }


    FontDataCreateInfo {
        _font_data_name: String::from(font_data_name),
        _range_min: range_min,
        _range_max: range_max,
        _text_count: range_max - range_min + 1,
        _count_of_side: count_of_side,
        _font_size: font_size
    }
}