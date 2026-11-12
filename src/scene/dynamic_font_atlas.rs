use std::cmp::max;

use bymsdfgen_core::bitmap::Bitmap;
use bymsdfgen_core::coloring::edge_coloring_simple;
use bymsdfgen_core::generator::{DistanceMapping, MsdfGeneratorConfig, Projection, SdfTransformation, generate_mtsdf};
use bymsdfgen_core::geometry::Shape;
use bymsdfgen_core::math::{Range, Vector2 as BVector2};
use bymsdfgen_io::font::{Font as MsdfFont, FontCoordinateScaling};
use nalgebra::{Vector2, Vector4};

use crate::scene::font::{FontData, GlyphMetric};

pub const DYNAMIC_FONT_ATLAS_SIZE: u32 = 4096;
pub const DYNAMIC_FONT_CELL_SIZE: u32 = 96;
pub const DYNAMIC_FONT_RENDER_SIZE: f32 = 72.0;
pub const DYNAMIC_FONT_SPREAD: f32 = 4.0;
pub const MAX_DYNAMIC_GLYPHS: usize = 4096;

/// 1D Felzenszwalb-Huttenlocher Distance Transform (parabolic envelope)
#[allow(dead_code)]
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

/// 2D Euclidean Distance Transform for a rectangular grid
#[allow(dead_code)]
fn edt_2d(f: &[f32], d: &mut [f32], w: usize, h: usize) {
    let max_dim = max(w, h) + 2;
    let mut v = vec![0usize; max_dim];
    let mut z = vec![0.0f32; max_dim + 1];

    let mut temp = vec![0.0f32; w * h];

    // Pass 1: Columns
    let mut col_in = vec![0.0f32; h];
    let mut col_out = vec![0.0f32; h];
    for x in 0..w {
        for y in 0..h {
            col_in[y] = f[y * w + x];
        }
        edt_1d(&col_in, &mut col_out, h, &mut v, &mut z);
        for y in 0..h {
            temp[y * w + x] = col_out[y];
        }
    }

    // Pass 2: Rows
    let mut row_in = vec![0.0f32; w];
    let mut row_out = vec![0.0f32; w];
    for y in 0..h {
        for x in 0..w {
            row_in[x] = temp[y * w + x];
        }
        edt_1d(&row_in, &mut row_out, w, &mut v, &mut z);
        for x in 0..w {
            d[y * w + x] = row_out[x];
        }
    }
}

pub struct DynamicFontAtlas {
    pub _font_name: String,
    pub _font_bytes: Vec<u8>,
    pub _atlas_width: u32,
    pub _atlas_height: u32,
    pub _cell_height: u32,
    pub _font_render_size: f32,
    pub _spread: f32,
    pub _stroke_bias: f32,
    pub _cursor_x: u32,
    pub _cursor_y: u32,
    pub _pixels: Vec<Vector4<u8>>,
    pub _char_to_index: Vec<i32>,
    pub _glyphs: Vec<GlyphMetric>,
    pub _is_dirty: bool,
    pub _dirty_min_x: u32,
    pub _dirty_min_y: u32,
    pub _dirty_max_x: u32,
    pub _dirty_max_y: u32,
}

impl DynamicFontAtlas {
    pub fn create_dynamic_font_atlas(font_name: &str, font_bytes: Vec<u8>) -> DynamicFontAtlas {
        let atlas_w = DYNAMIC_FONT_ATLAS_SIZE;
        let atlas_h = DYNAMIC_FONT_ATLAS_SIZE;
        let mut char_to_index = Vec::new();
        char_to_index.resize(65536, -1);

        let stroke_bias = if font_name.contains("Ubuntu") {
            0.65
        } else if font_name.contains("Nanum") {
            0.35
        } else {
            0.15
        };

        DynamicFontAtlas {
            _font_name: font_name.to_string(),
            _font_bytes: font_bytes,
            _atlas_width: atlas_w,
            _atlas_height: atlas_h,
            _cell_height: DYNAMIC_FONT_CELL_SIZE,
            _font_render_size: DYNAMIC_FONT_RENDER_SIZE,
            _spread: DYNAMIC_FONT_SPREAD,
            _stroke_bias: stroke_bias,
            _cursor_x: 0,
            _cursor_y: 0,
            _pixels: vec![Vector4::new(0, 0, 0, 0); (atlas_w * atlas_h) as usize],
            _char_to_index: char_to_index,
            _glyphs: Vec::with_capacity(MAX_DYNAMIC_GLYPHS),
            _is_dirty: true,
            _dirty_min_x: 0,
            _dirty_min_y: 0,
            _dirty_max_x: atlas_w,
            _dirty_max_y: atlas_h,
        }
    }

    pub fn get_stroke_bias(&self) -> f32 {
        self._stroke_bias
    }

    pub fn set_stroke_bias(&mut self, stroke_bias: f32) {
        self._stroke_bias = stroke_bias;
    }

    pub fn get_glyph_index(&self, ch: char) -> Option<u32> {
        let code = ch as usize;
        if code < self._char_to_index.len() {
            let idx = self._char_to_index[code];
            if idx >= 0 {
                return Some(idx as u32);
            }
        }
        None
    }

    pub fn get_glyph_metric(&self, ch: char) -> Option<&GlyphMetric> {
        let idx = self.get_glyph_index(ch)? as usize;
        self._glyphs.get(idx)
    }

    pub fn get_or_create_glyph(&mut self, ch: char) -> Option<u32> {
        let code = ch as usize;
        if code < self._char_to_index.len() {
            let idx = self._char_to_index[code];
            if idx >= 0 {
                return Some(idx as u32);
            }
        } else {
            self._char_to_index.resize(code + 1, -1);
        }

        if self._glyphs.len() >= MAX_DYNAMIC_GLYPHS {
            log::warn!("DynamicFontAtlas glyph capacity exceeded ({})", MAX_DYNAMIC_GLYPHS);
            return None;
        }

        let is_full = FontData::is_fullwidth(ch);
        let slot_w = if is_full {
            self._cell_height
        } else {
            self._cell_height / 2
        };
        let slot_h = self._cell_height;

        // Shelf packing allocation
        if self._cursor_x + slot_w > self._atlas_width {
            self._cursor_x = 0;
            self._cursor_y += self._cell_height;
        }

        if self._cursor_y + slot_h > self._atlas_height {
            log::warn!("DynamicFontAtlas is full! ({})", self._font_name);
            return None;
        }

        let slot_x = self._cursor_x;
        let slot_y = self._cursor_y;
        self._cursor_x += slot_w;

        let inv_atlas_w = 1.0 / self._atlas_width as f32;
        let inv_atlas_h = 1.0 / self._atlas_height as f32;
        let norm_advance = if is_full { 1.0f32 } else { 0.5f32 };

        if ch == ' ' {
            let metric = GlyphMetric {
                _uv_min: Vector2::new(slot_x as f32 * inv_atlas_w, slot_y as f32 * inv_atlas_h),
                _uv_max: Vector2::new(
                    (slot_x + slot_w) as f32 * inv_atlas_w,
                    (slot_y + slot_h) as f32 * inv_atlas_h,
                ),
                _plane_bounds: Vector4::new(0.0, 0.0, 0.0, 1.0),
                _advance_x: norm_advance,
            };
            let new_idx = self._glyphs.len() as u32;
            self._glyphs.push(metric);
            self._char_to_index[code] = new_idx as i32;
            return Some(new_idx);
        }

        // Parse font glyph vector outline and metrics via bymsdfgen_io / ttf-parser
        let font = match MsdfFont::from_slice(&self._font_bytes, 0) {
            Ok(f) => f,
            Err(e) => {
                log::error!("Failed to parse TTF font bytes for {}: {:?}", self._font_name, e);
                return None;
            }
        };

        let metrics = font.metrics(FontCoordinateScaling::EmNormalized);
        let glyph_id = font.glyph_index(ch);

        let sw = slot_w as usize;
        let sh = slot_h as usize;
        let em_size = self._font_render_size as f64;

        let advance = glyph_id
            .and_then(|gid| font.advance(gid, FontCoordinateScaling::EmNormalized))
            .unwrap_or(if is_full { 1.0 } else { 0.5 });
        let adv_px = advance * em_size;
        let ox = (slot_w as f64 - adv_px) * 0.5;
        let oy = (slot_h as f64 - em_size) * 0.5 - metrics.descender * em_size;

        let px_spread = self._spread as f64;
        let spread_em = px_spread / em_size;
        let bias_em = self._stroke_bias as f64 / em_size;

        let range = Range {
            lower: -spread_em - bias_em,
            upper: spread_em - bias_em,
        };

        let mut shape = Shape::new();
        let has_outline =
            glyph_id.map(|gid| font.load_glyph(&mut shape, gid, FontCoordinateScaling::EmNormalized)).unwrap_or(false);

        if has_outline && !shape.contours.is_empty() {
            edge_coloring_simple(&mut shape, 3.0, 0);

            let transformation = SdfTransformation {
                projection: Projection::new(
                    BVector2::new(em_size, em_size),
                    BVector2::new(ox / em_size, oy / em_size),
                ),
                distance_mapping: DistanceMapping::from_range(range),
            };

            let mut bitmap = Bitmap::<f32, 4>::new(sw, sh);
            let config = MsdfGeneratorConfig::default();
            generate_mtsdf(&mut bitmap, &shape, &transformation, &config);

            // Copy bitmap (bottom-up Y in bymsdfgen) to atlas (top-down Y in Vulkan)
            // Channels: RGB = MSDF (Multi-channel Signed Distance Field), A = True SDF (Signed Distance Field)
            for y in 0..sh {
                let atlas_y = slot_y + (slot_h - 1 - y as u32);
                for x in 0..sw {
                    let p = bitmap.pixel(x, y);
                    let r = (p[0].clamp(0.0, 1.0) * 255.0).round() as u8;
                    let g = (p[1].clamp(0.0, 1.0) * 255.0).round() as u8;
                    let b = (p[2].clamp(0.0, 1.0) * 255.0).round() as u8;
                    let a = (p[3].clamp(0.0, 1.0) * 255.0).round() as u8;
                    let atlas_idx = (atlas_y * self._atlas_width + (slot_x + x as u32)) as usize;
                    self._pixels[atlas_idx] = Vector4::new(r, g, b, a);
                }
            }
        } else {
            for y in 0..sh {
                let atlas_y = slot_y + y as u32;
                for x in 0..sw {
                    let atlas_idx = (atlas_y * self._atlas_width + (slot_x + x as u32)) as usize;
                    self._pixels[atlas_idx] = Vector4::new(0, 0, 0, 0);
                }
            }
        }

        // Expand dirty rect for GPU sync
        if self._is_dirty {
            self._dirty_min_x = self._dirty_min_x.min(slot_x);
            self._dirty_min_y = self._dirty_min_y.min(slot_y);
            self._dirty_max_x = self._dirty_max_x.max(slot_x + slot_w);
            self._dirty_max_y = self._dirty_max_y.max(slot_y + slot_h);
        } else {
            self._dirty_min_x = slot_x;
            self._dirty_min_y = slot_y;
            self._dirty_max_x = slot_x + slot_w;
            self._dirty_max_y = slot_y + slot_h;
            self._is_dirty = true;
        }

        // Glyph typography metrics
        let u1 = slot_x as f32 * inv_atlas_w;
        let v1 = slot_y as f32 * inv_atlas_h;
        let u2 = (slot_x + slot_w) as f32 * inv_atlas_w;
        let v2 = (slot_y + slot_h) as f32 * inv_atlas_h;

        let em_size = self._font_render_size;
        let norm_w = slot_w as f32 / em_size;
        let norm_h = slot_h as f32 / em_size;
        let center_x = norm_advance * 0.5;
        let x1 = center_x - norm_w * 0.5;
        let x2 = center_x + norm_w * 0.5;
        let y1 = -((slot_h as f32 - em_size) * 0.5) / em_size;
        let y2 = y1 + norm_h;

        let metric = GlyphMetric {
            _uv_min: Vector2::new(u1, v1),
            _uv_max: Vector2::new(u2, v2),
            _plane_bounds: Vector4::new(x1, y1, x2, y2),
            _advance_x: norm_advance,
        };

        let new_idx = self._glyphs.len() as u32;
        self._glyphs.push(metric);
        self._char_to_index[code] = new_idx as i32;
        Some(new_idx)
    }

    pub fn prewarm_ascii(&mut self) {
        self.get_or_create_glyph(' ');
        for cp in 33u32..=126u32 {
            if let Some(ch) = char::from_u32(cp) {
                self.get_or_create_glyph(ch);
            }
        }
    }

    pub fn get_dirty_rect(&self) -> Option<(u32, u32, u32, u32)> {
        if self._is_dirty && self._dirty_max_x > self._dirty_min_x && self._dirty_max_y > self._dirty_min_y {
            Some((
                self._dirty_min_x,
                self._dirty_min_y,
                self._dirty_max_x - self._dirty_min_x,
                self._dirty_max_y - self._dirty_min_y,
            ))
        } else {
            None
        }
    }

    pub fn clear_dirty(&mut self) {
        self._is_dirty = false;
        self._dirty_min_x = u32::MAX;
        self._dirty_min_y = u32::MAX;
        self._dirty_max_x = 0;
        self._dirty_max_y = 0;
    }

    pub fn mark_all_dirty(&mut self) {
        self._dirty_min_x = 0;
        self._dirty_min_y = 0;
        self._dirty_max_x = self._atlas_width;
        self._dirty_max_y = self._atlas_height;
        self._is_dirty = true;
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use crate::utilities::system::newRcRefCell;

    #[test]
    fn test_dynamic_font_atlas_creation_and_ascii_prewarm() {
        let root = if PathBuf::from("resources").exists() {
            PathBuf::from(".")
        } else {
            PathBuf::from("..")
        };
        let ttf_path = root.join("resources/engine_resources/externals/fonts/D2Coding-Regular.ttf");
        if !ttf_path.exists() {
            return;
        }

        let font_bytes = std::fs::read(&ttf_path).expect("Failed to read test font");
        let mut atlas = DynamicFontAtlas::create_dynamic_font_atlas("D2Coding-Regular", font_bytes);

        atlas.prewarm_ascii();

        // 1 (space) + 94 (ASCII 33..=126) = 95 characters
        assert_eq!(atlas._glyphs.len(), 95);
        assert!(atlas.get_glyph_index(' ').is_some());
        assert!(atlas.get_glyph_index('A').is_some());
        assert!(atlas.get_glyph_index('z').is_some());
        assert!(atlas.get_glyph_index('9').is_some());

        // Check stroke bias defaults
        assert_eq!(atlas.get_stroke_bias(), 0.15);

        let ubuntu_atlas = DynamicFontAtlas::create_dynamic_font_atlas("UbuntuSansMono-Regular", vec![]);
        assert_eq!(ubuntu_atlas.get_stroke_bias(), 0.65);

        let nanum_atlas = DynamicFontAtlas::create_dynamic_font_atlas("NanumGothicCoding-Regular", vec![]);
        assert_eq!(nanum_atlas.get_stroke_bias(), 0.35);

        // Check advance metrics
        let space_metric = atlas.get_glyph_metric(' ').unwrap();
        assert_eq!(space_metric._advance_x, 0.5);

        let a_metric = atlas.get_glyph_metric('A').unwrap();
        assert_eq!(a_metric._advance_x, 0.5);

        // Check dirty rect
        let dirty = atlas.get_dirty_rect();
        assert!(dirty.is_some());
        let (_dx, _dy, dw, dh) = dirty.unwrap();
        assert!(dw > 0);
        assert!(dh > 0);

        atlas.clear_dirty();
        assert!(atlas.get_dirty_rect().is_none());
    }

    #[test]
    fn test_dynamic_font_atlas_korean_on_demand_and_rare_syllables() {
        let root = if PathBuf::from("resources").exists() {
            PathBuf::from(".")
        } else {
            PathBuf::from("..")
        };
        let ttf_path = root.join("resources/engine_resources/externals/fonts/D2Coding-Regular.ttf");
        if !ttf_path.exists() {
            return;
        }

        let font_bytes = std::fs::read(&ttf_path).expect("Failed to read test font");
        let mut atlas = DynamicFontAtlas::create_dynamic_font_atlas("D2Coding-Regular", font_bytes);

        // Standard KS X 1001 syllable
        let idx_ga = atlas.get_or_create_glyph('가');
        assert!(idx_ga.is_some());
        let metric_ga = *atlas.get_glyph_metric('가').unwrap();
        assert_eq!(metric_ga._advance_x, 1.0); // Fullwidth advance

        // Syllable outside KS X 1001 (e.g. '뷁', '흵')
        let idx_bwek = atlas.get_or_create_glyph('뷁');
        assert!(idx_bwek.is_some());
        let metric_bwek = *atlas.get_glyph_metric('뷁').unwrap();
        assert_eq!(metric_bwek._advance_x, 1.0);

        // Subsequent lookup should return cached index without reallocating
        assert_eq!(atlas.get_or_create_glyph('가'), idx_ga);
        assert_eq!(atlas.get_or_create_glyph('뷁'), idx_bwek);

        // Check UV bounds are within [0.0, 1.0]
        assert!(metric_ga._uv_min.x >= 0.0 && metric_ga._uv_max.x <= 1.0);
        assert!(metric_ga._uv_min.y >= 0.0 && metric_ga._uv_max.y <= 1.0);
        assert!(metric_ga._uv_max.x > metric_ga._uv_min.x);
        assert!(metric_ga._uv_max.y > metric_ga._uv_min.y);
    }

    #[test]
    fn test_dynamic_font_data_integration() {
        let root = if PathBuf::from("resources").exists() {
            PathBuf::from(".")
        } else {
            PathBuf::from("..")
        };
        let ttf_path = root.join("resources/engine_resources/externals/fonts/Pretendard-Regular.ttf");
        if !ttf_path.exists() {
            return;
        }

        let font_bytes = std::fs::read(&ttf_path).expect("Failed to read test font");
        let texture = newRcRefCell(crate::vulkan_context::texture::TextureData::default());
        let font_data = FontData::create_dynamic_font_data("Pretendard-Regular_Korean", font_bytes, texture);

        assert!(font_data.is_dynamic());
        assert!(font_data.has_glyph_metrics());
        assert!(font_data.is_cjk_font());

        // Prewarmed ASCII
        assert!(font_data.get_glyph_metric('A').is_some());

        // On-demand Korean glyph
        let metric_han = font_data.get_glyph_metric('한');
        assert!(metric_han.is_some());
        assert_eq!(metric_han.unwrap()._advance_x, 1.0);

        // Batch ensure_glyphs
        font_data.ensure_glyphs("동적 폰트 아틀라스 테스트");
        assert!(font_data.get_glyph_metric('동').is_some());
        assert!(font_data.get_glyph_metric('적').is_some());
        assert!(font_data.get_glyph_metric('트').is_some());
        assert!(font_data.get_glyph_metric('라').is_some());
        assert!(font_data.get_glyph_metric('스').is_some());
    }

    #[test]
    fn test_benchmark_glyph_generation_time() {
        let root = if PathBuf::from("resources").exists() {
            PathBuf::from(".")
        } else {
            PathBuf::from("..")
        };
        let ttf_path = root.join("resources/engine_resources/externals/fonts/D2Coding-Regular.ttf");
        if !ttf_path.exists() {
            return;
        }

        let font_bytes = std::fs::read(&ttf_path).expect("Failed to read test font");
        let mut atlas = DynamicFontAtlas::create_dynamic_font_atlas("D2Coding-Regular", font_bytes);

        // Warm up / parse font
        atlas.get_or_create_glyph('A');

        // Benchmark ASCII (simple Latin)
        let ascii_samples = ['B', 'g', 'W', 'm', '7', '@', '%', '{', 'Q', 'k'];
        let t0 = std::time::Instant::now();
        for &ch in &ascii_samples {
            atlas.get_or_create_glyph(ch);
        }
        let elapsed_ascii = t0.elapsed();
        let per_ascii_us = elapsed_ascii.as_micros() as f64 / ascii_samples.len() as f64;

        // Benchmark Korean (medium syllables)
        let korean_samples = ['한', '글', '동', '적', '폰', '트', '생', '성', '속', '도'];
        let t1 = std::time::Instant::now();
        for &ch in &korean_samples {
            atlas.get_or_create_glyph(ch);
        }
        let elapsed_korean = t1.elapsed();
        let per_korean_us = elapsed_korean.as_micros() as f64 / korean_samples.len() as f64;

        // Benchmark Complex Korean (heavy multi-contour syllables)
        let complex_samples = ['틈', '똠', '뷁', '흵', '값', '환', '닭', '밟', '끓', '흙'];
        let t2 = std::time::Instant::now();
        for &ch in &complex_samples {
            atlas.get_or_create_glyph(ch);
        }
        let elapsed_complex = t2.elapsed();
        let per_complex_us = elapsed_complex.as_micros() as f64 / complex_samples.len() as f64;

        println!("\n=== GLYPH GENERATION BENCHMARK RESULTS ===");
        println!(
            "  ASCII (Simple)    : {:.2} ms per glyph ({:.1} us)",
            per_ascii_us / 1000.0,
            per_ascii_us
        );
        println!(
            "  Korean (Standard) : {:.2} ms per glyph ({:.1} us)",
            per_korean_us / 1000.0,
            per_korean_us
        );
        println!(
            "  Korean (Complex)  : {:.2} ms per glyph ({:.1} us)",
            per_complex_us / 1000.0,
            per_complex_us
        );
    }

    #[test]
    fn test_dynamic_font_atlas_glyph_generation_and_metrics() {
        let root = if PathBuf::from("resources").exists() {
            PathBuf::from(".")
        } else {
            PathBuf::from("..")
        };
        for font_name in &["D2Coding-Regular", "UbuntuSansMono-Regular", "Pretendard-Regular"] {
            let ttf_path = root.join(format!("resources/engine_resources/externals/fonts/{}.ttf", font_name));
            if !ttf_path.exists() {
                continue;
            }
            let font_bytes = std::fs::read(&ttf_path).expect("Failed to read font");
            let mut atlas = DynamicFontAtlas::create_dynamic_font_atlas(font_name, font_bytes);
            assert!(atlas.get_or_create_glyph('A').is_some());
            assert!(atlas.get_or_create_glyph('틈').is_some());
            assert!(atlas.get_or_create_glyph('똠').is_some());

            for ch in ['A', '틈', '똠'] {
                let metric = atlas.get_glyph_metric(ch);
                assert!(metric.is_some());
                let m = metric.unwrap();
                assert!(m._uv_max.x > m._uv_min.x);
                assert!(m._uv_max.y > m._uv_min.y);
            }
        }
    }
}
