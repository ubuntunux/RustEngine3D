use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::SystemTime;

use ash::vk;
use rayon::prelude::*;
use rusty_dds::{D3DFormat, Dds, DdsView, DecodeContent, DxgiFormat, EncodeLayout};
use serde::{Deserialize, Serialize};

use crate::utilities::system;

pub const TEXTURE_RULES_FILE_NAME: &str = "texture_rules.json";
pub const EXT_TEXTURE_DDS: &str = "dds";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum TextureRuleFormat {
    #[default]
    Auto,
    BC1,
    BC2,
    BC3,
    BC4,
    BC5,
    BC7,
    RGBA8,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextureRuleEntry {
    #[serde(default, rename = "format")]
    pub _format: Option<TextureRuleFormat>,
    #[serde(default, rename = "mipmap")]
    pub _mipmap: Option<bool>,
    #[serde(default, rename = "srgb")]
    pub _srgb: Option<bool>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TextureRulesFile {
    #[serde(default = "default_rules_version", rename = "version")]
    pub _version: u32,
    #[serde(default, rename = "folder_rules")]
    pub _folder_rules: HashMap<String, TextureRuleEntry>,
    #[serde(default, rename = "overrides")]
    pub _overrides: HashMap<String, TextureRuleEntry>,
}

fn default_rules_version() -> u32 {
    1
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ResolvedTextureRule {
    pub _format: TextureRuleFormat,
    pub _mipmap: bool,
    pub _srgb: bool,
}

impl Default for ResolvedTextureRule {
    fn default() -> Self {
        Self {
            _format: TextureRuleFormat::Auto,
            _mipmap: true,
            _srgb: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoadedDdsTexture {
    pub _width: u32,
    pub _height: u32,
    pub _layers: u32,
    pub _mip_levels: u32,
    pub _format: vk::Format,
    pub _view_type: vk::ImageViewType,
    pub _data: Vec<u8>,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CookSummary {
    pub _cooked_count: usize,
    pub _skipped_count: usize,
    pub _removed_count: usize,
    pub _failed_count: usize,
}

impl CookSummary {
    pub fn has_changes(&self) -> bool {
        self._cooked_count > 0 || self._removed_count > 0
    }
}

fn normalize_resource_key(key: &str) -> String {
    key.replace('\\', "/").trim_start_matches('/').to_string()
}

fn normalize_folder_prefix(prefix: &str) -> String {
    let mut normalized = normalize_resource_key(prefix);
    if !normalized.is_empty() && !normalized.ends_with('/') {
        normalized.push('/');
    }
    normalized
}

impl TextureRulesFile {
    pub fn load_from_path(rules_path: &Path) -> (Self, Option<SystemTime>) {
        if !rules_path.is_file() {
            return (Self::default(), None);
        }
        let mtime = fs::metadata(rules_path).ok().and_then(|m| m.modified().ok());
        match fs::read_to_string(rules_path) {
            Ok(contents) => match serde_json::from_str::<Self>(&contents) {
                Ok(rules) => (rules, mtime),
                Err(err) => {
                    log::error!("Failed to parse texture rules {:?}: {}", rules_path, err);
                    (Self::default(), mtime)
                }
            },
            Err(err) => {
                log::error!("Failed to read texture rules {:?}: {}", rules_path, err);
                (Self::default(), mtime)
            }
        }
    }

    pub fn resolve_rule(&self, resource_name: &str) -> ResolvedTextureRule {
        let normalized_name = normalize_resource_key(resource_name);
        let mut resolved = ResolvedTextureRule::default();

        // Longest matching folder prefix rule
        let mut best_prefix_len: usize = 0;
        let mut best_folder_rule: Option<&TextureRuleEntry> = None;
        for (folder_key, entry) in &self._folder_rules {
            let prefix = normalize_folder_prefix(folder_key);
            if !prefix.is_empty() && normalized_name.starts_with(&prefix) && prefix.len() > best_prefix_len {
                best_prefix_len = prefix.len();
                best_folder_rule = Some(entry);
            }
        }
        if let Some(folder_rule) = best_folder_rule {
            if let Some(format) = folder_rule._format {
                resolved._format = format;
            }
            if let Some(mipmap) = folder_rule._mipmap {
                resolved._mipmap = mipmap;
            }
            if let Some(srgb) = folder_rule._srgb {
                resolved._srgb = srgb;
            }
        }

        // Exact texture override
        let exact_override = self._overrides.get(&normalized_name).or_else(|| {
            self._overrides.iter().find(|(k, _)| normalize_resource_key(k) == normalized_name).map(|(_, v)| v)
        });
        if let Some(override_rule) = exact_override {
            if let Some(format) = override_rule._format {
                resolved._format = format;
            }
            if let Some(mipmap) = override_rule._mipmap {
                resolved._mipmap = mipmap;
            }
            if let Some(srgb) = override_rule._srgb {
                resolved._srgb = srgb;
            }
        }

        resolved
    }
}

pub fn calc_2d_mip_levels(width: u32, height: u32) -> u32 {
    let max_dim = width.max(height).max(1);
    (u32::BITS - max_dim.leading_zeros()).max(1)
}

pub fn resolve_effective_format(
    rule_format: TextureRuleFormat,
    width: u32,
    height: u32,
    rgba8_pixels: &[u8],
) -> TextureRuleFormat {
    let supports_bcn_size = width >= 4 && height >= 4 && width % 4 == 0 && height % 4 == 0;
    if !supports_bcn_size {
        return TextureRuleFormat::RGBA8;
    }

    match rule_format {
        TextureRuleFormat::Auto => {
            let mut all_opaque = true;
            let mut all_zero = true;
            for pixel in rgba8_pixels.chunks_exact(4) {
                let alpha = pixel[3];
                if alpha != 255 {
                    all_opaque = false;
                }
                if alpha != 0 {
                    all_zero = false;
                }
                if !all_opaque && !all_zero {
                    return TextureRuleFormat::BC3;
                }
            }
            TextureRuleFormat::BC1
        }
        explicit => explicit,
    }
}

fn to_encode_content_and_dxgi(format: TextureRuleFormat, srgb: bool) -> (DecodeContent, DxgiFormat) {
    match format {
        TextureRuleFormat::BC1 => (
            DecodeContent::Bc1,
            if srgb {
                DxgiFormat::BC1_UNorm_sRGB
            } else {
                DxgiFormat::BC1_UNorm
            },
        ),
        TextureRuleFormat::BC2 => (
            DecodeContent::Bc2,
            if srgb {
                DxgiFormat::BC2_UNorm_sRGB
            } else {
                DxgiFormat::BC2_UNorm
            },
        ),
        TextureRuleFormat::BC3 => (
            DecodeContent::Bc3,
            if srgb {
                DxgiFormat::BC3_UNorm_sRGB
            } else {
                DxgiFormat::BC3_UNorm
            },
        ),
        TextureRuleFormat::BC4 => (DecodeContent::Bc4UNorm, DxgiFormat::BC4_UNorm),
        TextureRuleFormat::BC5 => (DecodeContent::Bc5UNorm, DxgiFormat::BC5_UNorm),
        TextureRuleFormat::BC7 => (
            DecodeContent::Bc7,
            if srgb {
                DxgiFormat::BC7_UNorm_sRGB
            } else {
                DxgiFormat::BC7_UNorm
            },
        ),
        TextureRuleFormat::RGBA8 | TextureRuleFormat::Auto => (
            DecodeContent::Rgba8,
            if srgb {
                DxgiFormat::R8G8B8A8_UNorm_sRGB
            } else {
                DxgiFormat::R8G8B8A8_UNorm
            },
        ),
    }
}

pub fn dxgi_format_to_vk(format: DxgiFormat) -> Option<vk::Format> {
    match format {
        DxgiFormat::BC1_UNorm | DxgiFormat::BC1_Typeless => Some(vk::Format::BC1_RGB_UNORM_BLOCK),
        DxgiFormat::BC1_UNorm_sRGB => Some(vk::Format::BC1_RGB_SRGB_BLOCK),
        DxgiFormat::BC2_UNorm | DxgiFormat::BC2_Typeless => Some(vk::Format::BC2_UNORM_BLOCK),
        DxgiFormat::BC2_UNorm_sRGB => Some(vk::Format::BC2_SRGB_BLOCK),
        DxgiFormat::BC3_UNorm | DxgiFormat::BC3_Typeless => Some(vk::Format::BC3_UNORM_BLOCK),
        DxgiFormat::BC3_UNorm_sRGB => Some(vk::Format::BC3_SRGB_BLOCK),
        DxgiFormat::BC4_UNorm | DxgiFormat::BC4_Typeless => Some(vk::Format::BC4_UNORM_BLOCK),
        DxgiFormat::BC4_SNorm => Some(vk::Format::BC4_SNORM_BLOCK),
        DxgiFormat::BC5_UNorm | DxgiFormat::BC5_Typeless => Some(vk::Format::BC5_UNORM_BLOCK),
        DxgiFormat::BC5_SNorm => Some(vk::Format::BC5_SNORM_BLOCK),
        DxgiFormat::BC6H_UF16 | DxgiFormat::BC6H_Typeless => Some(vk::Format::BC6H_UFLOAT_BLOCK),
        DxgiFormat::BC6H_SF16 => Some(vk::Format::BC6H_SFLOAT_BLOCK),
        DxgiFormat::BC7_UNorm | DxgiFormat::BC7_Typeless => Some(vk::Format::BC7_UNORM_BLOCK),
        DxgiFormat::BC7_UNorm_sRGB => Some(vk::Format::BC7_SRGB_BLOCK),
        DxgiFormat::R8G8B8A8_UNorm | DxgiFormat::R8G8B8A8_Typeless => Some(vk::Format::R8G8B8A8_UNORM),
        DxgiFormat::R8G8B8A8_UNorm_sRGB => Some(vk::Format::R8G8B8A8_SRGB),
        DxgiFormat::B8G8R8A8_UNorm | DxgiFormat::B8G8R8A8_Typeless => Some(vk::Format::B8G8R8A8_UNORM),
        DxgiFormat::B8G8R8A8_UNorm_sRGB => Some(vk::Format::B8G8R8A8_SRGB),
        DxgiFormat::R16G16B16A16_UNorm => Some(vk::Format::R16G16B16A16_UNORM),
        DxgiFormat::R16G16B16A16_Float => Some(vk::Format::R16G16B16A16_SFLOAT),
        DxgiFormat::R32G32B32A32_Float => Some(vk::Format::R32G32B32A32_SFLOAT),
        _ => None,
    }
}

fn d3d_format_to_vk(format: D3DFormat) -> Option<vk::Format> {
    match format {
        D3DFormat::DXT1 => Some(vk::Format::BC1_RGB_UNORM_BLOCK),
        D3DFormat::DXT2 | D3DFormat::DXT3 => Some(vk::Format::BC2_UNORM_BLOCK),
        D3DFormat::DXT4 | D3DFormat::DXT5 => Some(vk::Format::BC3_UNORM_BLOCK),
        D3DFormat::A8B8G8R8 => Some(vk::Format::R8G8B8A8_UNORM),
        D3DFormat::A8R8G8B8 => Some(vk::Format::B8G8R8A8_UNORM),
        _ => None,
    }
}

pub fn encode_rgba8_to_dds_bytes(
    width: u32,
    height: u32,
    rgba8_pixels: &[u8],
    rule: ResolvedTextureRule,
) -> Result<(Vec<u8>, TextureRuleFormat, u32), String> {
    let effective_format = resolve_effective_format(rule._format, width, height, rgba8_pixels);
    let mip_levels = if rule._mipmap {
        calc_2d_mip_levels(width, height)
    } else {
        1
    };
    let (decode_content, target_dxgi) = to_encode_content_and_dxgi(effective_format, rule._srgb);
    let layout = EncodeLayout::flat_2d(decode_content, width, height).with_mips(mip_levels);
    let mut dds = Dds::encode_from_rgba8(rgba8_pixels, layout).map_err(|e| {
        format!(
            "Failed to encode DDS ({:?}, {}x{}, mips={}): {:?}",
            effective_format, width, height, mip_levels, e
        )
    })?;

    if let Some(ref mut header10) = dds.header10 {
        header10.dxgi_format = target_dxgi;
    }

    let mut out_bytes = Vec::with_capacity(148 + dds.data.len());
    dds.write(&mut out_bytes).map_err(|e| format!("Failed to serialize DDS bytes: {:?}", e))?;
    Ok((out_bytes, effective_format, mip_levels))
}

#[derive(Debug, Clone, Copy)]
struct DdsHeaderPeek {
    _width: u32,
    _height: u32,
    _mip_levels: u32,
    _dxgi_format: Option<DxgiFormat>,
}

fn peek_dds_header(dds_path: &Path) -> Option<DdsHeaderPeek> {
    let mut file = File::open(dds_path).ok()?;
    let mut buf = [0u8; 148];
    let n = file.read(&mut buf).ok()?;
    if n < 128 || &buf[0..4] != b"DDS " {
        return None;
    }
    let height = u32::from_le_bytes([buf[12], buf[13], buf[14], buf[15]]);
    let width = u32::from_le_bytes([buf[16], buf[17], buf[18], buf[19]]);
    let raw_mips = u32::from_le_bytes([buf[28], buf[29], buf[30], buf[31]]);
    let mip_levels = raw_mips.max(1);
    let dxgi_format = if n >= 148 && &buf[84..88] == b"DX10" {
        let raw_dxgi = u32::from_le_bytes([buf[128], buf[129], buf[130], buf[131]]);
        DxgiFormat::try_from(raw_dxgi).ok()
    } else {
        None
    };
    Some(DdsHeaderPeek {
        _width: width,
        _height: height,
        _mip_levels: mip_levels,
        _dxgi_format: dxgi_format,
    })
}

fn dds_header_matches_rule(peek: DdsHeaderPeek, rule: ResolvedTextureRule) -> bool {
    let expected_mips = if rule._mipmap {
        calc_2d_mip_levels(peek._width, peek._height)
    } else {
        1
    };
    if peek._mip_levels != expected_mips {
        return false;
    }
    let Some(existing_dxgi) = peek._dxgi_format else {
        return false;
    };

    let supports_bcn_size = peek._width >= 4 && peek._height >= 4 && peek._width % 4 == 0 && peek._height % 4 == 0;
    if !supports_bcn_size {
        let (_, expected_dxgi) = to_encode_content_and_dxgi(TextureRuleFormat::RGBA8, rule._srgb);
        return existing_dxgi == expected_dxgi;
    }

    match rule._format {
        TextureRuleFormat::Auto => {
            let (_, bc1_dxgi) = to_encode_content_and_dxgi(TextureRuleFormat::BC1, rule._srgb);
            let (_, bc3_dxgi) = to_encode_content_and_dxgi(TextureRuleFormat::BC3, rule._srgb);
            existing_dxgi == bc1_dxgi || existing_dxgi == bc3_dxgi
        }
        explicit => {
            let (_, expected_dxgi) = to_encode_content_and_dxgi(explicit, rule._srgb);
            existing_dxgi == expected_dxgi
        }
    }
}

fn needs_recook(src_path: &Path, dds_path: &Path, rules_mtime: Option<SystemTime>, rule: ResolvedTextureRule) -> bool {
    let Ok(dds_meta) = fs::metadata(dds_path) else {
        return true;
    };
    let Ok(dds_mtime) = dds_meta.modified() else {
        return true;
    };
    let Ok(src_mtime) = fs::metadata(src_path).and_then(|m| m.modified()) else {
        return true;
    };
    if src_mtime > dds_mtime {
        return true;
    }
    if let Some(r_mtime) = rules_mtime {
        if r_mtime > dds_mtime {
            return match peek_dds_header(dds_path) {
                Some(peek) => !dds_header_matches_rule(peek, rule),
                None => true,
            };
        }
    }
    false
}

fn collect_files_recursive(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_files_recursive(&path, out);
        } else if path.is_file() {
            out.push(path);
        }
    }
}

struct TextureCookJob {
    _resource_name: String,
    _src_path: PathBuf,
    _dds_path: PathBuf,
    _rule: ResolvedTextureRule,
}

pub fn cook_textures_in_directory(
    source_root: &Path,
    output_root: &Path,
    image_extensions: &[&str],
    excluded_source_files: &HashSet<PathBuf>,
) -> CookSummary {
    let mut summary = CookSummary::default();
    if !source_root.is_dir() {
        return summary;
    }

    let rules_path = source_root.join(TEXTURE_RULES_FILE_NAME);
    let (rules, rules_mtime) = TextureRulesFile::load_from_path(&rules_path);

    let mut all_source_files = Vec::new();
    collect_files_recursive(source_root, &mut all_source_files);
    all_source_files.sort();

    let mut valid_dds_paths: HashSet<PathBuf> = HashSet::new();
    let mut cook_jobs: Vec<TextureCookJob> = Vec::new();

    for src_path in all_source_files {
        if excluded_source_files.contains(&src_path) {
            continue;
        }
        let Some(ext) = src_path.extension().and_then(|e| e.to_str()) else {
            continue;
        };
        let ext_lower = ext.to_ascii_lowercase();
        if !image_extensions.contains(&ext_lower.as_str()) {
            continue;
        }
        let Ok(rel_path) = src_path.strip_prefix(source_root) else {
            continue;
        };
        let rel_no_ext = rel_path.with_extension("");
        let resource_name = normalize_resource_key(&rel_no_ext.to_string_lossy());
        let dds_path = output_root.join(&rel_no_ext).with_extension(EXT_TEXTURE_DDS);
        valid_dds_paths.insert(dds_path.clone());

        let rule = rules.resolve_rule(&resource_name);
        if needs_recook(&src_path, &dds_path, rules_mtime, rule) {
            cook_jobs.push(TextureCookJob {
                _resource_name: resource_name,
                _src_path: src_path,
                _dds_path: dds_path,
                _rule: rule,
            });
        } else {
            summary._skipped_count += 1;
        }
    }

    // Remove orphan .dds files whose source image no longer exists
    if output_root.is_dir() {
        let mut existing_outputs = Vec::new();
        collect_files_recursive(output_root, &mut existing_outputs);
        for out_path in existing_outputs {
            let is_dds = out_path
                .extension()
                .and_then(|e| e.to_str())
                .map(|e| e.eq_ignore_ascii_case(EXT_TEXTURE_DDS))
                .unwrap_or(false);
            if is_dds && !valid_dds_paths.contains(&out_path) {
                if let Err(err) = fs::remove_file(&out_path) {
                    log::warn!("Failed to remove orphan DDS {:?}: {}", out_path, err);
                } else {
                    log::info!("[TextureCooker] Removed orphan DDS: {:?}", out_path);
                    let meta_path = out_path.with_extension("meta");
                    if meta_path.is_file() {
                        let _ = fs::remove_file(&meta_path);
                    }
                    summary._removed_count += 1;
                }
            }
        }
    }

    if !cook_jobs.is_empty() {
        log::info!(
            "[TextureCooker] Cooking {} texture(s) from {:?} -> {:?} ({} up-to-date)",
            cook_jobs.len(),
            source_root,
            output_root,
            summary._skipped_count
        );

        let cooked_counter = AtomicUsize::new(0);
        let failed_counter = AtomicUsize::new(0);

        cook_jobs.par_iter().for_each(|job| match cook_single_texture(job) {
            Ok((format, mips, byte_len)) => {
                cooked_counter.fetch_add(1, Ordering::Relaxed);
                log::info!(
                    "[TextureCooker] Exported '{}' -> {:?} ({:?}, srgb={}, mips={}, {} bytes)",
                    job._resource_name,
                    job._dds_path,
                    format,
                    job._rule._srgb,
                    mips,
                    byte_len
                );
            }
            Err(err) => {
                failed_counter.fetch_add(1, Ordering::Relaxed);
                log::error!(
                    "[TextureCooker] Failed to cook '{}' ({:?}): {}",
                    job._resource_name,
                    job._src_path,
                    err
                );
            }
        });

        summary._cooked_count = cooked_counter.load(Ordering::Relaxed);
        summary._failed_count = failed_counter.load(Ordering::Relaxed);
    }

    summary
}

fn cook_single_texture(job: &TextureCookJob) -> Result<(TextureRuleFormat, u32, usize), String> {
    let dynamic_image =
        image::open(&job._src_path).map_err(|e| format!("Failed to open image {:?}: {}", job._src_path, e))?;
    let rgba_image = dynamic_image.to_rgba8();
    let (width, height) = rgba_image.dimensions();
    let (dds_bytes, effective_format, mip_levels) =
        encode_rgba8_to_dds_bytes(width, height, rgba_image.as_raw(), job._rule)?;

    if let Some(parent) = job._dds_path.parent() {
        if !parent.is_dir() {
            fs::create_dir_all(parent).map_err(|e| format!("Failed to create directory {:?}: {}", parent, e))?;
        }
    }

    let tmp_path = job._dds_path.with_extension("dds.tmp");
    fs::write(&tmp_path, &dds_bytes).map_err(|e| format!("Failed to write temp DDS {:?}: {}", tmp_path, e))?;
    fs::rename(&tmp_path, &job._dds_path)
        .map_err(|e| format!("Failed to rename {:?} -> {:?}: {}", tmp_path, job._dds_path, e))?;

    Ok((effective_format, mip_levels, dds_bytes.len()))
}

pub fn load_dds_texture(file_path: &Path) -> Result<LoadedDdsTexture, String> {
    let bytes = system::load(file_path).into_inner();
    parse_dds_bytes(&bytes, file_path)
}

pub fn parse_dds_bytes(bytes: &[u8], file_path: &Path) -> Result<LoadedDdsTexture, String> {
    let dds = DdsView::parse(bytes).map_err(|e| format!("Failed to parse DDS {:?}: {:?}", file_path, e))?;

    let vk_format = dds
        .get_dxgi_format()
        .and_then(dxgi_format_to_vk)
        .or_else(|| dds.get_d3d_format().and_then(d3d_format_to_vk))
        .ok_or_else(|| {
            format!(
                "Unsupported DDS format in {:?}: dxgi={:?}, d3d={:?}",
                file_path,
                dds.get_dxgi_format(),
                dds.get_d3d_format()
            )
        })?;

    let width = dds.get_width();
    let height = dds.get_height();
    let mip_levels = dds.get_num_mipmap_levels().max(1);
    let array_layers = dds.get_num_array_layers().max(1);
    let view_type = if array_layers > 1 {
        vk::ImageViewType::TYPE_2D_ARRAY
    } else {
        vk::ImageViewType::TYPE_2D
    };

    Ok(LoadedDdsTexture {
        _width: width,
        _height: height,
        _layers: array_layers,
        _mip_levels: mip_levels,
        _format: vk_format,
        _view_type: view_type,
        _data: dds.data.to_vec(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alpha_auto_detection_rules() {
        // All opaque (alpha = 255) -> BC1
        let opaque = vec![128, 64, 32, 255].repeat(16);
        assert_eq!(
            resolve_effective_format(TextureRuleFormat::Auto, 4, 4, &opaque),
            TextureRuleFormat::BC1
        );

        // All zero alpha (alpha = 0) -> BC1
        let zero_alpha = vec![128, 64, 32, 0].repeat(16);
        assert_eq!(
            resolve_effective_format(TextureRuleFormat::Auto, 4, 4, &zero_alpha),
            TextureRuleFormat::BC1
        );

        // Mixed alpha -> BC3
        let mut mixed = vec![128, 64, 32, 255].repeat(16);
        mixed[3] = 128;
        assert_eq!(
            resolve_effective_format(TextureRuleFormat::Auto, 4, 4, &mixed),
            TextureRuleFormat::BC3
        );

        // Non-multiple-of-4 dimensions -> RGBA8 fallback
        let small = vec![255, 255, 255, 255].repeat(4);
        assert_eq!(
            resolve_effective_format(TextureRuleFormat::Auto, 2, 2, &small),
            TextureRuleFormat::RGBA8
        );
        assert_eq!(
            resolve_effective_format(TextureRuleFormat::BC5, 2, 2, &small),
            TextureRuleFormat::RGBA8
        );
    }

    #[test]
    fn test_rules_resolution_priority_and_defaults() {
        let json = r#"{
            "version": 1,
            "folder_rules": {
                "ui/": { "format": "Auto", "mipmap": false },
                "ui/icons/": { "format": "BC7", "mipmap": false, "srgb": true }
            },
            "overrides": {
                "ui/icons/special": { "format": "RGBA8" },
                "common/default_n": { "format": "BC5" }
            }
        }"#;
        let rules: TextureRulesFile = serde_json::from_str(json).expect("valid rules json");

        // Unlisted texture defaults
        assert_eq!(
            rules.resolve_rule("characters/hero_d"),
            ResolvedTextureRule {
                _format: TextureRuleFormat::Auto,
                _mipmap: true,
                _srgb: false,
            }
        );

        // Folder rule match
        assert_eq!(
            rules.resolve_rule("ui/button"),
            ResolvedTextureRule {
                _format: TextureRuleFormat::Auto,
                _mipmap: false,
                _srgb: false,
            }
        );

        // Longest folder prefix match
        assert_eq!(
            rules.resolve_rule("ui/icons/wood"),
            ResolvedTextureRule {
                _format: TextureRuleFormat::BC7,
                _mipmap: false,
                _srgb: true,
            }
        );

        // Override merges on top of folder rule
        assert_eq!(
            rules.resolve_rule("ui/icons/special"),
            ResolvedTextureRule {
                _format: TextureRuleFormat::RGBA8,
                _mipmap: false,
                _srgb: true,
            }
        );

        // Normal override keeps default mipmap=true, srgb=false
        assert_eq!(
            rules.resolve_rule("common/default_n"),
            ResolvedTextureRule {
                _format: TextureRuleFormat::BC5,
                _mipmap: true,
                _srgb: false,
            }
        );
    }

    #[test]
    fn test_dds_encode_and_parse_roundtrip() {
        let pixels = vec![128, 128, 255, 255].repeat(8 * 8);
        let cases = [
            (TextureRuleFormat::BC1, false, true, vk::Format::BC1_RGB_UNORM_BLOCK, 4),
            (TextureRuleFormat::BC1, true, false, vk::Format::BC1_RGB_SRGB_BLOCK, 1),
            (TextureRuleFormat::BC3, false, true, vk::Format::BC3_UNORM_BLOCK, 4),
            (TextureRuleFormat::BC5, false, true, vk::Format::BC5_UNORM_BLOCK, 4),
            (TextureRuleFormat::BC7, true, true, vk::Format::BC7_SRGB_BLOCK, 4),
            (TextureRuleFormat::RGBA8, false, false, vk::Format::R8G8B8A8_UNORM, 1),
        ];
        for (format, srgb, mipmap, expected_vk, expected_mips) in cases {
            let rule = ResolvedTextureRule {
                _format: format,
                _mipmap: mipmap,
                _srgb: srgb,
            };
            let (bytes, eff, mips) = encode_rgba8_to_dds_bytes(8, 8, &pixels, rule).expect("encode");
            assert_eq!(eff, format);
            assert_eq!(mips, expected_mips);
            let parsed = parse_dds_bytes(&bytes, Path::new("test.dds")).expect("parse");
            assert_eq!(parsed._width, 8);
            assert_eq!(parsed._height, 8);
            assert_eq!(parsed._mip_levels, expected_mips);
            assert_eq!(parsed._format, expected_vk);
            assert!(!parsed._data.is_empty());
        }
    }

    #[test]
    fn test_cook_directory_incremental_and_rule_change() {
        let unique =
            std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map(|d| d.as_nanos()).unwrap_or(0);
        let temp_root = std::env::temp_dir().join(format!("re3d_cook_test_{}", unique));
        let src_dir = temp_root.join("src");
        let out_dir = temp_root.join("out");
        fs::create_dir_all(&src_dir).expect("create src_dir");

        // Write two 8x8 PNGs: opaque (auto BC1) and semi-transparent (auto BC3)
        let img_opaque = image::RgbaImage::from_pixel(8, 8, image::Rgba([200, 100, 50, 255]));
        img_opaque.save(src_dir.join("opaque.png")).expect("save opaque");

        let mut img_alpha = image::RgbaImage::from_pixel(8, 8, image::Rgba([200, 100, 50, 255]));
        img_alpha.put_pixel(0, 0, image::Rgba([200, 100, 50, 128]));
        img_alpha.save(src_dir.join("alpha.png")).expect("save alpha");

        // 1. First cook -> cooks both
        let excluded = HashSet::new();
        let s1 = cook_textures_in_directory(&src_dir, &out_dir, &["png"], &excluded);
        assert_eq!(s1._cooked_count, 2);
        assert_eq!(s1._failed_count, 0);
        assert_eq!(
            parse_dds_bytes(&fs::read(out_dir.join("opaque.dds")).unwrap(), Path::new("opaque.dds")).unwrap()._format,
            vk::Format::BC1_RGB_UNORM_BLOCK
        );
        assert_eq!(
            parse_dds_bytes(&fs::read(out_dir.join("alpha.dds")).unwrap(), Path::new("alpha.dds")).unwrap()._format,
            vk::Format::BC3_UNORM_BLOCK
        );

        // 2. Second cook with no changes -> skips both
        let s2 = cook_textures_in_directory(&src_dir, &out_dir, &["png"], &excluded);
        assert_eq!(s2._cooked_count, 0);
        assert_eq!(s2._skipped_count, 2);

        // 3. Add texture_rules.json overriding ONLY opaque -> BC7 + srgb=true (ensure mtime > dds)
        std::thread::sleep(std::time::Duration::from_millis(20));
        let rules_json = r#"{
            "version": 1,
            "folder_rules": {},
            "overrides": {
                "opaque": { "format": "BC7", "mipmap": false, "srgb": true }
            }
        }"#;
        fs::write(src_dir.join(TEXTURE_RULES_FILE_NAME), rules_json).expect("write rules");
        let s3 = cook_textures_in_directory(&src_dir, &out_dir, &["png"], &excluded);
        assert_eq!(s3._cooked_count, 1);
        assert_eq!(s3._skipped_count, 1);
        let parsed_opaque =
            parse_dds_bytes(&fs::read(out_dir.join("opaque.dds")).unwrap(), Path::new("opaque.dds")).unwrap();
        assert_eq!(parsed_opaque._format, vk::Format::BC7_SRGB_BLOCK);
        assert_eq!(parsed_opaque._mip_levels, 1);

        // 4. Delete source -> removes orphan DDS
        fs::remove_file(src_dir.join("alpha.png")).expect("remove alpha.png");
        let s4 = cook_textures_in_directory(&src_dir, &out_dir, &["png"], &excluded);
        assert_eq!(s4._removed_count, 1);
        assert!(!out_dir.join("alpha.dds").exists());

        let _ = fs::remove_dir_all(&temp_root);
    }
}
