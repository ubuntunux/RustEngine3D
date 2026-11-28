use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use ash::ext;
use ash::vk::Handle;
use ash::{Device, vk};

use crate::vulkan_context::debug_utils;

const PIPELINE_CACHE_MAGIC: [u8; 8] = *b"RE3DPPC1";
const PIPELINE_CACHE_FORMAT_VERSION: u32 = 1;
const PIPELINE_CACHE_FILE_HEADER_SIZE: usize = 64;
const VK_PIPELINE_CACHE_HEADER_MIN_SIZE: usize = 32;
const FNV1A_64_OFFSET_BASIS: u64 = 0xcbf29ce484222325;
const FNV1A_64_PRIME: u64 = 0x100000001b3;
const PIPELINE_CACHE_SUBDIR_NAME: &str = "pipeline_cache";
const PIPELINE_CACHE_FILE_EXTENSION: &str = "bin";
const DEFAULT_PIPELINE_CACHE_APP_DIR: &str = "RustEngine3D";
const PIPELINE_CACHE_DEBUG_OBJECT_NAME: &str = "EnginePipelineCache";

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PipelineCacheIdentity {
    pub _vendor_id: u32,
    pub _device_id: u32,
    pub _driver_version: u32,
    pub _api_version: u32,
    pub _pointer_size: u32,
    pub _pipeline_cache_uuid: [u8; vk::UUID_SIZE],
}

impl PipelineCacheIdentity {
    pub fn from_device_properties(device_properties: &vk::PhysicalDeviceProperties) -> Self {
        Self {
            _vendor_id: device_properties.vendor_id,
            _device_id: device_properties.device_id,
            _driver_version: device_properties.driver_version,
            _api_version: device_properties.api_version,
            _pointer_size: std::mem::size_of::<usize>() as u32,
            _pipeline_cache_uuid: device_properties.pipeline_cache_uuid,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PipelineCacheData {
    pub _pipeline_cache: vk::PipelineCache,
    pub _cache_file_path: Option<PathBuf>,
    pub _identity: PipelineCacheIdentity,
    pub _saved_data_size: usize,
    pub _saved_data_hash: u64,
}

impl Default for PipelineCacheData {
    fn default() -> Self {
        Self {
            _pipeline_cache: vk::PipelineCache::null(),
            _cache_file_path: None,
            _identity: PipelineCacheIdentity::default(),
            _saved_data_size: 0,
            _saved_data_hash: 0,
        }
    }
}

impl PipelineCacheData {
    pub fn get_pipeline_cache(&self) -> vk::PipelineCache {
        self._pipeline_cache
    }
}

fn compute_fnv1a_64(bytes: &[u8]) -> u64 {
    let mut hash = FNV1A_64_OFFSET_BASIS;
    for &byte in bytes {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(FNV1A_64_PRIME);
    }
    hash
}

fn sanitize_app_name(app_name: &str) -> String {
    let sanitized: String = app_name.chars().filter(|c| c.is_ascii_alphanumeric() || *c == '-' || *c == '_').collect();
    if sanitized.is_empty() {
        DEFAULT_PIPELINE_CACHE_APP_DIR.to_string()
    } else {
        sanitized
    }
}

#[cfg(target_os = "android")]
fn resolve_user_cache_root() -> Option<PathBuf> {
    None
}

#[cfg(target_os = "windows")]
fn resolve_user_cache_root() -> Option<PathBuf> {
    std::env::var_os("LOCALAPPDATA")
        .filter(|v| !v.is_empty())
        .or_else(|| std::env::var_os("APPDATA").filter(|v| !v.is_empty()))
        .map(PathBuf::from)
        .or_else(|| Some(std::env::temp_dir()))
}

#[cfg(target_os = "macos")]
fn resolve_user_cache_root() -> Option<PathBuf> {
    std::env::var_os("HOME")
        .filter(|v| !v.is_empty())
        .map(|home| PathBuf::from(home).join("Library").join("Caches"))
        .or_else(|| Some(std::env::temp_dir()))
}

#[cfg(not(any(target_os = "android", target_os = "windows", target_os = "macos")))]
fn resolve_user_cache_root() -> Option<PathBuf> {
    std::env::var_os("XDG_CACHE_HOME")
        .filter(|v| !v.is_empty())
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("HOME").filter(|v| !v.is_empty()).map(|home| PathBuf::from(home).join(".cache")))
        .or_else(|| Some(std::env::temp_dir()))
}

pub fn resolve_pipeline_cache_path(app_name: &str, identity: &PipelineCacheIdentity) -> Option<PathBuf> {
    let cache_root = resolve_user_cache_root()?;
    let app_dir = sanitize_app_name(app_name);
    let file_name = format!(
        "pipeline_cache_{:04x}_{:04x}.{}",
        identity._vendor_id, identity._device_id, PIPELINE_CACHE_FILE_EXTENSION
    );
    Some(cache_root.join(app_dir).join(PIPELINE_CACHE_SUBDIR_NAME).join(file_name))
}

pub fn encode_pipeline_cache_file(identity: &PipelineCacheIdentity, vk_data: &[u8]) -> Vec<u8> {
    let data_hash = compute_fnv1a_64(vk_data);
    let mut output = Vec::with_capacity(PIPELINE_CACHE_FILE_HEADER_SIZE + vk_data.len());
    output.extend_from_slice(&PIPELINE_CACHE_MAGIC);
    output.extend_from_slice(&PIPELINE_CACHE_FORMAT_VERSION.to_le_bytes());
    output.extend_from_slice(&identity._vendor_id.to_le_bytes());
    output.extend_from_slice(&identity._device_id.to_le_bytes());
    output.extend_from_slice(&identity._driver_version.to_le_bytes());
    output.extend_from_slice(&identity._api_version.to_le_bytes());
    output.extend_from_slice(&identity._pointer_size.to_le_bytes());
    output.extend_from_slice(&identity._pipeline_cache_uuid);
    output.extend_from_slice(&(vk_data.len() as u64).to_le_bytes());
    output.extend_from_slice(&data_hash.to_le_bytes());
    debug_assert_eq!(output.len(), PIPELINE_CACHE_FILE_HEADER_SIZE);
    output.extend_from_slice(vk_data);
    output
}

fn read_le_u32(bytes: &[u8], offset: usize) -> Result<u32, String> {
    let slice =
        bytes.get(offset..offset + 4).ok_or_else(|| format!("Unexpected EOF reading u32 at offset {}", offset))?;
    let mut arr = [0u8; 4];
    arr.copy_from_slice(slice);
    Ok(u32::from_le_bytes(arr))
}

fn read_le_u64(bytes: &[u8], offset: usize) -> Result<u64, String> {
    let slice =
        bytes.get(offset..offset + 8).ok_or_else(|| format!("Unexpected EOF reading u64 at offset {}", offset))?;
    let mut arr = [0u8; 8];
    arr.copy_from_slice(slice);
    Ok(u64::from_le_bytes(arr))
}

fn read_ne_u32(bytes: &[u8], offset: usize) -> Result<u32, String> {
    let slice = bytes
        .get(offset..offset + 4)
        .ok_or_else(|| format!("Unexpected EOF reading native u32 at offset {}", offset))?;
    let mut arr = [0u8; 4];
    arr.copy_from_slice(slice);
    Ok(u32::from_ne_bytes(arr))
}

fn validate_vk_pipeline_cache_header(payload: &[u8], expected_identity: &PipelineCacheIdentity) -> Result<(), String> {
    if payload.len() < VK_PIPELINE_CACHE_HEADER_MIN_SIZE {
        return Err(format!(
            "Vulkan pipeline cache payload too small: {} bytes",
            payload.len()
        ));
    }

    let header_size = read_ne_u32(payload, 0)?;
    let header_version = read_ne_u32(payload, 4)?;
    let vendor_id = read_ne_u32(payload, 8)?;
    let device_id = read_ne_u32(payload, 12)?;
    let uuid_slice = &payload[16..16 + vk::UUID_SIZE];

    if (header_size as usize) < VK_PIPELINE_CACHE_HEADER_MIN_SIZE || (header_size as usize) > payload.len() {
        return Err(format!("Invalid Vulkan pipeline cache header size: {}", header_size));
    }
    if header_version != vk::PipelineCacheHeaderVersion::ONE.as_raw() as u32 {
        return Err(format!(
            "Unsupported Vulkan pipeline cache header version: {}",
            header_version
        ));
    }
    if vendor_id != expected_identity._vendor_id || device_id != expected_identity._device_id {
        return Err(format!(
            "Vulkan pipeline cache device mismatch: file=({:#x}, {:#x}), current=({:#x}, {:#x})",
            vendor_id, device_id, expected_identity._vendor_id, expected_identity._device_id
        ));
    }
    if uuid_slice != expected_identity._pipeline_cache_uuid {
        return Err("Vulkan pipeline cache UUID mismatch".to_string());
    }

    Ok(())
}

pub fn decode_and_validate_pipeline_cache_file(
    file_bytes: &[u8],
    expected_identity: &PipelineCacheIdentity,
) -> Result<Vec<u8>, String> {
    if file_bytes.len() < PIPELINE_CACHE_FILE_HEADER_SIZE {
        return Err(format!("Pipeline cache file too small: {} bytes", file_bytes.len()));
    }

    if &file_bytes[0..8] != &PIPELINE_CACHE_MAGIC {
        return Err("Invalid pipeline cache magic".to_string());
    }

    let format_version = read_le_u32(file_bytes, 8)?;
    if format_version != PIPELINE_CACHE_FORMAT_VERSION {
        return Err(format!(
            "Unsupported pipeline cache format version: {} (expected {})",
            format_version, PIPELINE_CACHE_FORMAT_VERSION
        ));
    }

    let vendor_id = read_le_u32(file_bytes, 12)?;
    let device_id = read_le_u32(file_bytes, 16)?;
    let driver_version = read_le_u32(file_bytes, 20)?;
    let api_version = read_le_u32(file_bytes, 24)?;
    let pointer_size = read_le_u32(file_bytes, 28)?;
    let mut uuid = [0u8; vk::UUID_SIZE];
    uuid.copy_from_slice(&file_bytes[32..32 + vk::UUID_SIZE]);

    let file_identity = PipelineCacheIdentity {
        _vendor_id: vendor_id,
        _device_id: device_id,
        _driver_version: driver_version,
        _api_version: api_version,
        _pointer_size: pointer_size,
        _pipeline_cache_uuid: uuid,
    };

    if file_identity != *expected_identity {
        return Err(format!(
            "Pipeline cache identity mismatch (driver/device/api changed): file_driver={:#x}, current_driver={:#x}",
            file_identity._driver_version, expected_identity._driver_version
        ));
    }

    let data_size = read_le_u64(file_bytes, 48)?;
    let expected_hash = read_le_u64(file_bytes, 56)?;
    let payload = &file_bytes[PIPELINE_CACHE_FILE_HEADER_SIZE..];

    if payload.is_empty() || payload.len() as u64 != data_size {
        return Err(format!(
            "Pipeline cache payload size mismatch: header={}, actual={}",
            data_size,
            payload.len()
        ));
    }

    let actual_hash = compute_fnv1a_64(payload);
    if actual_hash != expected_hash {
        return Err(format!(
            "Pipeline cache checksum mismatch: expected={:#x}, actual={:#x}",
            expected_hash, actual_hash
        ));
    }

    validate_vk_pipeline_cache_header(payload, expected_identity)?;
    Ok(payload.to_vec())
}

pub fn load_pipeline_cache_bytes_from_path(path: &Path, identity: &PipelineCacheIdentity) -> Option<Vec<u8>> {
    if !path.is_file() {
        return None;
    }

    match fs::read(path) {
        Ok(file_bytes) => match decode_and_validate_pipeline_cache_file(&file_bytes, identity) {
            Ok(vk_data) => Some(vk_data),
            Err(reason) => {
                log::warn!("Ignoring stale/invalid pipeline cache {:?}: {}", path, reason);
                None
            }
        },
        Err(err) => {
            log::warn!("Failed to read pipeline cache {:?}: {:?}", path, err);
            None
        }
    }
}

pub fn save_pipeline_cache_bytes_to_path(
    path: &Path,
    identity: &PipelineCacheIdentity,
    vk_data: &[u8],
) -> Result<(), String> {
    if vk_data.is_empty() {
        return Ok(());
    }

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| format!("Failed to create cache dir {:?}: {:?}", parent, e))?;
    }

    let encoded = encode_pipeline_cache_file(identity, vk_data);
    let tmp_file_name = format!(
        "{}.tmp.{}",
        path.file_name().and_then(|n| n.to_str()).unwrap_or("pipeline_cache"),
        std::process::id()
    );
    let tmp_path = path.with_file_name(tmp_file_name);

    {
        let mut tmp_file =
            File::create(&tmp_path).map_err(|e| format!("Failed to create temp cache file {:?}: {:?}", tmp_path, e))?;
        if let Err(err) = tmp_file.write_all(&encoded) {
            let _ = fs::remove_file(&tmp_path);
            return Err(format!("Failed to write temp cache file {:?}: {:?}", tmp_path, err));
        }
        if let Err(err) = tmp_file.flush() {
            let _ = fs::remove_file(&tmp_path);
            return Err(format!("Failed to flush temp cache file {:?}: {:?}", tmp_path, err));
        }
    }

    if let Err(rename_err) = fs::rename(&tmp_path, path) {
        // Fallback for filesystems/Wine where rename over an existing file may fail.
        if path.exists() {
            let _ = fs::remove_file(path);
            if let Err(retry_err) = fs::rename(&tmp_path, path) {
                let _ = fs::remove_file(&tmp_path);
                return Err(format!(
                    "Failed to replace pipeline cache {:?}: {:?} (retry: {:?})",
                    path, rename_err, retry_err
                ));
            }
        } else {
            let _ = fs::remove_file(&tmp_path);
            return Err(format!(
                "Failed to move pipeline cache {:?} -> {:?}: {:?}",
                tmp_path, path, rename_err
            ));
        }
    }

    Ok(())
}

pub fn create_pipeline_cache_data(
    device: &Device,
    debug_utils_device: Option<&ext::debug_utils::Device>,
    app_name: &str,
    device_properties: &vk::PhysicalDeviceProperties,
) -> PipelineCacheData {
    let timer = Instant::now();
    let identity = PipelineCacheIdentity::from_device_properties(device_properties);
    let cache_file_path = resolve_pipeline_cache_path(app_name, &identity);

    let initial_bytes = cache_file_path.as_ref().and_then(|path| load_pipeline_cache_bytes_from_path(path, &identity));

    let (pipeline_cache, saved_data_size, saved_data_hash) = unsafe {
        if let Some(ref bytes) = initial_bytes {
            let create_info = vk::PipelineCacheCreateInfo::default().initial_data(bytes);
            match device.create_pipeline_cache(&create_info, None) {
                Ok(cache) => {
                    let hash = compute_fnv1a_64(bytes);
                    log::info!(
                        "Loaded Vulkan pipeline cache ({:.1} KB) from {:?} in {:.2}ms",
                        bytes.len() as f64 / 1024.0,
                        cache_file_path.as_deref().unwrap_or(Path::new("")),
                        timer.elapsed().as_secs_f64() * 1000.0
                    );
                    (cache, bytes.len(), hash)
                }
                Err(err) => {
                    log::warn!(
                        "vkCreatePipelineCache rejected cached data ({:?}); falling back to empty cache",
                        err
                    );
                    let empty_info = vk::PipelineCacheCreateInfo::default();
                    match device.create_pipeline_cache(&empty_info, None) {
                        Ok(cache) => (cache, 0, 0),
                        Err(fallback_err) => {
                            log::error!("Failed to create fallback empty pipeline cache: {:?}", fallback_err);
                            (vk::PipelineCache::null(), 0, 0)
                        }
                    }
                }
            }
        } else {
            let empty_info = vk::PipelineCacheCreateInfo::default();
            match device.create_pipeline_cache(&empty_info, None) {
                Ok(cache) => {
                    log::info!("Created new Vulkan pipeline cache (path: {:?})", cache_file_path);
                    (cache, 0, 0)
                }
                Err(err) => {
                    log::error!("Failed to create Vulkan pipeline cache: {:?}", err);
                    (vk::PipelineCache::null(), 0, 0)
                }
            }
        }
    };

    if pipeline_cache != vk::PipelineCache::null() {
        if let Some(debug_utils) = debug_utils_device {
            debug_utils::set_object_debug_info(
                debug_utils,
                PIPELINE_CACHE_DEBUG_OBJECT_NAME,
                vk::ObjectType::PIPELINE_CACHE,
                pipeline_cache.as_raw(),
            );
        }
    }

    PipelineCacheData {
        _pipeline_cache: pipeline_cache,
        _cache_file_path: cache_file_path,
        _identity: identity,
        _saved_data_size: saved_data_size,
        _saved_data_hash: saved_data_hash,
    }
}

pub fn save_pipeline_cache_data(device: &Device, cache_data: &mut PipelineCacheData) {
    if cache_data._pipeline_cache == vk::PipelineCache::null() {
        return;
    }
    let Some(ref cache_file_path) = cache_data._cache_file_path else {
        return;
    };

    let timer = Instant::now();
    let vk_data = match unsafe { device.get_pipeline_cache_data(cache_data._pipeline_cache) } {
        Ok(data) => data,
        Err(err) => {
            log::warn!("vkGetPipelineCacheData failed: {:?}", err);
            return;
        }
    };

    if vk_data.is_empty() {
        return;
    }

    let data_hash = compute_fnv1a_64(&vk_data);
    if vk_data.len() == cache_data._saved_data_size && data_hash == cache_data._saved_data_hash {
        log::debug!(
            "Vulkan pipeline cache unchanged ({:.1} KB); skipping save",
            vk_data.len() as f64 / 1024.0
        );
        return;
    }

    if let Err(err) = save_pipeline_cache_bytes_to_path(cache_file_path, &cache_data._identity, &vk_data) {
        log::warn!("Failed to persist Vulkan pipeline cache: {}", err);
        return;
    }

    cache_data._saved_data_size = vk_data.len();
    cache_data._saved_data_hash = data_hash;
    log::info!(
        "Saved Vulkan pipeline cache ({:.1} KB) to {:?} in {:.2}ms",
        vk_data.len() as f64 / 1024.0,
        cache_file_path,
        timer.elapsed().as_secs_f64() * 1000.0
    );
}

pub fn destroy_pipeline_cache_data(device: &Device, cache_data: &mut PipelineCacheData) {
    save_pipeline_cache_data(device, cache_data);
    if cache_data._pipeline_cache != vk::PipelineCache::null() {
        unsafe {
            device.destroy_pipeline_cache(cache_data._pipeline_cache, None);
        }
        cache_data._pipeline_cache = vk::PipelineCache::null();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_test_identity() -> PipelineCacheIdentity {
        PipelineCacheIdentity {
            _vendor_id: 0x10de,
            _device_id: 0x2684,
            _driver_version: 0x8a20_0000,
            _api_version: vk::make_api_version(0, 1, 3, 290),
            _pointer_size: std::mem::size_of::<usize>() as u32,
            _pipeline_cache_uuid: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16],
        }
    }

    fn make_valid_vk_payload(identity: &PipelineCacheIdentity, extra_bytes: &[u8]) -> Vec<u8> {
        let mut payload = Vec::with_capacity(VK_PIPELINE_CACHE_HEADER_MIN_SIZE + extra_bytes.len());
        payload.extend_from_slice(&(VK_PIPELINE_CACHE_HEADER_MIN_SIZE as u32).to_ne_bytes());
        payload.extend_from_slice(&(vk::PipelineCacheHeaderVersion::ONE.as_raw() as u32).to_ne_bytes());
        payload.extend_from_slice(&identity._vendor_id.to_ne_bytes());
        payload.extend_from_slice(&identity._device_id.to_ne_bytes());
        payload.extend_from_slice(&identity._pipeline_cache_uuid);
        payload.extend_from_slice(extra_bytes);
        payload
    }

    #[test]
    fn test_encode_decode_roundtrip() {
        let identity = make_test_identity();
        let vk_payload = make_valid_vk_payload(&identity, b"compiled_shader_binary_blob");
        let encoded = encode_pipeline_cache_file(&identity, &vk_payload);
        let decoded = decode_and_validate_pipeline_cache_file(&encoded, &identity).expect("valid cache should decode");
        assert_eq!(decoded, vk_payload);
    }

    #[test]
    fn test_reject_corrupted_payload_checksum() {
        let identity = make_test_identity();
        let vk_payload = make_valid_vk_payload(&identity, b"compiled_shader_binary_blob");
        let mut encoded = encode_pipeline_cache_file(&identity, &vk_payload);
        let last_idx = encoded.len() - 1;
        encoded[last_idx] ^= 0xff;
        assert!(decode_and_validate_pipeline_cache_file(&encoded, &identity).is_err());
    }

    #[test]
    fn test_reject_driver_or_device_mismatch() {
        let identity = make_test_identity();
        let vk_payload = make_valid_vk_payload(&identity, b"blob");
        let encoded = encode_pipeline_cache_file(&identity, &vk_payload);

        let mut other_driver = identity.clone();
        other_driver._driver_version += 1;
        assert!(decode_and_validate_pipeline_cache_file(&encoded, &other_driver).is_err());

        let mut other_uuid = identity.clone();
        other_uuid._pipeline_cache_uuid[0] ^= 0x01;
        assert!(decode_and_validate_pipeline_cache_file(&encoded, &other_uuid).is_err());
    }

    #[test]
    fn test_reject_invalid_vk_header() {
        let identity = make_test_identity();
        let bad_payload = vec![0u8; 64];
        let encoded = encode_pipeline_cache_file(&identity, &bad_payload);
        assert!(decode_and_validate_pipeline_cache_file(&encoded, &identity).is_err());
    }

    #[test]
    fn test_disk_save_and_load_roundtrip() {
        let identity = make_test_identity();
        let vk_payload = make_valid_vk_payload(&identity, b"pso_cache_data_v1");
        let test_dir = std::env::temp_dir().join(format!("re3d_pipeline_cache_test_{}", std::process::id()));
        let cache_path = test_dir.join("pipeline_cache_10de_2684.bin");

        save_pipeline_cache_bytes_to_path(&cache_path, &identity, &vk_payload).expect("save should succeed");
        let loaded = load_pipeline_cache_bytes_from_path(&cache_path, &identity).expect("load should succeed");
        assert_eq!(loaded, vk_payload);

        // Overwrite with updated cache blob (atomic replace)
        let vk_payload_v2 = make_valid_vk_payload(&identity, b"pso_cache_data_v2_larger_payload");
        save_pipeline_cache_bytes_to_path(&cache_path, &identity, &vk_payload_v2).expect("overwrite should succeed");
        let loaded_v2 = load_pipeline_cache_bytes_from_path(&cache_path, &identity).expect("load v2 should succeed");
        assert_eq!(loaded_v2, vk_payload_v2);

        let _ = fs::remove_dir_all(&test_dir);
    }

    #[test]
    fn test_sanitize_app_name() {
        assert_eq!(sanitize_app_name("Stone Age! / v1"), "StoneAgev1");
        assert_eq!(sanitize_app_name("my_game-01"), "my_game-01");
        assert_eq!(sanitize_app_name("   ///   "), DEFAULT_PIPELINE_CACHE_APP_DIR);
    }
}
