use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};

use ash::vk;
use byteorder::{LittleEndian, ReadBytesExt};
use serde_json::Value;

use crate::constants;
use crate::resource::collada_loader::Collada;
use crate::resource::gltf_loader::GLTF;
use crate::resource::obj_loader::WaveFrontOBJ;
use crate::resource::resource::{
    EngineResources, MetaData, EXT_COLLADA, EXT_GLTF, EXT_GLTF_BINARY, EXT_OBJ, EXT_TEXTURE, EXT_TEXTURE_2D_ARRAY,
    EXT_TEXTURE_3D, EXT_TEXTURE_CUBE, LOAD_FROM_EXTERNAL_FOR_MESH, USE_JSON_FOR_MESH,
};
use crate::scene::mesh::MeshDataCreateInfo;
use crate::utilities::system;

#[derive(Debug, Clone)]
pub enum AsyncLoadRequest {
    Texture {
        _texture_name: String,
        _meta_data: MetaData,
    },
    Mesh {
        _mesh_name: String,
        _meta_data: MetaData,
    },
}

#[derive(Debug, Clone)]
pub struct AsyncTextureData {
    pub _texture_name: String,
    pub _image_width: u32,
    pub _image_height: u32,
    pub _image_layers: u32,
    pub _image_format: vk::Format,
    pub _image_view_type: vk::ImageViewType,
    pub _image_data: Vec<u8>,
    pub _enable_mipmap: bool,
    pub _min_filter: vk::Filter,
    pub _mag_filter: vk::Filter,
    pub _wrap_mode: vk::SamplerAddressMode,
}

#[derive(Debug, Clone)]
pub struct AsyncMeshData {
    pub _mesh_name: String,
    pub _create_info: MeshDataCreateInfo,
}

#[derive(Debug, Clone)]
pub enum AsyncLoadResult {
    Texture {
        _texture_name: String,
        _result: Result<AsyncTextureData, String>,
    },
    Mesh {
        _mesh_name: String,
        _result: Result<AsyncMeshData, String>,
    },
}

impl AsyncTextureData {
    fn load_binary_texture(texture_name: &str, file_path: &Path) -> Result<Self, String> {
        let mut loaded_contents = system::load(file_path);
        let image_view_type = vk::ImageViewType::from_raw(
            loaded_contents
                .read_i32::<LittleEndian>()
                .map_err(|e| format!("Failed to read image_view_type: {:?}", e))?,
        );
        let image_width =
            loaded_contents.read_i32::<LittleEndian>().map_err(|e| format!("Failed to read width: {:?}", e))?;
        let image_height =
            loaded_contents.read_i32::<LittleEndian>().map_err(|e| format!("Failed to read height: {:?}", e))?;
        let image_depth =
            loaded_contents.read_i32::<LittleEndian>().map_err(|e| format!("Failed to read depth: {:?}", e))?;
        let image_format = vk::Format::from_raw(
            loaded_contents.read_i32::<LittleEndian>().map_err(|e| format!("Failed to read format: {:?}", e))?,
        );
        let enable_mipmap = 0
            != loaded_contents
                .read_i32::<LittleEndian>()
                .map_err(|e| format!("Failed to read mipmap flag: {:?}", e))?;
        let min_filter = vk::Filter::from_raw(
            loaded_contents.read_i32::<LittleEndian>().map_err(|e| format!("Failed to read min_filter: {:?}", e))?,
        );
        let mag_filter = vk::Filter::from_raw(
            loaded_contents.read_i32::<LittleEndian>().map_err(|e| format!("Failed to read mag_filter: {:?}", e))?,
        );
        let wrap = vk::SamplerAddressMode::from_raw(
            loaded_contents.read_i32::<LittleEndian>().map_err(|e| format!("Failed to read wrap mode: {:?}", e))?,
        );
        let data_bytes =
            loaded_contents.read_i32::<LittleEndian>().map_err(|e| format!("Failed to read data bytes: {:?}", e))?;
        let mut image_data = Vec::new();
        loaded_contents.read_to_end(&mut image_data).map_err(|e| format!("Failed to read image data: {:?}", e))?;
        if data_bytes as usize != image_data.len() {
            return Err(format!(
                "Binary texture data length mismatch in {:?}: expected {}, got {}",
                file_path,
                data_bytes,
                image_data.len()
            ));
        }

        Ok(AsyncTextureData {
            _texture_name: texture_name.to_string(),
            _image_width: image_width as u32,
            _image_height: image_height as u32,
            _image_layers: image_depth as u32,
            _image_format: image_format,
            _image_view_type: image_view_type,
            _image_data: image_data,
            _enable_mipmap: enable_mipmap,
            _min_filter: min_filter,
            _mag_filter: mag_filter,
            _wrap_mode: wrap,
        })
    }

    fn load_combined_texture(texture_name: &str, file_path: &Path) -> Result<Self, String> {
        let loaded_contents = system::load(file_path);
        let contents: Value = serde_json::from_reader(loaded_contents)
            .map_err(|e| format!("Failed to parse combined texture json {:?}: {:?}", file_path, e))?;
        let ext = file_path.extension().and_then(|e| e.to_str()).unwrap_or("");
        let mut texture_file_names: Vec<String> = Vec::new();
        let image_view_type = if EXT_TEXTURE_CUBE == ext {
            if let Value::Object(texture_cube_faces) = contents {
                for face in constants::CUBE_TEXTURE_FACES.iter() {
                    if let Some(Value::String(face_texture_name)) = texture_cube_faces.get(*face) {
                        texture_file_names.push(face_texture_name.clone());
                    }
                }
            }
            vk::ImageViewType::CUBE
        } else if EXT_TEXTURE_2D_ARRAY == ext || EXT_TEXTURE_3D == ext {
            if let Value::Array(texture_file_list) = contents {
                for texture_file in texture_file_list {
                    if let Value::String(texture_file) = texture_file {
                        texture_file_names.push(texture_file);
                    }
                }
            }

            if EXT_TEXTURE_2D_ARRAY == ext {
                vk::ImageViewType::TYPE_2D_ARRAY
            } else if EXT_TEXTURE_3D == ext {
                vk::ImageViewType::TYPE_3D
            } else {
                return Err(format!("Unsupported combined texture format: {}", ext));
            }
        } else {
            return Err(format!("Unknown combined texture extension: {}", ext));
        };

        let directory = file_path.parent().ok_or_else(|| format!("Invalid parent directory for {:?}", file_path))?;
        let mut texture_files: Vec<PathBuf> = Vec::new();
        for texture_file_name in texture_file_names {
            let mut texture_file = PathBuf::from(directory);
            let file_name = PathBuf::from(texture_file_name)
                .file_name()
                .ok_or_else(|| "Invalid file name in combined texture list".to_string())?
                .to_os_string();
            texture_file.push(file_name);
            texture_files.push(texture_file);
        }

        let (image_width, image_height, image_layers, image_data, image_format) =
            EngineResources::load_image_data_list(&texture_files);
        if vk::Format::UNDEFINED == image_format {
            return Err(format!(
                "Failed to load image data list for combined texture: {:?}",
                file_path
            ));
        }

        Ok(AsyncTextureData {
            _texture_name: texture_name.to_string(),
            _image_width: image_width,
            _image_height: image_height,
            _image_layers: image_layers,
            _image_format: image_format,
            _image_view_type: image_view_type,
            _image_data: image_data,
            _enable_mipmap: true,
            _min_filter: vk::Filter::LINEAR,
            _mag_filter: vk::Filter::LINEAR,
            _wrap_mode: vk::SamplerAddressMode::REPEAT,
        })
    }

    fn load_simple_image_texture(texture_name: &str, file_path: &Path) -> Result<Self, String> {
        let (image_width, image_height, image_layers, image_data, image_format) =
            EngineResources::load_image_data(&PathBuf::from(file_path));
        if vk::Format::UNDEFINED == image_format {
            return Err(format!("Failed to load simple image data: {:?}", file_path));
        }

        Ok(AsyncTextureData {
            _texture_name: texture_name.to_string(),
            _image_width: image_width,
            _image_height: image_height,
            _image_layers: image_layers,
            _image_format: image_format,
            _image_view_type: vk::ImageViewType::TYPE_2D,
            _image_data: image_data,
            _enable_mipmap: true,
            _min_filter: vk::Filter::LINEAR,
            _mag_filter: vk::Filter::LINEAR,
            _wrap_mode: vk::SamplerAddressMode::REPEAT,
        })
    }

    pub fn load_from_meta(texture_name: &str, meta_data: &MetaData) -> Result<Self, String> {
        let file_path = if meta_data._source_file_path.is_file() {
            &meta_data._source_file_path
        } else if meta_data._resource_file_path.is_file() {
            &meta_data._resource_file_path
        } else {
            return Err(format!("Texture file not found in meta_data: {:?}", meta_data));
        };

        let ext = file_path.extension().and_then(|e| e.to_str()).unwrap_or("");
        if ext == EXT_TEXTURE_CUBE || ext == EXT_TEXTURE_2D_ARRAY || ext == EXT_TEXTURE_3D {
            Self::load_combined_texture(texture_name, file_path)
        } else if EXT_TEXTURE.contains(&ext) {
            Self::load_binary_texture(texture_name, file_path)
        } else {
            Self::load_simple_image_texture(texture_name, file_path)
        }
    }
}

impl AsyncMeshData {
    pub fn load_from_meta(mesh_name: &str, meta_data: &MetaData) -> Result<Self, String> {
        let mesh_data_create_info = if !LOAD_FROM_EXTERNAL_FOR_MESH && meta_data._resource_file_path.is_file() {
            let loaded_contents = system::load(&meta_data._resource_file_path);
            if USE_JSON_FOR_MESH {
                serde_json::from_reader(loaded_contents).map_err(|e| {
                    format!(
                        "Failed to deserialize mesh json {:?}: {:?}",
                        meta_data._resource_file_path, e
                    )
                })?
            } else {
                bincode::deserialize_from(loaded_contents).map_err(|e| {
                    format!(
                        "Failed to deserialize mesh bincode {:?}: {:?}",
                        meta_data._resource_file_path, e
                    )
                })?
            }
        } else if meta_data._source_file_path.is_file() {
            let src_file_ext = meta_data._source_file_path.extension().and_then(|ext| ext.to_str()).unwrap_or("");
            let create_info = match src_file_ext {
                EXT_OBJ => WaveFrontOBJ::get_mesh_data_create_infos(&meta_data._source_file_path),
                EXT_COLLADA => Collada::get_mesh_data_create_infos(&meta_data._source_file_path),
                EXT_GLTF_BINARY | EXT_GLTF => GLTF::get_mesh_data_create_infos(&meta_data._source_file_path),
                _ => {
                    if meta_data._resource_file_path.is_file() {
                        let loaded = system::load(&meta_data._resource_file_path);
                        if USE_JSON_FOR_MESH {
                            serde_json::from_reader(loaded).map_err(|e| format!("JSON parse error: {:?}", e))?
                        } else {
                            bincode::deserialize_from(loaded).map_err(|e| format!("Bincode parse error: {:?}", e))?
                        }
                    } else {
                        return Err(format!("Unknown mesh source format: {:?}", meta_data));
                    }
                }
            };

            if !LOAD_FROM_EXTERNAL_FOR_MESH && !meta_data._resource_file_path.as_os_str().is_empty() {
                if let Some(parent) = meta_data._resource_file_path.parent() {
                    let _ = std::fs::create_dir_all(parent);
                }
                if let Ok(mut write_file) = File::create(&meta_data._resource_file_path) {
                    if USE_JSON_FOR_MESH {
                        if let Ok(contents) = serde_json::to_string(&create_info) {
                            let _ = write_file.write_all(contents.as_bytes());
                        }
                    } else if let Ok(contents) = bincode::serialize(&create_info) {
                        let _ = write_file.write_all(&contents);
                    }
                }
            }
            create_info
        } else if meta_data._resource_file_path.is_file() {
            let loaded_contents = system::load(&meta_data._resource_file_path);
            if USE_JSON_FOR_MESH {
                serde_json::from_reader(loaded_contents).map_err(|e| {
                    format!(
                        "Failed to deserialize mesh json {:?}: {:?}",
                        meta_data._resource_file_path, e
                    )
                })?
            } else {
                bincode::deserialize_from(loaded_contents).map_err(|e| {
                    format!(
                        "Failed to deserialize mesh bincode {:?}: {:?}",
                        meta_data._resource_file_path, e
                    )
                })?
            }
        } else {
            return Err(format!("Mesh file not found in meta_data: {:?}", meta_data));
        };

        Ok(AsyncMeshData {
            _mesh_name: mesh_name.to_string(),
            _create_info: mesh_data_create_info,
        })
    }
}

pub struct AsyncResourceLoader {
    pub _request_sender: Sender<AsyncLoadRequest>,
    pub _result_receiver: Receiver<AsyncLoadResult>,
    pub _worker_handles: Vec<JoinHandle<()>>,
}

impl AsyncResourceLoader {
    pub fn new(num_workers: usize) -> Self {
        let (request_sender, request_receiver) = channel::<AsyncLoadRequest>();
        let (result_sender, result_receiver) = channel::<AsyncLoadResult>();
        let request_receiver = Arc::new(Mutex::new(request_receiver));

        let mut worker_handles = Vec::with_capacity(num_workers);
        for worker_id in 0..num_workers {
            let req_rcv = Arc::clone(&request_receiver);
            let res_snd = result_sender.clone();
            let handle = thread::Builder::new()
                .name(format!("ResourceLoaderWorker-{}", worker_id))
                .spawn(move || {
                    while let Ok(request) = {
                        let lock = req_rcv.lock().unwrap();
                        lock.recv()
                    } {
                        let result = match request {
                            AsyncLoadRequest::Texture {
                                _texture_name,
                                _meta_data,
                            } => {
                                let load_res = AsyncTextureData::load_from_meta(&_texture_name, &_meta_data);
                                AsyncLoadResult::Texture {
                                    _texture_name,
                                    _result: load_res,
                                }
                            }
                            AsyncLoadRequest::Mesh { _mesh_name, _meta_data } => {
                                let load_res = AsyncMeshData::load_from_meta(&_mesh_name, &_meta_data);
                                AsyncLoadResult::Mesh {
                                    _mesh_name,
                                    _result: load_res,
                                }
                            }
                        };

                        if res_snd.send(result).is_err() {
                            break;
                        }
                    }
                })
                .expect("Failed to spawn resource loader worker thread");
            worker_handles.push(handle);
        }

        AsyncResourceLoader {
            _request_sender: request_sender,
            _result_receiver: result_receiver,
            _worker_handles: worker_handles,
        }
    }

    pub fn send_request(&self, request: AsyncLoadRequest) -> bool {
        self._request_sender.send(request).is_ok()
    }

    pub fn try_recv_result(&self) -> Option<AsyncLoadResult> {
        self._result_receiver.try_recv().ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_async_resource_loader_worker_lifecycle() {
        let loader = AsyncResourceLoader::new(2);

        let request = AsyncLoadRequest::Texture {
            _texture_name: String::from("non_existent_texture"),
            _meta_data: MetaData::default(),
        };

        assert!(loader.send_request(request));

        let mut received = false;
        let start = std::time::Instant::now();
        while start.elapsed() < std::time::Duration::from_secs(1) {
            if let Some(result) = loader.try_recv_result() {
                match result {
                    AsyncLoadResult::Texture { _texture_name, _result } => {
                        assert_eq!(_texture_name, "non_existent_texture");
                        assert!(_result.is_err());
                        received = true;
                        break;
                    }
                    _ => panic!("Unexpected result type"),
                }
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        assert!(received, "Worker should process and return result");
    }
}
