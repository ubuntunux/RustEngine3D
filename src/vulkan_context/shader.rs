use std::env;
use std::fs;
use std::io::Read;
use std::path::{
    Path,
    PathBuf,
};
use std::os::raw::c_char;
use std::process;

use tempdir::TempDir;
use nalgebra::{
    Matrix4
};
use ash::{
    vk,
    Device,
};
use ash::version::{
    DeviceV1_0
};
use std::fs::create_dir_all;

pub const SHADER_CACHE_DIRECTORY: &str = "resource/shader_caches";
pub const SHADER_DIRECTORY: &str = "resource/shaders";

pub fn spirv_file_path_with_defines(shader_filename: &PathBuf, shader_defines: &[String]) -> PathBuf {
    let ext = shader_filename.extension().unwrap();
    let mut just_filename = PathBuf::new();
    just_filename.push(shader_filename.parent().unwrap());
    just_filename.push(shader_filename.file_stem().unwrap());
    let mut spirv_file_path = PathBuf::from(SHADER_CACHE_DIRECTORY);
    spirv_file_path.push(just_filename);
    let mut spirv_file_path_str: String = String::from(spirv_file_path.to_str().unwrap());
    if false == shader_defines.is_empty() {
        let mut combined_shader_define: String = shader_defines.join("_");
        combined_shader_define = combined_shader_define.replace("=", "");
        spirv_file_path_str.push_str("_");
        spirv_file_path_str.push_str(&combined_shader_define);
    }
    spirv_file_path_str.push('.');
    spirv_file_path_str.push_str(ext.to_str().unwrap());
    spirv_file_path_str.push_str(".spirv");
    PathBuf::from(spirv_file_path_str)
}


pub fn compile_GLSL(shader_filename: &PathBuf, shader_defines: &[String]) -> Vec<u8> {
    let validator_exe = match which::which("glslangValidator") {
        Ok(path) => path,
        Err(_) => panic!("Cannot find glslangValidator executable.\nCheck if it is available in your $PATH\nRead more about it at https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/")
    };

    let shader_define_args: Vec<String> = shader_defines
        .iter()
        .map(|shader_define| {
            let mut arg = shader_define.clone().replace(" ", "");
            arg.insert_str(0, "-D");
            arg
        })
        .collect();
    let combined_shader_defines: String = shader_define_args.join(" ");
    let mut shader_file_path: PathBuf = PathBuf::from(SHADER_DIRECTORY);
    shader_file_path.push(shader_filename);

    let mut shader_dir = PathBuf::new();
    shader_dir.push(shader_file_path.parent().unwrap());
    shader_dir.push(shader_file_path.file_stem().unwrap());

    let spirv_file_path: PathBuf = spirv_file_path_with_defines(&shader_filename, &shader_defines);
    let force_convert: bool = true; // need recursive include file time diff implementation
    // TODO : need recursive include file time diff implementation
    let do_convert: bool = if spirv_file_path.is_file() {
        let shader_file_metadata = fs::metadata(&shader_file_path).unwrap();
        let spirv_file_metadata = fs::metadata(&spirv_file_path).unwrap();
        spirv_file_metadata.modified().unwrap() < shader_file_metadata.modified().unwrap()
    } else {
        true
    };

    // convert glsl -> spirv
    if do_convert || force_convert {
        fs::create_dir_all(spirv_file_path.parent().unwrap());
        if false == shader_file_path.is_file() {
            panic!("compileGLSL: {:?} does not exist.", shader_file_path);
        }

        let mut command = process::Command::new(validator_exe);
        command.arg("-V");
        command.arg("-o");
        command.arg(spirv_file_path.to_str().unwrap());
        command.arg(shader_file_path.to_str().unwrap());
        if false == shader_define_args.is_empty() {
            command.arg(combined_shader_defines);
        }
        command.current_dir(".");
        let output = command.output().expect("failed to execute glslangValidator.");
        log::info!("{}", String::from_utf8(output.stdout).unwrap());
    }

    // read spirv
    let mut f = fs::File::open(&spirv_file_path).unwrap();
    let mut buffer: Vec<u8> = Vec::new();
    f.read_to_end(&mut buffer).unwrap();
    let mut content_size = buffer.len() as u32;
    let remain = content_size % 4;
    if 0 < remain {
        for i in 0..remain {
            buffer.push(0);
        }
        content_size = content_size + 4 - remain;
    }
    buffer
}

pub fn create_shader_stage_create_info(
    device: &Device,
    shader_filename: &PathBuf,
    shader_defines: &[String],
    stage_flag: vk::ShaderStageFlags
) -> vk::PipelineShaderStageCreateInfo {
    log::info!("createShaderStageCreateInfo: {:?}: {:?}", stage_flag, shader_filename);
    // ex) shaderDefines = ["STATIC_MESH", "RENDER_SHADOW=true", "SAMPLES=16"]
    let code_buffer = compile_GLSL(shader_filename, shader_defines);
    let shader_module_create_info = vk::ShaderModuleCreateInfo {
        code_size: code_buffer.len(),
        p_code: code_buffer.as_ptr() as *const u32,
        ..Default::default()
    };
    unsafe {
        let shader_module = device.create_shader_module(&shader_module_create_info, None).expect("vkCreateShaderModule failed!");
        let main: *const c_char = "main".as_ptr() as *const c_char;
        vk::PipelineShaderStageCreateInfo {
            stage: stage_flag,
            module: shader_module,
            p_name: main,
            ..Default::default()
        }
    }
}

pub fn destroy_shader_stage_create_info(device: &Device, shader_stage_create_info: &vk::PipelineShaderStageCreateInfo) {
    log::info!("destroyShaderStageCreateInfo : stage {:?}, module {:?}", shader_stage_create_info.stage, shader_stage_create_info.module);
    unsafe {
        device.destroy_shader_module(shader_stage_create_info.module, None);
    }
}