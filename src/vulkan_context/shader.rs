use std::fs;
use std::io::Read;
use std::path::{
    PathBuf,
};
use std::os::raw::c_char;
use std::process;

use regex::Regex;

use ash::{
    vk,
    Device,
};
use ash::vk::Handle;
use crate::utilities::system;
use crate::resource::resource;

pub fn spirv_file_path_with_defines(is_engine_resource: bool, shader_filename: &PathBuf, shader_defines: &[String]) -> PathBuf {
    let ext = shader_filename.extension().unwrap();
    let mut just_filename = PathBuf::new();
    just_filename.push(shader_filename.parent().unwrap());
    just_filename.push(shader_filename.file_stem().unwrap());

    let mut spirv_file_path = PathBuf::new();
    if is_engine_resource {
        spirv_file_path.push(resource::ENGINE_RESOURCE_PATH);
    } else {
        spirv_file_path.push(resource::PROJECT_RESOURCE_PATH)
    }
    spirv_file_path.push(resource::SHADER_CACHE_DIRECTORY);
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

pub fn get_shader_file_path(shader_filename: &PathBuf) -> (bool, PathBuf) {
    let engine_shader_file_path: PathBuf = PathBuf::from(resource::ENGINE_RESOURCE_PATH)
        .join(resource::SHADER_DIRECTORY)
        .join(shader_filename);
    let project_shader_file_path: PathBuf = PathBuf::from(resource::PROJECT_RESOURCE_PATH)
        .join(resource::SHADER_DIRECTORY)
        .join(shader_filename);
    return if engine_shader_file_path.is_file() && false == project_shader_file_path.is_file() {
        (true, engine_shader_file_path)
    } else {
        (false, project_shader_file_path)
    };
}

pub fn compile_glsl(shader_filename: &PathBuf, shader_defines: &[String]) -> Vec<u8> {
    let (is_engine_resource, shader_file_path) = get_shader_file_path(shader_filename);
    let spirv_file_path: PathBuf = spirv_file_path_with_defines(is_engine_resource, &shader_filename, &shader_defines);

    // collect include files
    let re_include = Regex::new("\\#include\\s+[\"|<](.+?)[\"|>]").unwrap();
    let shader_file_contents: String = fs::read_to_string(&shader_file_path).expect("Something went wrong reading the file");
    let mut included_files: Vec<PathBuf> = Vec::new();
    let shader_file_lines = shader_file_contents.split("\n");
    let shader_file_dir = shader_filename.parent().unwrap().to_path_buf();
    for line in shader_file_lines {
        if let Some(captures) = re_include.captures(line) {
            let included_file = shader_file_dir.join(&captures[1]);
            let (_, included_file_path) = get_shader_file_path(&PathBuf::from(included_file));
            included_files.push(included_file_path);
        }
    }

    // check need to compile
    #[cfg(not(target_os = "android"))]
    let need_to_compile: bool = if spirv_file_path.is_file() {
        let spirv_file_modified_time = fs::metadata(&spirv_file_path).unwrap().modified().unwrap();
        let mut recent_modified_time = fs::metadata(&shader_file_path).unwrap().modified().unwrap();
        for included_file_path in included_files.iter() {
            let included_file_modified_time = fs::metadata(included_file_path).unwrap().modified().unwrap();
            if recent_modified_time < included_file_modified_time {
                recent_modified_time = included_file_modified_time;
            }
        }
        spirv_file_modified_time < recent_modified_time
    } else {
        true
    };

    // compile glsl -> spirv
    #[cfg(not(target_os = "android"))]
    if need_to_compile {
        log::info!("        compile shader: {:?} -> {:?}", shader_file_path, spirv_file_path);

        fs::create_dir_all(spirv_file_path.parent().unwrap()).expect("Failed to create directories.");

        if false == shader_file_path.is_file() {
            panic!("compileGLSL: {:?} does not exist.", shader_file_path);
        }

        let validator_exe = match which::which("glslangValidator") {
            Ok(path) => path,
            Err(_) => panic!("Cannot find glslangValidator executable.\nCheck if it is available in your $PATH\nRead more about it at https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/")
        };

        let mut command = process::Command::new(validator_exe);
        command.arg("-V");
        command.arg("-o");
        command.arg(spirv_file_path.to_str().unwrap());
        command.arg(shader_file_path.to_str().unwrap());
        let shader_define_args: Vec<String> = shader_defines
            .iter()
            .map(|shader_define| {
                let mut arg = shader_define.clone().replace(" ", "");
                arg.insert_str(0, "-D");
                arg
            })
            .collect();
        for shader_define_arg in shader_define_args.iter() {
            command.arg(shader_define_arg);
        }

        command.current_dir(".");
        match command.output() {
            Ok(output) => {
                let msg = String::from_utf8(output.stdout).unwrap();
                if msg.contains("ERROR") {
                    panic!("Compile error: {}", msg);
                }
                if msg.trim() != shader_file_path.to_str().unwrap() {
                    log::error!("{}", msg);
                }
            },
            Err(e) => panic!("failed to execute glslangValidator. {:?}", e),
        }
    }

    // read spirv
    let mut f = system::load(&spirv_file_path);
    let mut buffer: Vec<u8> = Vec::new();
    f.read_to_end(&mut buffer).unwrap();
    let content_size = buffer.len() as u32;
    let remain = content_size % 4;
    if 0 < remain {
        for __i in 0..remain {
            buffer.push(0);
        }
    }
    buffer
}

pub fn create_shader_stage_create_info(
    device: &Device,
    shader_filename: &PathBuf,
    shader_defines: &[String],
    stage_flag: vk::ShaderStageFlags
) -> vk::PipelineShaderStageCreateInfo {
    // ex) shaderDefines = ["STATIC_MESH", "RENDER_SHADOW=true", "SAMPLES=16"]
    let code_buffer = compile_glsl(shader_filename, shader_defines);
    let shader_module_create_info = vk::ShaderModuleCreateInfo {
        code_size: code_buffer.len(),
        p_code: code_buffer.as_ptr() as *const u32,
        ..Default::default()
    };
    unsafe {
        let shader_module = device.create_shader_module(&shader_module_create_info, None).expect("vkCreateShaderModule failed!");
        log::trace!("    create_shader_module: {:#X} {:?}: {:?}", shader_module.as_raw(), stage_flag, shader_filename);
        let main: *const c_char = "main\0".as_ptr() as *const c_char;
        vk::PipelineShaderStageCreateInfo {
            stage: stage_flag,
            module: shader_module,
            p_name: main,
            ..Default::default()
        }
    }
}

pub fn destroy_shader_stage_create_info(device: &Device, shader_stage_create_info: &vk::PipelineShaderStageCreateInfo) {
    if 0 != shader_stage_create_info.module.as_raw() {
        log::debug!("    destroy_shader_module : stage {:?}, module {:?}", shader_stage_create_info.stage, shader_stage_create_info.module);
        unsafe {
            device.destroy_shader_module(shader_stage_create_info.module, None);
        }
    }
}