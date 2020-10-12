use std::os::raw::c_char;
use std::path::{
    Path,
    PathBuf,
};

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

pub const SHADER_CACHE_DIRECTORY: &str = "Resource/ShaderCaches";
pub const SHADER_DIRECTORY: &str = "Resource/Shaders";

pub fn spirv_file_path_with_defines(shader_filename: &String, shader_defines: &[String]) -> PathBuf {
    let filepath = Path::new(shader_filename);
    let ext = filepath.extension().unwrap();
    let mut just_filename = PathBuf::new();
    just_filename.push(filepath.parent().unwrap());
    just_filename.push(filepath.file_stem().unwrap());
    let mut spirv_file_path = PathBuf::from(SHADER_DIRECTORY);
    if shader_defines.is_empty() {
        spirv_file_path.push(just_filename);
    } else {
        let mut filename_with_defines: String = String::from(just_filename.to_str().unwrap());
        let mut shader_file_post_fix: String = shader_defines.join("_");
        shader_file_post_fix = shader_file_post_fix.replace("=", "");
        filename_with_defines.push_str(&shader_file_post_fix);
        spirv_file_path.push(filename_with_defines);
    }
    spirv_file_path.push(ext);
    spirv_file_path.push(".spirv");
    spirv_file_path
}


pub fn compile_GLSL(shader_filename: &PathBuf, shader_defines: &[String]) -> (usize, *const u32) {
    (0, std::ptr::null())
}
// compileGLSL :: FilePath -> [Text.Text] -> IO (Int, Ptr Word32)
// compileGLSL shaderFileName shaderDefines = do
//     validatorExe <- fromMaybe
//         ( error $ unlines
//           [ "Cannot find glslangValidator executable."
//           , "Check if it is available in your $PATH."
//           , "Read more about it at https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/"]
//         ) <$> findExecutable "glslangValidator"
//     tmpDir <- getTemporaryDirectory
//     curDir <- getCurrentDirectory
//     createDirectoryIfMissing True tmpDir
//     let shaderDefineArgs = Text.unpack . Text.unwords . map (\d -> Text.append (Text.pack "-D") (Text.replace " " "" d)) $ shaderDefines
//         shaderFilePath = shaderDirectory </> shaderFileName
//         shaderDir = takeDirectory shaderFilePath
//         spirvCodeFile = spirvFilePathWithDefines shaderFileName shaderDefines
//         forceConvert = True -- need recursive include file time diff implementation
//     -- convert glsl -> spirv
//     doConvert shaderFilePath spirvCodeFile >>= \result ->
//         when (result || forceConvert) $ do
//             createDirectoryIfMissing True (takeDirectory spirvCodeFile)
//             doesFileExist shaderFilePath >>= flip unless (error $ "compileGLSL: " ++ shaderFilePath ++ " does not exist.")
//             (exitCode, stdo, stde) <- readCreateProcessWithExitCode
//                 ((shell $ validatorExe ++ " -V -o " ++ spirvCodeFile ++ " " ++ shaderFilePath ++ " " ++ shaderDefineArgs) { cwd = Just "." }) ""
//             case exitCode of
//                 ExitSuccess -> pure ()
//                 ExitFailure i -> do
//                     logInfo stdo
//                     logInfo stde
//                     error $ "glslangValidator exited with code " ++ show i ++ "."
//
//     -- read spirv
//     withBinaryFile spirvCodeFile ReadMode $ \h -> do
//         fsize <- hFileSize h
//         let contentSize = fromIntegral $ case rem fsize 4 of
//               0 -> fsize
//               k -> fsize + 4 - k
//         contentsPtr <- mallocArray contentSize
//         hasRead <- hGetBuf h contentsPtr contentSize
//         unless (contentSize /= hasRead) $ do
//             contents <- peekArray hasRead contentsPtr
//             pokeArray contentsPtr (contents ++ (replicate (contentSize - hasRead) 0))
//         return (contentSize, contentsPtr)
//     where
//         -- TODO : need recursive include file time diff implementation
//         doConvert shaderFilePath spirvFilePath = do
//             doesFileExist spirvFilePath >>= \case
//                 True -> do
//                     shaderFileTime <- getModificationTime shaderFilePath
//                     spirvFileTime <- getModificationTime spirvFilePath
//                     return $ (0::NominalDiffTime) < diffUTCTime shaderFileTime spirvFileTime
//                 False -> return True

pub fn create_shader_stage_create_info(
    device: &Device,
    shader_filename: &PathBuf,
    shader_defines: &[String],
    stage_flag: vk::ShaderStageFlags
) -> vk::PipelineShaderStageCreateInfo {
    log::info!("createShaderStageCreateInfo: {:?}: {:?}", stage_flag, shader_filename);
    // ex) shaderDefines = ["STATIC_MESH", "RENDER_SHADOW=true", "SAMPLES=16"]
    let (code_size, code_ptr) = compile_GLSL(shader_filename, shader_defines);
    let shader_module_create_info = vk::ShaderModuleCreateInfo {
        code_size: code_size,
        p_code: code_ptr,
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