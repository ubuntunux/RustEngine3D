use std::fs;
use std::io::prelude::{ Write };
use std::path::{ Path, PathBuf };

fn walk_directory_recursive(dir: &Path, extensions: &[&str], out_contents: &mut Vec<PathBuf>) {
    let contents = fs::read_dir(dir).unwrap();
    for content in contents {
        let content = content.unwrap().path();
        if content.is_dir() {
            walk_directory_recursive(&content, extensions, out_contents);
        } else {
            let ext = content.extension();
            if extensions.is_empty() || (ext.is_some() && extensions.contains(&ext.unwrap().to_str().unwrap())) {
                out_contents.push(PathBuf::from(content));
            }
        }
    }
}

pub fn walk_directory(dir: &Path, extensions: &[&str]) -> Vec<PathBuf> {
    let mut out_contents: Vec<PathBuf> = Vec::new();
    walk_directory_recursive(dir, extensions, &mut out_contents);
    out_contents
}

// fn get_root_path() -> &'static Path {
//     Path::new(env!("CARGO_MANIFEST_DIR"))
// }

fn main() {
    let files = walk_directory(&Path::new("resource"), &[]);
    let mut write_file = fs::File::create("resource/resources.txt").expect("Failed to create file");
    for filename in files {
        write_file.write(format!("{}\n", filename.to_str().unwrap()).as_bytes()).expect("Failed to write");
    }
}