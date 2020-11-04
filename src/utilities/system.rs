use std::fs;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{ Path, PathBuf };

pub type RcRefCell<T> = Rc<RefCell<T>>;

#[allow(non_snake_case)]
pub fn newRcRefCell<T>(t: T) -> RcRefCell<T> {
    Rc::new(RefCell::new(t))
}

pub fn enum_to_string<T: std::fmt::Debug>(e: &T) -> String {
    String::from(format!("{:?}", e))
}

pub fn get_relative_path(path_parent: &PathBuf, path_child: &PathBuf) -> PathBuf {
    let path_parent: &str = path_parent.to_str().unwrap();
    let path_child: &str = path_child.to_str().unwrap();
    PathBuf::from(&path_child[(path_parent.len() + 1)..])
}

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

fn unique_name_generator<T>(data_map: &HashMap<String, T>, data_name: &String, index: u32) -> String {
    let new_data_name: String = format!("{}_{}", data_name, index);
    match data_map.get(&new_data_name) {
        Some(_) => unique_name_generator(data_map, data_name, index + 1),
        None => new_data_name,
    }
}

pub fn generate_unique_name<T>(data_map: &HashMap<String, T>, data_name: &String) -> String {
    let data = data_map.get(data_name);
    match data {
        Some(_) => unique_name_generator(data_map, data_name, 0),
        None => data_name.clone(),
    }
}

pub fn to_bytes<'a, T>(data: *const T) -> &'a [u8]{
    unsafe {
        std::slice::from_raw_parts(data as *mut u8, std::mem::size_of::<T>())
    }
}
