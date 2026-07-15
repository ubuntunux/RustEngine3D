use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::io::Cursor;
use std::path::{Path, PathBuf};
use std::rc::{Rc, Weak};
use strum_macros::EnumIter;
use uuid::Uuid;

#[derive(Clone, Copy, Debug, Hash, PartialEq, EnumIter)]
pub enum State {
    End,
    Begin,
    Update,
}

#[macro_export]
macro_rules! begin_block {
    ($name:expr) => {
        // BeginBlock($name)
    };
}

pub type RcRefCell<T> = Rc<RefCell<T>>;
pub type WeakRefCell<T> = Weak<RefCell<T>>;

pub fn ptr_as_ref<'a, T: ?Sized>(t: *const T) -> &'a T {
    unsafe { &*t }
}
pub fn ptr_as_mut<'a, T: ?Sized>(t: *const T) -> &'a mut T {
    unsafe { &mut *(t as *mut T) }
}
#[allow(non_snake_case)]
pub fn newRcRefCell<T>(t: T) -> RcRefCell<T> {
    Rc::new(RefCell::new(t))
}

#[allow(non_snake_case)]
pub fn intoWeakRefCell<T>(t: &RcRefCell<T>) -> WeakRefCell<T> {
    Rc::downgrade(t)
}

#[allow(non_snake_case)]
pub fn intoRcRefCell<T>(t: &WeakRefCell<T>) -> Option<RcRefCell<T>> {
    Weak::upgrade(t)
}

pub fn enum_to_string<T: std::fmt::Debug>(e: &T) -> String {
    format!("{:?}", e)
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

fn unique_name_generator<T>(data_map: &HashMap<String, T>, data_name: &str, index: u32) -> String {
    let new_data_name: String = format!("{}_{}", data_name, index);
    match data_map.get(&new_data_name) {
        Some(_) => unique_name_generator(data_map, data_name, index + 1),
        None => new_data_name,
    }
}

pub fn generate_unique_name<T>(data_map: &HashMap<String, T>, data_name: &str) -> String {
    let data = data_map.get(data_name);
    match data {
        Some(_) => unique_name_generator(data_map, data_name, 0),
        None => String::from(data_name),
    }
}

pub fn convert_to_bytes<'a, T>(data: *const T) -> &'a [u8] {
    unsafe { std::slice::from_raw_parts(data as *mut u8, std::mem::size_of::<T>()) }
}

pub fn convert_vec<S, D>(src: Vec<S>) -> Vec<D> {
    unsafe {
        let size_of_src = std::mem::size_of::<S>();
        let size_of_dst = std::mem::size_of::<D>();
        let length = src.len() * size_of_src / size_of_dst;
        let capacity = src.capacity() * size_of_src / size_of_dst;
        let ptr = src.as_ptr() as *mut D;
        std::mem::forget(src); // Don't run the destructor for vec32
        Vec::from_raw_parts(ptr, length, capacity) // Construct new Vec
    }
}

#[cfg(not(target_os = "android"))]
pub fn load<P: AsRef<Path>>(path: P) -> Cursor<Vec<u8>> {
    use std::fs::File;
    use std::io::Read;

    let mut buf = Vec::new();
    let file = File::open(&path);
    if file.is_ok() {
        file.unwrap().read_to_end(&mut buf).unwrap();
    } else {
        log::info!("file not found: {}", path.as_ref().display());
    }
    Cursor::new(buf)
}

#[cfg(target_os = "android")]
pub fn load<P: AsRef<Path>>(path: P) -> Cursor<Vec<u8>> {
    use std::io::Read;

    let asset_manager = ndk_glue::native_activity().asset_manager();

    let path = path.as_ref().strip_prefix("resources/").unwrap().to_str().unwrap();
    let mut asset = asset_manager.open(&std::ffi::CString::new(path).unwrap()).unwrap();

    let mut buf = Vec::new();
    asset.read_to_end(&mut buf).unwrap();
    Cursor::new(buf)
}

pub fn format_name_with_uuid(name: &str, uuid: Uuid) -> String {
    format!("{}_{}", name, uuid)
}

pub fn extract_name_and_uuid(raw_key: &str) -> (String, Option<Uuid>) {
    if raw_key.len() < 37 {
        return (raw_key.to_string(), None);
    }

    let split_idx = raw_key.len() - 36;
    let uuid_candidate = &raw_key[split_idx..];

    let prefix = &raw_key[..split_idx];
    if (prefix.ends_with('_') || prefix.ends_with('-')) && Uuid::parse_str(uuid_candidate).is_ok() {
        let pure_name = &prefix[..prefix.len() - 1];
        let parsed_uuid = Uuid::parse_str(uuid_candidate).unwrap();
        return (pure_name.to_string(), Some(parsed_uuid));
    }
    (raw_key.to_string(), None)
}