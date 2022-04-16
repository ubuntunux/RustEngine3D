use std::collections::HashMap;
use std::path::PathBuf;
use std::io::BufReader;
use xml::EventReader;
use xml::reader::XmlEvent;
use crate::utilities::system;

type XmlTreeMap = HashMap<String, Vec<XmlTree>>;

#[derive(Debug, Clone)]
pub struct XmlTree {
    pub parent: *const XmlTree,
    pub name: String,
    pub attributes: HashMap<String, String>,
    pub text: String,
    pub children: XmlTreeMap,
}

impl Default for XmlTree {
    fn default() -> XmlTree {
        XmlTree {
            parent: std::ptr::null(),
            name: String::new(),
            attributes: HashMap::new(),
            text: String::new(),
            children: HashMap::new(),
        }
    }
}

pub fn get_elements_attribute(xml_tree: &Option<&Vec<XmlTree>>, attribute_name: &str, default_value: &str) -> String {
    if xml_tree.is_some() {
        let attribute_value = xml_tree.unwrap()[0].attributes.get(attribute_name);
        if attribute_value.is_some() {
            return attribute_value.unwrap().clone();
        }
    }
    String::from(default_value)
}

pub fn get_elements_text(xml_tree: &Option<&Vec<XmlTree>>, default_value: &str) -> String {
    match xml_tree {
        Some(xml_elements) => xml_elements[0].text.clone(),
        None => String::from(default_value),
    }
}

pub fn get_element_attribute(xml_tree: &Option<&XmlTree>, attribute_name: &str, default_value: &str) -> String {
    if xml_tree.is_some() {
        let attribute_value = xml_tree.unwrap().attributes.get(attribute_name);
        if attribute_value.is_some() {
            return attribute_value.unwrap().clone();
        }
    }
    String::from(default_value)
}

pub fn get_element_text(xml_tree: &Option<&XmlTree>, default_value: &str) -> String {
    match xml_tree {
        Some(xml_element) => xml_element.text.clone(),
        None => String::from(default_value),
    }
}


impl XmlTree {
    pub fn parse(filepath: &PathBuf) -> XmlTree {
        let file = system::load(filepath);
        let file = BufReader::new(file);
        let parser = EventReader::new(file);

        let mut xml_root = XmlTree::default();
        let mut xml_tree: *mut XmlTree = &mut xml_root;
        unsafe {
            for e in parser {
                match e {
                    Ok(XmlEvent::StartElement { name, attributes, namespace: _namespace }) => {
                        let children: &mut Vec<XmlTree> = match (*xml_tree).children.get_mut(&name.local_name) {
                            Some(children) => children,
                            None => {
                                (*xml_tree).children.insert(name.local_name.clone(), Vec::new());
                                (*xml_tree).children.get_mut(&name.local_name).unwrap()
                            }
                        };
                        children.push(XmlTree {
                            parent: xml_tree,
                            name: name.local_name.clone(),
                            ..Default::default()
                        });
                        xml_tree = children.last_mut().unwrap();

                        for attribute in attributes {
                            (*xml_tree).attributes.insert(attribute.name.local_name.clone(), attribute.value.clone());
                        }
                    },
                    Ok(XmlEvent::Characters(text)) => {
                        (*xml_tree).text = text.clone();
                    },
                    Ok(XmlEvent::EndElement { name: _name }) => {
                        xml_tree = (*xml_tree).parent as *mut XmlTree;
                    },
                    Err(e) => {
                        panic!("Error: {}", e);
                    },
                    _ => { }
                }
            }
        }
        xml_root
    }

    fn get_elements_from_paths(&self, paths: &Vec<&str>, index: usize) -> Option<&Vec<XmlTree>> {
        match self.children.get(paths[index]) {
            None => None,
            maybe_children => {
                if (index + 1) == paths.len() {
                    maybe_children
                } else {
                    maybe_children.unwrap().get(0).unwrap().get_elements_from_paths(paths, index + 1)
                }
            }
        }
    }

    pub fn get_elements(&self, path: &str) -> Option<&Vec<XmlTree>> {
        let paths: Vec<&str> = path.split("/").collect();
        self.get_elements_from_paths(&paths, 0)
    }

    pub fn get_element(&self, path: &str) -> Option<&XmlTree> {
        let paths: Vec<&str> = path.split("/").collect();
        match self.get_elements_from_paths(&paths, 0) {
            Some(elements) => Some(&elements[0]),
            None => None,
        }
    }

    pub fn get_attribute(&self, attribute_name: &str, default_value: &str) -> String {
        match self.attributes.get(attribute_name) {
            Some(attribute_value) => attribute_value.clone(),
            None => String::from(default_value),
        }
    }
}