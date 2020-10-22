

pub fn enum_to_string<T: std::fmt::Debug>(e: &T) -> String {
    String::from(format!("{:?}", e))
}