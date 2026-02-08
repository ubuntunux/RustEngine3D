use std::collections::HashMap;
use std::ops::Drop;
use std::time::Instant;
use std::sync::{OnceLock, Mutex};

pub static TIME_PROFILER: OnceLock<Mutex<HashMap<String, f64>>> = OnceLock::new();

pub struct ScopeTimer {
    _label: String,
    _start: Instant,
}

impl ScopeTimer {
    pub fn new(label: &str) -> ScopeTimer {
        let timer = ScopeTimer {
            _label: label.to_string(),
            _start: Instant::now(),
        };
        timer
    }
}

impl Drop for ScopeTimer {
    fn drop(&mut self) {
        // to ms
        let mut duration = self._start.elapsed().as_secs_f64() * 1000.0;
        let time_profiler_mutex = TIME_PROFILER.get_or_init(|| Mutex::new(HashMap::new()));
        let mut time_profiler = time_profiler_mutex.lock().unwrap();

        if let Some(duration_prev) = time_profiler.get(&self._label) {
            duration += *duration_prev;
        }
        time_profiler.insert(self._label.clone(), duration);
    }
}
