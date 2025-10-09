use std::collections::HashMap;
use std::ops::Drop;
use std::time::Instant;

pub static mut TIME_PROFILER: Option<HashMap<String, f64>> = None;

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
        unsafe {
            if TIME_PROFILER.is_none() {
                TIME_PROFILER = Some(HashMap::new());
            }

            let time_profiler: &mut HashMap<String, f64> = TIME_PROFILER.as_mut().unwrap();
            if let Some(duration_prev) = time_profiler.get(&self._label) {
                duration += *duration_prev;
            }
            time_profiler.insert(self._label.clone(), duration);
        }
    }
}
