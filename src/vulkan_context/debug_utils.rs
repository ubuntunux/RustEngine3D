use std::ffi::CString;
use std::os::raw::c_char;
use ash::vk;
use ash::Device;
use ash::extensions::ext::DebugUtils;

// ScopedDebugLabel
enum DebugLabelType {
    CmdDebugLabel(vk::CommandBuffer),
    QueueDebugLabel(vk::Queue),
}

pub struct ScopedDebugLabel {
    _debug_utils: *const DebugUtils,
    _label_type: DebugLabelType,
    _label_text: CString,
    _label: vk::DebugUtilsLabelEXT,
}

impl Drop for ScopedDebugLabel {
    fn drop(&mut self) {
        self.call_end_debug_utils_label();
    }
}

impl ScopedDebugLabel {
    pub fn create_scoped_cmd_label(debug_utils: *const DebugUtils, command_buffer: vk::CommandBuffer, label_name: &str) -> ScopedDebugLabel {
        ScopedDebugLabel::create_scoped_label(debug_utils, label_name, DebugLabelType::CmdDebugLabel(command_buffer))
    }

    pub fn create_scoped_queue_label(debug_utils: *const DebugUtils, command_queue: vk::Queue, label_name: &str) -> ScopedDebugLabel {
        ScopedDebugLabel::create_scoped_label(debug_utils, label_name, DebugLabelType::QueueDebugLabel(command_queue))
    }

    fn create_scoped_label(debug_utils: *const DebugUtils, label_name: &str, label_type: DebugLabelType) -> ScopedDebugLabel {
        let label_text: CString = CString::new(label_name).unwrap();
        let label_name_ptr = label_text.as_ptr() as *const c_char;
        let label = ScopedDebugLabel {
            _debug_utils: debug_utils,
            _label_type: label_type,
            _label_text: label_text,
            _label: vk::DebugUtilsLabelEXT {
                s_type: vk::StructureType::DEBUG_UTILS_LABEL_EXT,
                p_label_name: label_name_ptr,
                ..Default::default()
            },
        };
        label.call_begin_debug_utils_label();
        label
    }

    fn call_begin_debug_utils_label(&self) {
        unsafe {
            match self._label_type {
                DebugLabelType::CmdDebugLabel(command_buffer) => (*self._debug_utils).cmd_begin_debug_utils_label(command_buffer, &self._label),
                DebugLabelType::QueueDebugLabel(command_queue) => (*self._debug_utils).queue_begin_debug_utils_label(command_queue, &self._label)
            }
        }
    }

    fn call_end_debug_utils_label(&self) {
        unsafe {
            match self._label_type {
                DebugLabelType::CmdDebugLabel(command_buffer) => (*self._debug_utils).cmd_end_debug_utils_label(command_buffer),
                DebugLabelType::QueueDebugLabel(command_queue) => (*self._debug_utils).queue_end_debug_utils_label(command_queue)
            }
        }
    }
}

pub fn set_object_debug_info(device: &Device, debug_utils: &DebugUtils, object_name: &str, object_type: vk::ObjectType, object_handle: u64) {
    let object_name_string: CString = CString::new(object_name).unwrap();
    let object_name_ptr = object_name_string.as_ptr() as *const c_char;
    let object_name_info = vk::DebugUtilsObjectNameInfoEXT {
        s_type: vk::StructureType::DEBUG_UTILS_OBJECT_NAME_INFO_EXT,
        object_type: object_type,
        object_handle: object_handle,
        p_object_name: object_name_ptr,
        ..Default::default()
    };
    unsafe {
        debug_utils.set_debug_utils_object_name(device.handle(), &object_name_info).expect("failed to set_debug_utils_object_name.");
    }
}