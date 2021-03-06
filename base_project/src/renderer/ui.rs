use rust_engine_3d::renderer::ui::{UIManagerBase, UIManagerData, UIWidgetTypes, Widget};
use rust_engine_3d::renderer::renderer::RendererData;
use rust_engine_3d::resource::resource::Resources;
use rust_engine_3d::vulkan_context::vulkan_context::{ get_color32 };


pub struct UIManager {
    pub _ui_manager_data: *const UIManagerData
}

impl UIManagerBase for UIManager {
    fn get_ui_manager_data(&self) -> &UIManagerData {
        unsafe { &*(self._ui_manager_data) }
    }

    fn get_ui_manager_data_mut(&self) -> &mut UIManagerData {
        unsafe { &mut *(self._ui_manager_data as *mut UIManagerData) }
    }

    fn initialize_ui_manager(&mut self, ui_manager_data: &UIManagerData) {
        self._ui_manager_data = ui_manager_data;
    }

    fn build_ui(&mut self, _renderer_data: &RendererData, resources: &Resources) {
        unsafe {
            let root = &mut *(self.get_ui_manager_data().get_root_ptr() as *mut dyn Widget);

            let btn = UIManagerData::create_widget(UIWidgetTypes::Default);
            let ui_component = &mut btn.as_mut().unwrap().get_ui_component_mut();
            ui_component.set_pos(25.0,255.0);
            ui_component.set_size(200.0, 100.0);
            ui_component.set_color(get_color32(255, 255, 255, 255));
            ui_component.set_font_color(get_color32(0, 0, 0, 255));
            ui_component.set_border_color(get_color32(255, 0, 0, 255));
            ui_component.set_margine(5.0);
            ui_component.set_round(10.0);
            ui_component.set_border(5.0);
            ui_component.set_text(String::from("Child\nChild Test"));
            ui_component.set_material_instance(&resources.get_material_instance_data("ui/render_ui_test"));
            root.add_widget(btn);

            let btn2 = UIManagerData::create_widget(UIWidgetTypes::Default);
            let ui_component = &mut btn2.as_mut().unwrap().get_ui_component_mut();
            ui_component.set_pos(200.0, 255.0);
            ui_component.set_size(100.0, 50.0);
            ui_component.set_color(get_color32(255, 128, 128, 255));
            ui_component.set_font_color(get_color32(255, 255, 255, 255));
            ui_component.set_border_color(get_color32(0, 0, 0, 255));
            ui_component.set_margine(5.0);
            ui_component.set_round(10.0);
            ui_component.set_border(5.0);

            ui_component.set_text(String::from("Btn2\nBtn2 Test"));
            root.add_widget(btn2);
        }
    }
}

impl UIManager {
    pub fn create_ui_manager() -> UIManager {
        UIManager {
            _ui_manager_data: std::ptr::null(),
        }
    }
}