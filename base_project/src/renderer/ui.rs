use rust_engine_3d::renderer::ui::{
    UIComponentInstance,
    UIManagerBase,
    UIManagerData,
    UIWidgetTypes,
    Widget,
    UILayoutType,
    Orientation,
    HorizontalAlign,
    VerticalAlign
};
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

            static touch_down: fn(widget: *const dyn Widget) = |widget: *const dyn Widget| {
                println!("touch_down");
            };
            static touch_move: fn(widget: *const dyn Widget) = |widget: *const dyn Widget| {
                println!("touch_move");
            };
            static touch_up: fn(widget: *const dyn Widget) = |widget: *const dyn Widget| {
                println!("touch_up");
            };

            let btn = UIManagerData::create_widget(UIWidgetTypes::Default);
            let ui_component = &mut btn.as_mut().unwrap().get_ui_component_mut();
            ui_component.set_pos(25.0,255.0);
            ui_component.set_size(400.0, 200.0);
            ui_component.set_color(get_color32(255, 255, 255, 255));
            ui_component.set_font_color(get_color32(0, 0, 0, 255));
            ui_component.set_border_color(get_color32(255, 0, 0, 255));
            ui_component.set_margine(5.0);
            ui_component.set_round(10.0);
            ui_component.set_border(5.0);
            ui_component.set_dragable(true);
            ui_component.set_touchable(true);
            ui_component.set_text(String::from("Child\nChild Test"));
            ui_component.set_material_instance(&resources.get_material_instance_data("ui/render_ui_test"));
            ui_component._callback_touch_down = Some(&touch_down);
            ui_component._callback_touch_up = Some(&touch_up);
            ui_component._callback_touch_move = Some(&touch_move);
            root.add_widget(btn);

            //
            let btn2 = UIManagerData::create_widget(UIWidgetTypes::Default);
            let ui_component = &mut btn2.as_mut().unwrap().get_ui_component_mut();
            ui_component.set_pos(0.0, 5.0);
            ui_component.set_size(100.0, 50.0);
            ui_component.set_color(get_color32(255, 128, 128, 255));
            ui_component.set_font_color(get_color32(255, 255, 255, 255));
            ui_component.set_border_color(get_color32(0, 0, 0, 255));
            ui_component.set_margine(5.0);
            ui_component.set_round(10.0);
            ui_component.set_border(5.0);
            ui_component.set_dragable(true);
            ui_component.set_touchable(true);
            ui_component.set_expandable(true);
            ui_component.set_text(String::from("Btn2\nBtn2 Test"));
            ui_component._callback_touch_down = Some(&touch_down);
            ui_component._callback_touch_up = Some(&touch_up);
            ui_component._callback_touch_move = Some(&touch_move);
            btn.as_mut().unwrap().add_widget(btn2);

            let btn3 = UIManagerData::create_widget(UIWidgetTypes::Default);
            let ui_component = &mut btn3.as_mut().unwrap().get_ui_component_mut();
            ui_component.set_pos(0.0, 5.0);
            ui_component.set_size(200.0, 100.0);
            ui_component.set_color(get_color32(128, 128, 255, 255));
            ui_component.set_font_color(get_color32(0, 0, 0, 255));
            ui_component.set_border_color(get_color32(0, 0, 0, 128));
            ui_component.set_margine(5.0);
            ui_component.set_round(10.0);
            ui_component.set_border(5.0);
            ui_component.set_dragable(true);
            ui_component.set_touchable(true);
            ui_component.set_text(String::from("Btn2\nBtn2 Test"));
            ui_component._callback_touch_down = Some(&touch_down);
            ui_component._callback_touch_up = Some(&touch_up);
            ui_component._callback_touch_move = Some(&touch_move);
            btn2.as_mut().unwrap().add_widget(btn3);

            //
            let btn2 = UIManagerData::create_widget(UIWidgetTypes::Default);
            let ui_component = &mut btn2.as_mut().unwrap().get_ui_component_mut();
            ui_component.set_layout_type(UILayoutType::BoxLayout);
            ui_component.set_layout_orientation(Orientation::VERTICAL);
            ui_component.set_halign(HorizontalAlign::RIGHT);
            ui_component.set_valign(VerticalAlign::BOTTOM);
            ui_component.set_pos(100.0, 50.0);
            ui_component.set_size(100.0, 100.0);
            ui_component.set_color(get_color32(255, 128, 128, 255));
            ui_component.set_font_color(get_color32(255, 255, 255, 255));
            ui_component.set_border_color(get_color32(0, 0, 0, 255));
            ui_component.set_margine(5.0);
            ui_component.set_round(10.0);
            ui_component.set_border(5.0);
            ui_component.set_dragable(true);
            ui_component.set_touchable(true);
            ui_component.set_expandable(true);
            ui_component.set_text(String::from("Btn2\nBtn2 Test"));
            ui_component._callback_touch_down = Some(&touch_down);
            ui_component._callback_touch_up = Some(&touch_up);
            ui_component._callback_touch_move = Some(&touch_move);
            btn.as_mut().unwrap().add_widget(btn2);

            let btn3 = UIManagerData::create_widget(UIWidgetTypes::Default);
            let ui_component = &mut btn3.as_mut().unwrap().get_ui_component_mut();
            ui_component.set_pos(0.0, 5.0);
            ui_component.set_size(50.0, 75.0);
            ui_component.set_color(get_color32(255, 128, 255, 255));
            ui_component.set_font_color(get_color32(0, 0, 0, 255));
            ui_component.set_border_color(get_color32(0, 0, 0, 128));
            ui_component.set_margine(5.0);
            ui_component.set_round(10.0);
            ui_component.set_border(5.0);
            ui_component.set_dragable(true);
            ui_component.set_touchable(true);
            ui_component.set_expandable(true);
            ui_component.set_text(String::from("Basd\nTasdas"));
            ui_component._callback_touch_down = Some(&touch_down);
            ui_component._callback_touch_up = Some(&touch_up);
            ui_component._callback_touch_move = Some(&touch_move);
            btn2.as_mut().unwrap().add_widget(btn3);

            let btn3 = UIManagerData::create_widget(UIWidgetTypes::Default);
            let ui_component = &mut btn3.as_mut().unwrap().get_ui_component_mut();
            ui_component.set_halign(HorizontalAlign::RIGHT);
            ui_component.set_valign(VerticalAlign::BOTTOM);
            ui_component.set_pos(0.0, 5.0);
            ui_component.set_size(150.0, 50.0);
            ui_component.set_color(get_color32(128, 128, 255, 255));
            ui_component.set_font_color(get_color32(0, 0, 0, 255));
            ui_component.set_border_color(get_color32(0, 0, 0, 128));
            ui_component.set_margine(5.0);
            ui_component.set_margine_top(40.0);
            ui_component.set_padding_top(40.0);
            ui_component.set_round(10.0);
            ui_component.set_border(5.0);
            ui_component.set_dragable(true);
            ui_component.set_touchable(true);
            ui_component.set_expandable(true);
            ui_component.set_text(String::from("Btn3\nBtn3 Test"));
            ui_component._callback_touch_down = Some(&touch_down);
            ui_component._callback_touch_up = Some(&touch_up);
            ui_component._callback_touch_move = Some(&touch_move);
            btn2.as_mut().unwrap().add_widget(btn3);
        }
    }
}

impl UIManager {
    pub fn create_ui_manager() -> Box<UIManager> {
        Box::new(UIManager {
            _ui_manager_data: std::ptr::null(),
        })
    }
}