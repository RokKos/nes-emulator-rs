use crate::Chip6502;

#[derive(Debug)]
pub struct MyEguiApp {
    chip: Chip6502,
}

impl Default for MyEguiApp {
    fn default() -> Self {
        Self {
            chip: Chip6502::power_up(),
        }
    }
}

impl MyEguiApp {
    #[must_use]
    pub fn new(_cc: &eframe::CreationContext<'_>) -> Self {
        // Customize egui here with cc.egui_ctx.set_fonts and cc.egui_ctx.set_visuals.
        // Restore app state using cc.storage (requires the "persistence" feature).
        // Use the cc.gl (a glow::Context) to create graphics shaders and buffers that you can use
        // for e.g. egui::PaintCallback.
        Self::default()
    }
}

impl eframe::App for MyEguiApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Hello World!");

            ui.label(format!("{0}", self.chip.pc));
        });
    }
}
