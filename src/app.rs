use std::sync::Arc;

use egui::{Color32, ColorImage, ImageData, TextureHandle, TextureOptions};
use egui_memory_editor::MemoryEditor;

use crate::chip6502::{Instruction, Opcode};
use crate::Chip6502;

pub struct MyEguiApp {
    chip: Chip6502,
    memory_editor: MemoryEditor,
    screen_texture: TextureHandle,
}

impl MyEguiApp {
    /// Called once before the first frame.
    #[must_use]
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        // You can customize egui here (fonts, visuals) if needed.

        let screen_texture = cc.egui_ctx.load_texture(
            "screen",
            ImageData::Color(Arc::new(ColorImage::new([320, 80], Color32::TRANSPARENT))),
            TextureOptions::default(),
        );

        Self {
            chip: Chip6502::power_up(),
            memory_editor: MemoryEditor::new().with_address_range("All", 0..0xFFFF),
            screen_texture, // Directly initialize screen_texture here
        }
    }
}

impl eframe::App for MyEguiApp {
    /// Called each frame to update the UI.
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // --- Side Panel for Registers and Instructions ---
        egui::SidePanel::left("side_panel")
            .default_width(300.0)
            .show(ctx, |ui| {
                ui.heading("6502 State");
                ui.separator();

                // Display Registers
                show_registers(ui, &self.chip);

                ui.separator();

                // Display Instruction Table
                show_instruction_table(ui, &self.chip.instruction_table);
            });

        // --- Central Panel (can be used for other controls or a title) ---
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("6502 Memory Inspector");
            ui.label("The memory editor is in a separate, draggable window.");
            ui.separator();
            ui.label(
                "You could add execution controls (step, run, stop) or other analysis tools here.",
            );

            egui::ScrollArea::both().show(ui, |ui| {
                // This should obviously not be here, but it's just a test
                let mut img = ColorImage::new([400, 400], Color32::RED);
                for x in 15..=17 {
                    for y in 8..24 {
                        img[(x, y)] = Color32::BLUE;
                        img[(y, x)] = Color32::GREEN;
                    }
                }
                self.screen_texture
                    .set(ColorImage::from(img), TextureOptions::default());
                ui.add(
                    egui::Image::new(&self.screen_texture)
                        .max_height(400.0)
                        .max_width(500.0)
                        .corner_radius(10.0),
                );
            });
        });

        // --- Memory Editor Window ---
        // This creates a floating, draggable, and resizable window for the memory editor.
        let mut is_memory_editor_open = true;
        self.memory_editor.window_ui(
            ctx,
            &mut is_memory_editor_open,
            // Pass mutable access to the RAM
            &mut self.chip.ram,
            // Read callback: provides a byte from memory at a given address
            |mem, address| mem.get(address).copied(),
            // Write callback: writes a byte to memory at a given address
            |mem, address, value| {
                if let Some(byte) = mem.get_mut(address) {
                    *byte = value;
                }
            },
        );
    }
}

/// Renders the register state in a formatted grid.
fn show_registers(ui: &mut egui::Ui, chip: &Chip6502) {
    ui.label(egui::RichText::new("Registers").strong());
    egui::Grid::new("registers_grid")
        .num_columns(2)
        .spacing([40.0, 4.0])
        .striped(true)
        .show(ui, |ui| {
            ui.label("A (Accumulator):");
            ui.monospace(format!("0x{:02X}", chip.a));
            ui.end_row();

            ui.label("X (Index Register):");
            ui.monospace(format!("0x{:02X}", chip.x));
            ui.end_row();

            ui.label("Y (Index Register):");
            ui.monospace(format!("0x{:02X}", chip.y));
            ui.end_row();

            ui.label("S (Stack Pointer):");
            ui.monospace(format!("0x{:02X}", chip.s));
            ui.end_row();

            ui.label("P (Status Register):");
            ui.monospace(format!("0b{:08b}", chip.p));
            ui.end_row();

            ui.label("PC (Program Counter):");
            ui.monospace(format!("0x{:04X}", chip.pc));
            ui.end_row();
        });
}

/// Renders the instruction table in a scrollable, filterable area.
fn show_instruction_table(ui: &mut egui::Ui, instructions: &[Instruction]) {
    ui.label(egui::RichText::new("Instruction Set").strong());

    // Use a ScrollArea to handle the large number of instructions.
    egui::ScrollArea::vertical()
        .auto_shrink([false, false])
        .show(ui, |ui| {
            egui::Grid::new("instruction_table_grid")
                .num_columns(4)
                .spacing([20.0, 4.0])
                .striped(true)
                .show(ui, |ui| {
                    // Header row
                    ui.label(egui::RichText::new("Opcode").strong());
                    ui.label(egui::RichText::new("Addressing").strong());
                    ui.label(egui::RichText::new("Cycles").strong());
                    ui.end_row();

                    // Instruction rows
                    #[allow(clippy::explicit_iter_loop)]
                    for (i, instruction) in instructions.iter().enumerate() {
                        if instruction.opcode == Opcode::XXX {
                            continue;
                        }

                        ui.monospace(format!("0x{i:02X}"));
                        ui.monospace(format!("{:?}", instruction.opcode));
                        ui.label(format!("{:?}", instruction.adressing_mode));
                        ui.monospace(format!("{}", instruction.cycle_count));
                        ui.end_row();
                    }
                });
        });
}
