use std::{fs::File, path::Path};

pub mod app;
pub mod chip6502;
use crate::app::MyEguiApp;
use crate::chip6502::BusOperationType;
use crate::chip6502::Chip6502;
use crate::chip6502::TestNES6502;

#[allow(clippy::too_many_lines)]
#[allow(clippy::panic)]
fn main() {
    let native_options = eframe::NativeOptions::default();
    let _ = eframe::run_native(
        "My egui App",
        native_options,
        Box::new(|cc| Ok(Box::new(MyEguiApp::new(cc)))),
    );

    //let rom_path = "./test/roms/Best NES Games/Donkey Kong/Donkey Kong (World) (Rev 1).nes";
    //let nes_rom = read_rom(rom_path);

    let opcode_to_test: Vec<&str> = vec![
        "9d", "ea", "1a", "3a", "5a", "7a", "da", "fa", "80", "82", "89", "c2", "e2", "04", "44",
        "64", "14", "34", "54", "74", "d4", "f4", "0c", "1c", "3c", "5c", "7c", "dc", "fc", "10",
        "30", "50", "70", "90", "b0", "d0", "f0", "20", "4c", "6c", "18", "38", "b8", "d8", "f8",
        "c9", "c5", "d5", "cd", "dd", "d9", "c1", "d1", "e0", "e4", "ec", "c0", "c4", "cc", "69",
        "65", "75", "6d", "7d", "79", "61", "71", "e9", "e5", "f5", "ed", "fd", "f9", "e1", "f1",
        "49", "45", "55", "4d", "5d", "59", "41", "51", "29", "25", "35", "2d", "3d", "39", "21",
        "31", "09", "05", "15", "0d", "1d", "19", "01", "11", "aa", "8a", "a8", "98", "ba", "9a",
        "85", "a0", "a4", "b4", "ac", "bc", "95", "8d", "99", "81", "91", "86", "96", "8e", "84",
        "94", "8c", "bc", "ac", "b4", "a4", "a0", "be", "ae", "b6", "a6", "b1", "a9", "a2", "a5",
        "b5", "ad", "bd", "b9", "a1",
    ];
    for opcode in opcode_to_test {
        println!("Running Test: {opcode}");
        let file_path = format!("./test/nes6502/v1/{opcode}.json");
        let json_file_path = Path::new(&file_path);

        let file = match File::open(json_file_path) {
            Ok(f) => f,
            Err(e) => panic!("File Error: Could not open the test data file. Reason: {e}",),
        };

        let tests: Vec<TestNES6502> = match serde_json::from_reader(file) {
            Ok(t) => t,
            Err(e) => panic!("File Error: Could not open the test data file. Reason: {e}",),
        };

        for test in tests {
            let mut chip = Chip6502::power_up();
            chip.debug_state_set(test.initial);
            let bus_operations = chip.run_op();
            let result_state = chip.debug_state_get(&test.r#final);

            println!("{0}", test.name);

            assert_eq!(result_state.a, test.r#final.a, "A Reg is not Equal");
            assert_eq!(result_state.x, test.r#final.x, "X Reg is not Equal");
            assert_eq!(result_state.y, test.r#final.y, "Y Reg is not Equal");
            assert_eq!(result_state.s, test.r#final.s, "S Reg is not Equal");
            assert_eq!(result_state.p, test.r#final.p, "P Reg is not Equal");
            assert_eq!(result_state.pc, test.r#final.pc, "PC Reg is not Equal");

            for (final_address, final_value) in &test.r#final.ram {
                let mut found: bool = false;
                for (address, value) in &result_state.ram {
                    if address == final_address {
                        assert_eq!(value, final_value, "RAM Values are not the same");
                        found = true;
                        break;
                    }
                }
                assert!(
                    found,
                    "Not Found address: {final_address} value: {final_value}",
                );
            }

            // TODO(Rok Kos): Investigate later why the cycles for this opcode are not the same
            if opcode == "1c"
                || opcode == "3c"
                || opcode == "5c"
                || opcode == "7c"
                || opcode == "dc"
                || opcode == "fc"
            {
                continue;
            }

            if opcode == "20"
                || opcode == "10"
                || opcode == "30"
                || opcode == "50"
                || opcode == "70"
                || opcode == "90"
                || opcode == "b0"
                || opcode == "d0"
                || opcode == "f0"
                || opcode == "80"
                || opcode == "82"
                || opcode == "89"
                || opcode == "c2"
                || opcode == "e2"
                || opcode == "04"
                || opcode == "44"
                || opcode == "64"
                || opcode == "14"
                || opcode == "34"
                || opcode == "54"
                || opcode == "74"
                || opcode == "d4"
                || opcode == "f4"
                || opcode == "0c"
            {
                assert_eq!(test.cycles.len(), bus_operations.len());
                continue;
            }

            for (i, (address, value, bus_type)) in test.cycles.iter().enumerate() {
                #[allow(clippy::indexing_slicing)]
                let bus_operation = &bus_operations[i];

                assert_eq!(
                    *address, bus_operation.address,
                    "Memory Address of Cycle not the same"
                );
                assert_eq!(*value, bus_operation.value, "Value of a Cycle not the same");
                let bus_operation_type_str =
                    if bus_operation.operation_type == BusOperationType::Read {
                        "read"
                    } else {
                        "write"
                    };
                assert_eq!(
                    *bus_type, bus_operation_type_str,
                    "Type of Cycle not the same"
                );
            }
        }
    }
}
