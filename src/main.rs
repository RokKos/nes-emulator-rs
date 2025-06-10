#![deny(unsafe_code, unused_must_use, clippy::pedantic, clippy::nursery)]
#![allow(dead_code)]

use serde::Deserialize;
use std::{fmt, fs::File, path::Path};

const KIB: u32 = 1024;
const MEMORY_SIZE: u32 = 64 * KIB;

#[derive(Debug, PartialEq, Eq)]
pub enum StatusFlag {
    Carry = 1 << 0,
    Zero = 1 << 1,
    InterruptDisable = 1 << 2,
    DecimalMode = 1 << 3,
    Break = 1 << 4,
    Overflow = 1 << 6,
    Negative = 1 << 7,
}

struct Chip6502 {
    a: u8,
    x: u8,
    y: u8,
    s: u8,
    p: u8,
    pc: u16,
    ram: [u8; MEMORY_SIZE as usize],
}

impl Chip6502 {
    const fn power_up() -> Self {
        Self {
            a: 0,
            x: 0,
            y: 0,
            pc: 0xFFC,
            s: 0xFD,
            p: 0b0010_0100,
            ram: [0; MEMORY_SIZE as usize],
        }
    }

    fn run_op(&mut self) -> u8 {
        let op = self.ram[self.pc as usize];
        self.pc = self.pc.wrapping_add(1);
        let cycles: u8 = match op {
            0xa9 => {
                let value = self.ram[self.pc as usize];
                Self::register_load(&mut self.a, &mut self.p, value);

                2
            }

            0xa5 => {
                let oper = self.ram[self.pc as usize];
                let value = self.ram[oper as usize];
                Self::register_load(&mut self.a, &mut self.p, value);

                3
            }

            0xb5 => {
                let oper = self.ram[self.pc as usize];
                let address = self.x.wrapping_add(oper);
                let value = self.ram[address as usize];

                Self::register_load(&mut self.a, &mut self.p, value);

                4
            }
            0xad => {
                let oper_low: u16 = self.ram[self.pc as usize].into();
                self.pc = self.pc.wrapping_add(1);
                let oper_high: u16 = self.ram[self.pc as usize].into();
                let address_absolute: u16 = (oper_high << 8) | oper_low;
                let value = self.ram[address_absolute as usize];

                Self::register_load(&mut self.a, &mut self.p, value);

                4
            }

            0xbd => {
                let oper_low: u8 = self.ram[self.pc as usize];
                self.pc = self.pc.wrapping_add(1);
                let mut oper_high: u16 = self.ram[self.pc as usize].into();

                let mut cycles: u8 = 4;
                let oper_low: u16 = match oper_low.checked_add(self.x) {
                    Some(o) => o.into(),
                    None => {
                        cycles += 1;
                        oper_high = oper_high.wrapping_add(1);
                        oper_low.wrapping_add(self.x).into()
                    }
                };

                let address_absolute: u16 = (oper_high << 8) | oper_low;
                let value = self.ram[address_absolute as usize];

                Self::register_load(&mut self.a, &mut self.p, value);

                cycles
            }

            0xb9 => {
                let oper_low: u8 = self.ram[self.pc as usize];
                self.pc = self.pc.wrapping_add(1);
                let mut oper_high: u16 = self.ram[self.pc as usize].into();

                let mut cycles: u8 = 4;
                let oper_low: u16 = match oper_low.checked_add(self.y) {
                    Some(o) => o.into(),
                    None => {
                        cycles += 1;
                        oper_high = oper_high.wrapping_add(1);
                        oper_low.wrapping_add(self.y).into()
                    }
                };

                let address_absolute: u16 = (oper_high << 8) | oper_low;
                let value = self.ram[address_absolute as usize];

                Self::register_load(&mut self.a, &mut self.p, value);

                cycles
            }

            0xa2 => {
                let value = self.ram[self.pc as usize];
                Self::register_load(&mut self.x, &mut self.p, value);
                2
            }

            _ => {
                println!("OP Not Implemented");
                0
            }
        };

        self.pc = self.pc.wrapping_add(1);
        cycles
    }

    fn register_load(register_target: &mut u8, register_flag: &mut u8, value: u8) {
        *register_target = value;
        Self::register_flag_set(register_flag, StatusFlag::Zero, value == 0);
        let is_negative = (value & StatusFlag::Negative as u8) != 0;
        Self::register_flag_set(register_flag, StatusFlag::Negative, is_negative);
    }

    fn register_flag_set(reg: &mut u8, flag: StatusFlag, value: bool) {
        if value {
            *reg |= flag as u8;
        } else {
            *reg &= !(flag as u8);
        }
    }

    fn debug_state_set(&mut self, test_state: TestNES6502State) {
        self.a = test_state.a;
        self.x = test_state.x;
        self.y = test_state.y;
        self.s = test_state.s;
        self.p = test_state.p;
        self.pc = test_state.pc;

        for (address, value) in test_state.ram {
            assert!(address < MEMORY_SIZE as usize);
            self.ram[address] = value;
        }
    }
    fn debug_state_get(&self, final_state: &TestNES6502State) -> TestNES6502State {
        let mut ram_values: Vec<(usize, u8)> = Vec::new();

        for (address, _) in &final_state.ram {
            let address = *address;
            assert!(address < MEMORY_SIZE as usize);
            let value = self.ram[address];
            ram_values.push((address, value));
        }

        TestNES6502State {
            a: self.a,
            x: self.x,
            y: self.y,
            s: self.s,
            p: self.p,
            pc: self.pc,
            ram: ram_values,
        }
    }
}

impl fmt::Display for Chip6502 {
    /// Formats the Chip6502 struct for display.
    /// The output will be:
    ///
    /// ```text
    /// ┌────────────────────────┐
    /// │ 6502 CPU State         │
    /// ├────────────────────────┤
    /// │ PC: $8000 (32768)      │
    /// │ A:  $42   (66)         │
    /// │ X:  $0F   (15)         │
    /// │ Y:  $FF   (255)        │
    /// │ S:  $FA   (250)        │
    /// │ P:  %00110101 (NV-BDIZC)│
    /// └────────────────────────┘
    /// ```
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "┌─────────────────────────┐")?;
        writeln!(f, "│ 6502 CPU State          │")?;
        writeln!(f, "├─────────────────────────┤")?;
        writeln!(f, "│ PC: ${:04X} ({})        │", self.pc, self.pc)?;
        writeln!(f, "│ A:  ${:02X}   ({})           │", self.a, self.a)?;
        writeln!(f, "│ X:  ${:02X}   ({})           │", self.x, self.x)?;
        writeln!(f, "│ Y:  ${:02X}   ({})           │", self.y, self.y)?;
        writeln!(f, "│ S:  ${:02X}   ({})         │", self.s, self.s)?;
        writeln!(f, "│ P:  %{:08b} (NV-BDIZC)│", self.p)?;
        write!(f, "└─────────────────────────┘")
    }
}

#[derive(Debug, Deserialize)]
struct TestNES6502State {
    a: u8,
    x: u8,
    y: u8,
    s: u8,
    p: u8,
    pc: u16,
    ram: Vec<(usize, u8)>,
}

#[derive(Debug, Deserialize)]
struct TestNES6502 {
    name: String,
    initial: TestNES6502State,
    r#final: TestNES6502State,
    cycles: Vec<(u16, u8, String)>,
}

fn main() {
    let opcode_to_test: Vec<&str> = vec!["a9", "a2", "a5", "b5", "ad", "bd", "b9"];
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
            let cycles = chip.run_op();
            let result_state = chip.debug_state_get(&test.r#final);

            println!("{0}", test.name);
            assert_eq!(result_state.a, test.r#final.a, "A Reg is not Equal");
            assert_eq!(result_state.x, test.r#final.x, "X Reg is not Equal");
            assert_eq!(result_state.y, test.r#final.y, "Y Reg is not Equal");
            assert_eq!(result_state.s, test.r#final.s, "S Reg is not Equal");
            assert_eq!(result_state.p, test.r#final.p, "P Reg is not Equal");
            assert_eq!(result_state.pc, test.r#final.pc, "PC Reg is not Equal");
            assert_eq!(
                cycles as usize,
                test.cycles.len(),
                "Cycles Count doesn't match"
            );

            for (final_address, final_value) in &test.r#final.ram {
                let mut found: bool = false;
                for (address, value) in &result_state.ram {
                    if address == final_address {
                        assert_eq!(value, final_value);
                        found = true;
                        break;
                    }
                }
                assert!(
                    found,
                    "Not Found address: {final_address} value: {final_value}",
                );
            }
            assert_eq!(result_state.p, test.r#final.p);
        }
    }
}
