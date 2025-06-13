#![deny(dead_code, unsafe_code, unused_must_use, clippy::all, clippy::nursery)]
#![allow(
    clippy::panic,
    clippy::missing_docs_in_private_items,
    clippy::option_if_let_else
)]

use serde::Deserialize;
use std::{fs::File, path::Path};

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

#[derive(Debug, PartialEq, Eq)]
pub enum BusOperationType {
    Read = 0,
    Write = 1,
}

#[derive(Debug)]
struct BusOperation {
    address: u16,
    value: u8,
    operation_type: BusOperationType,
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

    fn run_op(&mut self) -> Vec<BusOperation> {
        let mut bus_operations: Vec<BusOperation> = Vec::new();

        let read_operand: BusOperation = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);
        let operand = read_operand.value;
        bus_operations.push(read_operand);

        match operand {
            // LDA
            0xa9 => {
                let (value, read_address) = self.addressing_immediate();
                self.pc = self.pc.wrapping_add(1);
                bus_operations.push(read_address);

                Self::register_load(&mut self.a, &mut self.p, value);
            }

            0xa5 => {
                let (value, mut read_operations) = self.addressing_zeropage();
                self.pc = self.pc.wrapping_add(1);
                bus_operations.append(&mut read_operations);
                Self::register_load(&mut self.a, &mut self.p, value);
            }

            0xb5 => {
                let (value, mut read_operations) = self.addressing_zeropage_indexed(self.x);
                self.pc = self.pc.wrapping_add(1);
                bus_operations.append(&mut read_operations);

                Self::register_load(&mut self.a, &mut self.p, value);
            }
            0xad => {
                let (value, mut read_operations) = self.addressing_absolute();
                bus_operations.append(&mut read_operations);

                Self::register_load(&mut self.a, &mut self.p, value);
            }

            0xbd => {
                let (value, mut read_operations) = self.addressing_absolute_indexed(self.x);
                bus_operations.append(&mut read_operations);
                Self::register_load(&mut self.a, &mut self.p, value);
            }

            0xb9 => {
                let (value, mut read_operations) = self.addressing_absolute_indexed(self.y);
                bus_operations.append(&mut read_operations);
                Self::register_load(&mut self.a, &mut self.p, value);
            }

            0xa1 => {
                let (value, mut read_operations) = self.addressing_indexed_indirect(self.x);
                bus_operations.append(&mut read_operations);
                self.pc = self.pc.wrapping_add(1);
                Self::register_load(&mut self.a, &mut self.p, value);
            }
            0xb1 => {
                let (value, mut read_operations) = self.addressing_indirect_indexed(self.y);
                bus_operations.append(&mut read_operations);
                self.pc = self.pc.wrapping_add(1);
                Self::register_load(&mut self.a, &mut self.p, value);
            }

            // LDX
            0xa2 => {
                let (value, read_address) = self.addressing_immediate();
                bus_operations.push(read_address);
                self.pc = self.pc.wrapping_add(1);
                Self::register_load(&mut self.x, &mut self.p, value);
            }
            0xa6 => {
                let (value, mut read_operations) = self.addressing_zeropage();
                self.pc = self.pc.wrapping_add(1);
                bus_operations.append(&mut read_operations);
                Self::register_load(&mut self.x, &mut self.p, value);
            }
            0xb6 => {
                let (value, mut read_operations) = self.addressing_zeropage_indexed(self.y);
                self.pc = self.pc.wrapping_add(1);
                bus_operations.append(&mut read_operations);
                Self::register_load(&mut self.x, &mut self.p, value);
            }
            0xae => {
                let (value, mut read_operations) = self.addressing_absolute();
                bus_operations.append(&mut read_operations);
                Self::register_load(&mut self.x, &mut self.p, value);
            }
            0xbe => {
                let (value, mut read_operations) = self.addressing_absolute_indexed(self.y);
                bus_operations.append(&mut read_operations);
                Self::register_load(&mut self.x, &mut self.p, value);
            }

            _ => {
                panic!("OP Not Implemented");
            }
        };

        bus_operations
    }

    const fn bus_read(&self, address: u16) -> BusOperation {
        BusOperation {
            address: address,
            value: self.ram[address as usize],
            operation_type: BusOperationType::Read,
        }
    }

    const fn addressing_immediate(&self) -> (u8, BusOperation) {
        let read_address = self.bus_read(self.pc);
        (read_address.value, read_address)
    }

    fn addressing_zeropage(&self) -> (u8, Vec<BusOperation>) {
        let read_operand = self.bus_read(self.pc);
        let read_address = self.bus_read(read_operand.value.into());

        (read_address.value, vec![read_operand, read_address])
    }

    fn addressing_zeropage_indexed(&self, register: u8) -> (u8, Vec<BusOperation>) {
        let read_operand = self.bus_read(self.pc);
        // Note(Rok Kos): This is a dummy read into zero page, before being indexed
        let read_zeropage = self.bus_read(read_operand.value.into());
        let zero_page_index = read_operand.value.wrapping_add(register);
        let read_address = self.bus_read(zero_page_index.into());

        (
            read_address.value,
            vec![read_operand, read_zeropage, read_address],
        )
    }
    fn addressing_absolute(&mut self) -> (u8, Vec<BusOperation>) {
        let read_operand_low = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);
        let read_operand_high = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        let operand_low: u16 = read_operand_low.value.into();
        let operand_high: u16 = read_operand_high.value.into();
        let address_absolute: u16 = (operand_high << 8) | operand_low;

        let read_address = self.bus_read(address_absolute);

        (
            read_address.value,
            vec![read_operand_low, read_operand_high, read_address],
        )
    }

    fn addressing_absolute_indexed(&mut self, register: u8) -> (u8, Vec<BusOperation>) {
        let read_operand_low = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        let read_operand_high = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        let mut operand_high: u16 = read_operand_high.value.into();
        if let Some(o) = read_operand_low.value.checked_add(register) {
            let operand_low: u16 = o.into();
            let address_absolute: u16 = (operand_high << 8) | operand_low;

            let read_address = self.bus_read(address_absolute);

            (
                read_address.value,
                vec![read_operand_low, read_operand_high, read_address],
            )
        } else {
            let operand_low: u16 = read_operand_low.value.wrapping_add(register).into();

            let address_absolute: u16 = (operand_high << 8) | operand_low;
            let miss_read_address = self.bus_read(address_absolute);

            operand_high = read_operand_high.value.wrapping_add(1).into();
            let address_absolute: u16 = (operand_high << 8) | operand_low;
            let read_address = self.bus_read(address_absolute);
            (
                read_address.value,
                vec![
                    read_operand_low,
                    read_operand_high,
                    miss_read_address,
                    read_address,
                ],
            )
        }
    }
    fn addressing_indexed_indirect(&self, register: u8) -> (u8, Vec<BusOperation>) {
        let read_operand = self.bus_read(self.pc);
        let miss_read_operand = self.bus_read(read_operand.value.into());

        let mut peek: u8 = read_operand.value.wrapping_add(register);
        let read_address_low = self.bus_read(peek.into());

        peek = peek.wrapping_add(1);
        let read_address_high = self.bus_read(peek.into());

        let address_low: u16 = read_address_low.value.into();
        let address_high: u16 = read_address_high.value.into();
        let address: u16 = (address_high << 8) | address_low;

        let read_address = self.bus_read(address);

        (
            read_address.value,
            vec![
                read_operand,
                miss_read_operand,
                read_address_low,
                read_address_high,
                read_address,
            ],
        )
    }

    fn addressing_indirect_indexed(&self, register: u8) -> (u8, Vec<BusOperation>) {
        let read_operand = self.bus_read(self.pc);

        let read_address_low = self.bus_read(read_operand.value.into());

        let read_address_high: BusOperation =
            if let Some(address_operand_high) = read_operand.value.checked_add(1) {
                self.bus_read(address_operand_high.into())
            } else {
                let address_operand_high = read_operand.value.wrapping_add(1);
                self.bus_read(address_operand_high.into())
            };

        if let Some(address_low) = read_address_low.value.checked_add(register) {
            let address_low: u16 = address_low.into();
            let address_high: u16 = read_address_high.value.into();

            let address: u16 = (address_high << 8) | address_low;
            let read_address = self.bus_read(address);

            (
                read_address.value,
                vec![
                    read_operand,
                    read_address_low,
                    read_address_high,
                    read_address,
                ],
            )
        } else {
            const CARRY: u16 = 1 << 8;
            let address_low: u16 = read_address_low.value.wrapping_add(register).into();
            let address_high: u16 = read_address_high.value.into();

            let miss_address: u16 = (address_high << 8) | address_low;
            let miss_read_address = self.bus_read(miss_address);

            let address: u16 = miss_address.wrapping_add(CARRY);
            let read_address = self.bus_read(address);

            (
                read_address.value,
                vec![
                    read_operand,
                    read_address_low,
                    read_address_high,
                    miss_read_address,
                    read_address,
                ],
            )
        }
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
            assert!(
                address < MEMORY_SIZE as usize,
                "Address out of bound of 64KiB"
            );
            self.ram[address] = value;
        }
    }
    fn debug_state_get(&self, final_state: &TestNES6502State) -> TestNES6502State {
        let mut ram_values: Vec<(usize, u8)> = Vec::new();

        for (address, _) in &final_state.ram {
            let address = *address;
            assert!(
                address < MEMORY_SIZE as usize,
                "Address out of bound of 64KiB"
            );
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
    let opcode_to_test: Vec<&str> = vec![
        "be", "ae", "b6", "a6", "b1", "a9", "a2", "a5", "b5", "ad", "bd", "b9", "a1",
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

            for (i, (address, value, bus_type)) in test.cycles.iter().enumerate() {
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
