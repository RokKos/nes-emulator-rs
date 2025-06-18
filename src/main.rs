#![deny(dead_code, unsafe_code, unused_must_use, clippy::all, clippy::nursery)]
#![allow(
    clippy::panic,
    clippy::missing_docs_in_private_items,
    clippy::option_if_let_else,
    clippy::too_many_lines,
    clippy::enum_glob_use,
    clippy::unreachable,
    clippy::unnecessary_wraps
)]

use serde::Deserialize;
use std::{fs::File, path::Path};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Register {
    A,
    X,
    Y,
    S,
}

#[derive(Debug, Clone, Copy)]
enum AddressingMode {
    Implied,
    Immediate,
    ZeroPage,
    ZeroPageIndexed(Register),
    Absolute,
    AbsoluteIndexed(Register),
    Indirect,
    IndirectIndexed(Register),
    IndexedIndirect(Register),
    Relative,
}

#[rustfmt::skip]
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    // Official Opcodes
    ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC,
    CLD, CLI, CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP,
    JSR, LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA, PLP, ROL, ROR, RTI,
    RTS, SBC, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA,

    // Unofficial/Illegal Opcodes
    ALR, ANC, ANE, ARR, DCP, ISC, LAS, LAX, LXA, RLA, RRA, SAX, SBX, SHA,
    SHX, SHY, SLO, SRE, TAS, USBC,

    // System/Misc
    KIL, // also JAM, HLT
    XXX, // Represents a truly unknown or unprocessed opcode
}

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    opcode: Opcode,
    adressing_mode: AddressingMode,
    // NOTE(Rok Kos): this is the least amount of cycles, we have this so that we
    // can insert the dummy read or writes. Because every cycle in NES is bus operation
    cycle_count: u8,
    format: &'static str,
}

impl Instruction {
    const fn new(
        opcode: Opcode,
        adressing_mode: AddressingMode,
        cycle_count: u8,
        format: &'static str,
    ) -> Self {
        Self {
            opcode,
            adressing_mode,
            cycle_count,
            format,
        }
    }
}

const INSTRUCTION_XXX: Instruction =
    Instruction::new(Opcode::XXX, AddressingMode::Absolute, 1, "XXX");

const KIB: u32 = 1024;
const MEMORY_SIZE: u32 = 64 * KIB;
const INSTRUCTION_COUNT: u16 = 256;

struct Chip6502 {
    a: u8,
    x: u8,
    y: u8,
    s: u8,
    p: u8,
    pc: u16,
    ram: [u8; MEMORY_SIZE as usize],
    instruction_table: [Instruction; INSTRUCTION_COUNT as usize],
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
            instruction_table: Self::build_instruction_table(),
        }
    }

    const fn build_instruction_table() -> [Instruction; 256] {
        use AddressingMode::*;
        use Opcode::*;
        use Register::*;
        // Helper alias for the constructor to keep lines even shorter
        let i = Instruction::new;

        let mut table: [Instruction; 256] = [INSTRUCTION_XXX; 256];

        // --- Load/Store Operations ---

        // LDA - Load Accumulator
        table[0xA9] = i(LDA, Immediate, 2, "LDA #{}");
        table[0xA5] = i(LDA, ZeroPage, 3, "LDA {}");
        table[0xB5] = i(LDA, ZeroPageIndexed(X), 4, "LDA {},X");
        table[0xAD] = i(LDA, Absolute, 4, "LDA {}");
        table[0xBD] = i(LDA, AbsoluteIndexed(X), 4, "LDA {},X"); // +1 if page crossed
        table[0xB9] = i(LDA, AbsoluteIndexed(Y), 4, "LDA {},Y"); // +1 if page crossed
        table[0xA1] = i(LDA, IndexedIndirect(X), 6, "LDA ({},X)");
        table[0xB1] = i(LDA, IndirectIndexed(Y), 5, "LDA ({}),Y"); // +1 if page crossed

        // LDX - Load X Register
        table[0xA2] = i(LDX, Immediate, 2, "LDX #{}");
        table[0xA6] = i(LDX, ZeroPage, 3, "LDX {}");
        table[0xB6] = i(LDX, ZeroPageIndexed(Y), 4, "LDX {},Y");
        table[0xAE] = i(LDX, Absolute, 4, "LDX {}");
        table[0xBE] = i(LDX, AbsoluteIndexed(Y), 4, "LDX {},Y"); // +1 if page crossed

        // LDY - Load Y Register
        table[0xA0] = i(LDY, Immediate, 2, "LDY #{}");
        table[0xA4] = i(LDY, ZeroPage, 3, "LDY {}");
        table[0xB4] = i(LDY, ZeroPageIndexed(X), 4, "LDY {},X");
        table[0xAC] = i(LDY, Absolute, 4, "LDY {}");
        table[0xBC] = i(LDY, AbsoluteIndexed(X), 4, "LDY {},X"); // +1 if page crossed

        // STA - Store Accumulator
        table[0x85] = i(STA, ZeroPage, 3, "STA {}");
        table[0x95] = i(STA, ZeroPageIndexed(X), 4, "STA {},X");
        table[0x8D] = i(STA, Absolute, 4, "STA {}");
        table[0x9D] = i(STA, AbsoluteIndexed(X), 5, "STA {},X");
        table[0x99] = i(STA, AbsoluteIndexed(Y), 5, "STA {},Y");
        table[0x81] = i(STA, IndexedIndirect(X), 6, "STA ({},X)");
        table[0x91] = i(STA, IndirectIndexed(Y), 6, "STA ({}),Y");

        // STX - Store X Register
        table[0x86] = i(STX, ZeroPage, 3, "STX {}");
        table[0x96] = i(STX, ZeroPageIndexed(Y), 4, "STX {},Y");
        table[0x8E] = i(STX, Absolute, 4, "STX {}");

        // STY - Store Y Register
        table[0x84] = i(STY, ZeroPage, 3, "STY {}");
        table[0x94] = i(STY, ZeroPageIndexed(X), 4, "STY {},X");
        table[0x8C] = i(STY, Absolute, 4, "STY {}");
        // --- Arithmetic and Logical Operations ---

        // ADC - Add with Carry
        table[0x69] = i(ADC, Immediate, 2, "ADC #{}");
        table[0x65] = i(ADC, ZeroPage, 3, "ADC {}");
        table[0x75] = i(ADC, ZeroPageIndexed(X), 4, "ADC {},X");
        table[0x6D] = i(ADC, Absolute, 4, "ADC {}");
        table[0x7D] = i(ADC, AbsoluteIndexed(X), 4, "ADC {},X"); // +1 if page crossed
        table[0x79] = i(ADC, AbsoluteIndexed(Y), 4, "ADC {},Y"); // +1 if page crossed
        table[0x61] = i(ADC, IndexedIndirect(X), 6, "ADC ({},X)");
        table[0x71] = i(ADC, IndirectIndexed(Y), 5, "ADC ({}),Y"); // +1 if page crossed

        // SBC - Subtract with Carry
        table[0xE9] = i(SBC, Immediate, 2, "SBC #{}");
        table[0xE5] = i(SBC, ZeroPage, 3, "SBC {}");
        table[0xF5] = i(SBC, ZeroPageIndexed(X), 4, "SBC {},X");
        table[0xED] = i(SBC, Absolute, 4, "SBC {}");
        table[0xFD] = i(SBC, AbsoluteIndexed(X), 4, "SBC {},X"); // +1 if page crossed
        table[0xF9] = i(SBC, AbsoluteIndexed(Y), 4, "SBC {},Y"); // +1 if page crossed
        table[0xE1] = i(SBC, IndexedIndirect(X), 6, "SBC ({},X)");
        table[0xF1] = i(SBC, IndirectIndexed(Y), 5, "SBC ({}),Y"); // +1 if page crossed

        // AND - Logical AND
        table[0x29] = i(AND, Immediate, 2, "AND #{}");
        table[0x25] = i(AND, ZeroPage, 3, "AND {}");
        table[0x35] = i(AND, ZeroPageIndexed(X), 4, "AND {},X");
        table[0x2D] = i(AND, Absolute, 4, "AND {}");
        table[0x3D] = i(AND, AbsoluteIndexed(X), 4, "AND {},X"); // +1 if page crossed
        table[0x39] = i(AND, AbsoluteIndexed(Y), 4, "AND {},Y"); // +1 if page crossed
        table[0x21] = i(AND, IndexedIndirect(X), 6, "AND ({},X)");
        table[0x31] = i(AND, IndirectIndexed(Y), 5, "AND ({}),Y"); // +1 if page crossed

        // ORA - Logical Inclusive OR
        table[0x09] = i(ORA, Immediate, 2, "ORA #{}");
        table[0x05] = i(ORA, ZeroPage, 3, "ORA {}");
        table[0x15] = i(ORA, ZeroPageIndexed(X), 4, "ORA {},X");
        table[0x0D] = i(ORA, Absolute, 4, "ORA {}");
        table[0x1D] = i(ORA, AbsoluteIndexed(X), 4, "ORA {},X"); // +1 if page crossed
        table[0x19] = i(ORA, AbsoluteIndexed(Y), 4, "ORA {},Y"); // +1 if page crossed
        table[0x01] = i(ORA, IndexedIndirect(X), 6, "ORA ({},X)");
        table[0x11] = i(ORA, IndirectIndexed(Y), 5, "ORA ({}),Y"); // +1 if page crossed

        // EOR - Logical Exclusive OR
        table[0x49] = i(EOR, Immediate, 2, "EOR #{}");
        table[0x45] = i(EOR, ZeroPage, 3, "EOR {}");
        table[0x55] = i(EOR, ZeroPageIndexed(X), 4, "EOR {},X");
        table[0x4D] = i(EOR, Absolute, 4, "EOR {}");
        table[0x5D] = i(EOR, AbsoluteIndexed(X), 4, "EOR {},X"); // +1 if page crossed
        table[0x59] = i(EOR, AbsoluteIndexed(Y), 4, "EOR {},Y"); // +1 if page crossed
        table[0x41] = i(EOR, IndexedIndirect(X), 6, "EOR ({},X)");
        table[0x51] = i(EOR, IndirectIndexed(Y), 5, "EOR ({}),Y"); // +1 if page crossed

        // --- Compare Operations ---

        // CMP - Compare Accumulator
        table[0xC9] = i(CMP, Immediate, 2, "CMP #{}");
        table[0xC5] = i(CMP, ZeroPage, 3, "CMP {}");
        table[0xD5] = i(CMP, ZeroPageIndexed(X), 4, "CMP {},X");
        table[0xCD] = i(CMP, Absolute, 4, "CMP {}");
        table[0xDD] = i(CMP, AbsoluteIndexed(X), 4, "CMP {},X"); // +1 if page crossed
        table[0xD9] = i(CMP, AbsoluteIndexed(Y), 4, "CMP {},Y"); // +1 if page crossed
        table[0xC1] = i(CMP, IndexedIndirect(X), 6, "CMP ({},X)");
        table[0xD1] = i(CMP, IndirectIndexed(Y), 5, "CMP ({}),Y"); // +1 if page crossed

        // CPX - Compare X Register
        table[0xE0] = i(CPX, Immediate, 2, "CPX #{}");
        table[0xE4] = i(CPX, ZeroPage, 3, "CPX {}");
        table[0xEC] = i(CPX, Absolute, 4, "CPX {}");

        // CPY - Compare Y Register
        table[0xC0] = i(CPY, Immediate, 2, "CPY #{}");
        table[0xC4] = i(CPY, ZeroPage, 3, "CPY {}");
        table[0xCC] = i(CPY, Absolute, 4, "CPY {}");
        /*
            // BIT - Bit Test
            table[0x24] = i(BIT, ZeroPage, 3, "BIT {}");
            table[0x2C] = i(BIT, Absolute, 4, "BIT {}");

            // --- Shift and Rotate Operations ---

            // ASL - Arithmetic Shift Left
            table[0x0A] = i(ASL, Accumulator, 2, "ASL A");
            table[0x06] = i(ASL, ZeroPage, 5, "ASL {}");
            table[0x16] = i(ASL, ZeroPageIndexed(X), 6, "ASL {},X");
            table[0x0E] = i(ASL, Absolute, 6, "ASL {}");
            table[0x1E] = i(ASL, AbsoluteIndexed(X), 7, "ASL {},X");

            // LSR - Logical Shift Right
            table[0x4A] = i(LSR, Accumulator, 2, "LSR A");
            table[0x46] = i(LSR, ZeroPage, 5, "LSR {}");
            table[0x56] = i(LSR, ZeroPageIndexed(X), 6, "LSR {},X");
            table[0x4E] = i(LSR, Absolute, 6, "LSR {}");
            table[0x5E] = i(LSR, AbsoluteIndexed(X), 7, "LSR {},X");

            // ROL - Rotate Left
            table[0x2A] = i(ROL, Accumulator, 2, "ROL A");
            table[0x26] = i(ROL, ZeroPage, 5, "ROL {}");
            table[0x36] = i(ROL, ZeroPageIndexed(X), 6, "ROL {},X");
            table[0x2E] = i(ROL, Absolute, 6, "ROL {}");
            table[0x3E] = i(ROL, AbsoluteIndexed(X), 7, "ROL {},X");

            // ROR - Rotate Right
            table[0x6A] = i(ROR, Accumulator, 2, "ROR A");
            table[0x66] = i(ROR, ZeroPage, 5, "ROR {}");
            table[0x76] = i(ROR, ZeroPageIndexed(X), 6, "ROR {},X");
            table[0x6E] = i(ROR, Absolute, 6, "ROR {}");
            table[0x7E] = i(ROR, AbsoluteIndexed(X), 7, "ROR {},X");

            // --- Increment and Decrement Operations ---

            // INC - Increment Memory
            table[0xE6] = i(INC, ZeroPage, 5, "INC {}");
            table[0xF6] = i(INC, ZeroPageIndexed(X), 6, "INC {},X");
            table[0xEE] = i(INC, Absolute, 6, "INC {}");
            table[0xFE] = i(INC, AbsoluteIndexed(X), 7, "INC {},X");

            // DEC - Decrement Memory
            table[0xC6] = i(DEC, ZeroPage, 5, "DEC {}");
            table[0xD6] = i(DEC, ZeroPageIndexed(X), 6, "DEC {},X");
            table[0xCE] = i(DEC, Absolute, 6, "DEC {}");
            table[0xDE] = i(DEC, AbsoluteIndexed(X), 7, "DEC {},X");

            // INX, INY, DEX, DEY
            table[0xE8] = i(INX, Implied, 2, "INX");
            table[0xC8] = i(INY, Implied, 2, "INY");
            table[0xCA] = i(DEX, Implied, 2, "DEX");
            table[0x88] = i(DEY, Implied, 2, "DEY");

        */
        // --- Branch Operations --- (cycles are for branch not taken)
        table[0x10] = i(BPL, Relative, 2, "BPL {}"); // +1 if taken, +2 if page crossed
        table[0x30] = i(BMI, Relative, 2, "BMI {}"); // +1 if taken, +2 if page crossed
        table[0x50] = i(BVC, Relative, 2, "BVC {}"); // +1 if taken, +2 if page crossed
        table[0x70] = i(BVS, Relative, 2, "BVS {}"); // +1 if taken, +2 if page crossed
        table[0x90] = i(BCC, Relative, 2, "BCC {}"); // +1 if taken, +2 if page crossed
        table[0xB0] = i(BCS, Relative, 2, "BCS {}"); // +1 if taken, +2 if page crossed
        table[0xD0] = i(BNE, Relative, 2, "BNE {}"); // +1 if taken, +2 if page crossed
        table[0xF0] = i(BEQ, Relative, 2, "BEQ {}"); // +1 if taken, +2 if page crossed

        // --- Jump and Subroutine Operations ---
        table[0x4C] = i(JMP, Absolute, 3, "JMP {}");
        table[0x6C] = i(JMP, Indirect, 5, "JMP ({})");
        table[0x20] = i(JSR, Absolute, 6, "JSR {}");
        table[0x60] = i(RTS, Implied, 6, "RTS");
        table[0x40] = i(RTI, Implied, 6, "RTI");

        // --- Register Transfer Operations ---
        table[0xAA] = i(TAX, Implied, 2, "TAX");
        table[0x8A] = i(TXA, Implied, 2, "TXA");
        table[0xA8] = i(TAY, Implied, 2, "TAY");
        table[0x98] = i(TYA, Implied, 2, "TYA");
        table[0xBA] = i(TSX, Implied, 2, "TSX");
        table[0x9A] = i(TXS, Implied, 2, "TXS");
        /*
            // --- Stack Operations ---
            table[0x48] = i(PHA, Implied, 3, "PHA");
            table[0x68] = i(PLA, Implied, 4, "PLA");
            table[0x08] = i(PHP, Implied, 3, "PHP");
            table[0x28] = i(PLP, Implied, 4, "PLP");

        */
        // --- Status Flag Operations ---
        table[0x18] = i(CLC, Implied, 2, "CLC");
        table[0x38] = i(SEC, Implied, 2, "SEC");
        table[0xB8] = i(CLV, Implied, 2, "CLV");
        table[0xD8] = i(CLD, Implied, 2, "CLD");
        table[0xF8] = i(SED, Implied, 2, "SED");
        /*
            table[0x58] = i(CLI, Implied, 2, "CLI");
            table[0x78] = i(SEI, Implied, 2, "SEI");

            // --- System and NOP ---
            table[0x00] = i(BRK, Implied, 7, "BRK");
            table[0xEA] = i(NOP, Implied, 2, "NOP");

        // --- Illegal and Undocumented Opcodes ---

        table[0xEB] = i(USBC, Immediate, 2, "SBC #{}");

        // SLO (ASL + ORA)
        table[0x07] = i(SLO, ZeroPage, 5, "SLO {}");
        table[0x17] = i(SLO, ZeroPageIndexed(X), 6, "SLO {},X");
        table[0x0F] = i(SLO, Absolute, 6, "SLO {}");
        table[0x1F] = i(SLO, AbsoluteIndexed(X), 7, "SLO {},X");
        table[0x1B] = i(SLO, AbsoluteIndexed(Y), 7, "SLO {},Y");
        table[0x03] = i(SLO, IndexedIndirect, 8, "SLO ({},X)");
        table[0x13] = i(SLO, IndirectIndexed, 8, "SLO ({}),Y");

        // RLA (ROL + AND)
        table[0x27] = i(RLA, ZeroPage, 5, "RLA {}");
        table[0x37] = i(RLA, ZeroPageIndexed(X), 6, "RLA {},X");
        table[0x2F] = i(RLA, Absolute, 6, "RLA {}");
        table[0x3F] = i(RLA, AbsoluteIndexed(X), 7, "RLA {},X");
        table[0x3B] = i(RLA, AbsoluteIndexed(Y), 7, "RLA {},Y");
        table[0x23] = i(RLA, IndexedIndirect, 8, "RLA ({},X)");
        table[0x33] = i(RLA, IndirectIndexed, 8, "RLA ({}),Y");

        // SRE (LSR + EOR)
        table[0x47] = i(SRE, ZeroPage, 5, "SRE {}");
        table[0x57] = i(SRE, ZeroPageIndexed(X), 6, "SRE {},X");
        table[0x4F] = i(SRE, Absolute, 6, "SRE {}");
        table[0x5F] = i(SRE, AbsoluteIndexed(X), 7, "SRE {},X");
        table[0x5B] = i(SRE, AbsoluteIndexed(Y), 7, "SRE {},Y");
        table[0x43] = i(SRE, IndexedIndirect, 8, "SRE ({},X)");
        table[0x53] = i(SRE, IndirectIndexed, 8, "SRE ({}),Y");

        // RRA (ROR + ADC)
        table[0x67] = i(RRA, ZeroPage, 5, "RRA {}");
        table[0x77] = i(RRA, ZeroPageIndexed(X), 6, "RRA {},X");
        table[0x6F] = i(RRA, Absolute, 6, "RRA {}");
        table[0x7F] = i(RRA, AbsoluteIndexed(X), 7, "RRA {},X");
        table[0x7B] = i(RRA, AbsoluteIndexed(Y), 7, "RRA {},Y");
        table[0x63] = i(RRA, IndexedIndirect, 8, "RRA ({},X)");
        table[0x73] = i(RRA, IndirectIndexed, 8, "RRA ({}),Y");

        // SAX (Store A&X)
        table[0x87] = i(SAX, ZeroPage, 3, "SAX {}");
        table[0x97] = i(SAX, ZeroPageIndexed(Y), 4, "SAX {},Y");
        table[0x8F] = i(SAX, Absolute, 4, "SAX {}");
        table[0x83] = i(SAX, IndexedIndirect, 6, "SAX ({},X)");

        // LAX (LDA + LDX)
        table[0xA7] = i(LAX, ZeroPage, 3, "LAX {}");
        table[0xB7] = i(LAX, ZeroPageIndexed(Y), 4, "LAX {},Y");
        table[0xAF] = i(LAX, Absolute, 4, "LAX {}");
        table[0xBF] = i(LAX, AbsoluteIndexed(Y), 4, "LAX {},Y");
        table[0xA3] = i(LAX, IndexedIndirect, 6, "LAX ({},X)");
        table[0xB3] = i(LAX, IndirectIndexed, 5, "LAX ({}),Y");

        // DCP (DEC + CMP)
        table[0xC7] = i(DCP, ZeroPage, 5, "DCP {}");
        table[0xD7] = i(DCP, ZeroPageIndexed(X), 6, "DCP {},X");
        table[0xCF] = i(DCP, Absolute, 6, "DCP {}");
        table[0xDF] = i(DCP, AbsoluteIndexed(X), 7, "DCP {},X");
        table[0xDB] = i(DCP, AbsoluteIndexed(Y), 7, "DCP {},Y");
        table[0xC3] = i(DCP, IndexedIndirect, 8, "DCP ({},X)");
        table[0xD3] = i(DCP, IndirectIndexed, 8, "DCP ({}),Y");

        // ISC (INC + SBC)
        table[0xE7] = i(ISC, ZeroPage, 5, "ISC {}");
        table[0xF7] = i(ISC, ZeroPageIndexed(X), 6, "ISC {},X");
        table[0xEF] = i(ISC, Absolute, 6, "ISC {}");
        table[0xFF] = i(ISC, AbsoluteIndexed(X), 7, "ISC {},X");
        table[0xFB] = i(ISC, AbsoluteIndexed(Y), 7, "ISC {},Y");
        table[0xE3] = i(ISC, IndexedIndirect, 8, "ISC ({},X)");
        table[0xF3] = i(ISC, IndirectIndexed, 8, "ISC ({}),Y");

        // Misc Illegal Opcodes
        table[0x4B] = i(ALR, Immediate, 2, "ALR #{}");
        table[0x0B] = i(ANC, Immediate, 2, "ANC #{}");
        table[0x2B] = i(ANC, Immediate, 2, "ANC #{}");
        table[0x6B] = i(ARR, Immediate, 2, "ARR #{}");
        table[0xCB] = i(SBX, Immediate, 2, "SBX #{}");

        // Illegal NOPs
        table[0x1A] = i(NOP, Implied, 2, "NOP");
        table[0x3A] = i(NOP, Implied, 2, "NOP");
        table[0x5A] = i(NOP, Implied, 2, "NOP");
        table[0x7A] = i(NOP, Implied, 2, "NOP");
        table[0xDA] = i(NOP, Implied, 2, "NOP");
        table[0xFA] = i(NOP, Implied, 2, "NOP");
        table[0x80] = i(NOP, Immediate, 2, "NOP #{}");
        table[0x82] = i(NOP, Immediate, 2, "NOP #{}");
        table[0x89] = i(NOP, Immediate, 2, "NOP #{}");
        table[0xC2] = i(NOP, Immediate, 2, "NOP #{}");
        table[0xE2] = i(NOP, Immediate, 2, "NOP #{}");
        table[0x04] = i(NOP, ZeroPage, 3, "NOP {}");
        table[0x44] = i(NOP, ZeroPage, 3, "NOP {}");
        table[0x64] = i(NOP, ZeroPage, 3, "NOP {}");
        table[0x14] = i(NOP, ZeroPageIndexed(X), 4, "NOP {},X");
        table[0x34] = i(NOP, ZeroPageIndexed(X), 4, "NOP {},X");
        table[0x54] = i(NOP, ZeroPageIndexed(X), 4, "NOP {},X");
        table[0x74] = i(NOP, ZeroPageIndexed(X), 4, "NOP {},X");
        table[0xD4] = i(NOP, ZeroPageIndexed(X), 4, "NOP {},X");
        table[0xF4] = i(NOP, ZeroPageIndexed(X), 4, "NOP {},X");
        table[0x0C] = i(NOP, Absolute, 4, "NOP {}");
        table[0x1C] = i(NOP, AbsoluteIndexed(X), 4, "NOP {},X");
        table[0x3C] = i(NOP, AbsoluteIndexed(X), 4, "NOP {},X");
        table[0x5C] = i(NOP, AbsoluteIndexed(X), 4, "NOP {},X");
        table[0x7C] = i(NOP, AbsoluteIndexed(X), 4, "NOP {},X");
        table[0xDC] = i(NOP, AbsoluteIndexed(X), 4, "NOP {},X");
        table[0xFC] = i(NOP, AbsoluteIndexed(X), 4, "NOP {},X");

        // KIL (JAM/HLT)
        table[0x02] = i(KIL, Implied, 2, "KIL");
        table[0x12] = i(KIL, Implied, 2, "KIL");
        table[0x22] = i(KIL, Implied, 2, "KIL");
        table[0x32] = i(KIL, Implied, 2, "KIL");
        table[0x42] = i(KIL, Implied, 2, "KIL");
        table[0x52] = i(KIL, Implied, 2, "KIL");
        table[0x62] = i(KIL, Implied, 2, "KIL");
        table[0x72] = i(KIL, Implied, 2, "KIL");
        table[0x92] = i(KIL, Implied, 2, "KIL");
        table[0xB2] = i(KIL, Implied, 2, "KIL");
        table[0xD2] = i(KIL, Implied, 2, "KIL");
        table[0xF2] = i(KIL, Implied, 2, "KIL");
            */
        table
    }

    fn run_op(&mut self) -> Vec<BusOperation> {
        use AddressingMode::*;
        use Opcode::*;
        use Register::*;

        let mut bus_operations: Vec<BusOperation> = Vec::new();

        let read_operand: BusOperation = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);
        let operand = read_operand.value;
        bus_operations.push(read_operand);

        let instruction: Instruction = self.instruction_table[operand as usize];

        let (address, mut read_operations) = match instruction.adressing_mode {
            Implied => self.addressing_implied(instruction.cycle_count),
            Immediate => self.addressing_immediate(),
            Relative => self.addressing_relative(),
            ZeroPage => self.addressing_zeropage(),
            ZeroPageIndexed(X) => self.addressing_zeropage_indexed(self.x),
            ZeroPageIndexed(Y) => self.addressing_zeropage_indexed(self.y),
            Absolute => self.addressing_absolute(),
            AbsoluteIndexed(X) => self.addressing_absolute_indexed(self.x),
            AbsoluteIndexed(Y) => self.addressing_absolute_indexed(self.y),
            IndexedIndirect(X) => self.addressing_indexed_indirect(self.x),
            IndexedIndirect(Y) => self.addressing_indexed_indirect(self.y),
            Indirect => self.addressing_indirect(),
            IndirectIndexed(X) => self.addressing_indirect_indexed(self.x),
            IndirectIndexed(Y) => self.addressing_indirect_indexed(self.y),
            ZeroPageIndexed(A | S)
            | AbsoluteIndexed(A | S)
            | IndirectIndexed(A | S)
            | IndexedIndirect(A | S) => {
                unreachable!("This kind of addressing mode is not possible")
            }
        };

        bus_operations.append(&mut read_operations);

        let dummy_read = self.bus_read(address);

        let opcode_operation = match instruction.opcode {
            LDA => self.register_load(A, address),
            LDX => self.register_load(X, address),
            LDY => self.register_load(Y, address),
            STA => self.register_save(A, address),
            STX => self.register_save(X, address),
            STY => self.register_save(Y, address),
            TAX => self.register_transfer(A, X),
            TXA => self.register_transfer(X, A),
            TAY => self.register_transfer(A, Y),
            TYA => self.register_transfer(Y, A),
            TSX => self.register_transfer(S, X),
            TXS => self.register_transfer(X, S),
            AND => self.and(address),
            ORA => self.or(address),
            EOR => self.eor(address),
            ADC => self.adc(address),
            SBC => self.sbc(address),
            CMP => self.register_compare(A, address),
            CPX => self.register_compare(X, address),
            CPY => self.register_compare(Y, address),
            CLC => self.flag_clear(StatusFlag::Carry),
            CLD => self.flag_clear(StatusFlag::DecimalMode),
            CLV => self.flag_clear(StatusFlag::Overflow),
            SEC => self.flag_set(StatusFlag::Carry),
            SED => self.flag_set(StatusFlag::DecimalMode),
            JMP => self.jump(address),
            // TODO(Rok Kos): implmemen
            _ => {
                todo!("Opcode not implemented");
            }
        };

        if instruction.cycle_count as usize > bus_operations.len().wrapping_add(1) {
            // NOTE(Rok Kos): Because every cycle in NES is bus operation, we insert dummy read if cycle
            // count is not at least cycle_count
            bus_operations.push(dummy_read);
        }

        if let Some(opcode_operation_value) = opcode_operation {
            let instruction_display = instruction
                .format
                .replace("{}", &opcode_operation_value.value.to_string());
            println!("{instruction_display }");

            bus_operations.push(opcode_operation_value);
        } else {
            println!("{}", instruction.format);
        }

        bus_operations
    }

    const fn bus_read(&self, address: u16) -> BusOperation {
        BusOperation {
            address,
            value: self.ram[address as usize],
            operation_type: BusOperationType::Read,
        }
    }

    fn bus_write(&mut self, address: u16, value: u8) -> BusOperation {
        self.ram[address as usize] = value;
        BusOperation {
            address,
            value,
            operation_type: BusOperationType::Write,
        }
    }

    fn addressing_immediate(&mut self) -> (u16, Vec<BusOperation>) {
        let address = self.pc;
        self.pc = self.pc.wrapping_add(1);
        (address, vec![])
    }

    fn addressing_relative(&mut self) -> (u16, Vec<BusOperation>) {
        let address = self.pc;
        self.pc = self.pc.wrapping_add(1);
        (address, vec![])
    }

    fn addressing_implied(&self, cycles: u8) -> (u16, Vec<BusOperation>) {
        let address = self.pc;
        let mut bus_operations: Vec<BusOperation> = Vec::with_capacity(cycles as usize);
        for _ in 0..cycles {
            bus_operations.push(self.bus_read(address));
        }

        (address, bus_operations)
    }

    fn addressing_zeropage(&mut self) -> (u16, Vec<BusOperation>) {
        let read_address = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        (read_address.value.into(), vec![read_address])
    }

    fn addressing_zeropage_indexed(&mut self, register: u8) -> (u16, Vec<BusOperation>) {
        let read_operand = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);
        // Note(Rok Kos): This is a dummy read into zero page, before being indexed
        let read_zeropage = self.bus_read(read_operand.value.into());
        let zero_page_index = read_operand.value.wrapping_add(register);

        (zero_page_index.into(), vec![read_operand, read_zeropage])
    }
    fn addressing_absolute(&mut self) -> (u16, Vec<BusOperation>) {
        let read_operand_low = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);
        let read_operand_high = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        let operand_low: u16 = read_operand_low.value.into();
        let operand_high: u16 = read_operand_high.value.into();
        let address_absolute: u16 = (operand_high << 8) | operand_low;

        (address_absolute, vec![read_operand_low, read_operand_high])
    }

    fn addressing_absolute_indexed(&mut self, register: u8) -> (u16, Vec<BusOperation>) {
        let read_operand_low = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        let read_operand_high = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        let mut operand_high: u16 = read_operand_high.value.into();
        if let Some(o) = read_operand_low.value.checked_add(register) {
            let operand_low: u16 = o.into();
            let address_absolute: u16 = (operand_high << 8) | operand_low;

            (address_absolute, vec![read_operand_low, read_operand_high])
        } else {
            let operand_low: u16 = read_operand_low.value.wrapping_add(register).into();

            let address_absolute: u16 = (operand_high << 8) | operand_low;
            let miss_read_address = self.bus_read(address_absolute);

            operand_high = read_operand_high.value.wrapping_add(1).into();
            let address_absolute: u16 = (operand_high << 8) | operand_low;
            (
                address_absolute,
                vec![read_operand_low, read_operand_high, miss_read_address],
            )
        }
    }
    fn addressing_indexed_indirect(&mut self, register: u8) -> (u16, Vec<BusOperation>) {
        let read_operand = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);
        let miss_read_operand = self.bus_read(read_operand.value.into());

        let mut peek: u8 = read_operand.value.wrapping_add(register);
        let read_address_low = self.bus_read(peek.into());

        peek = peek.wrapping_add(1);
        let read_address_high = self.bus_read(peek.into());

        let address_low: u16 = read_address_low.value.into();
        let address_high: u16 = read_address_high.value.into();
        let address: u16 = (address_high << 8) | address_low;

        (
            address,
            vec![
                read_operand,
                miss_read_operand,
                read_address_low,
                read_address_high,
            ],
        )
    }
    fn addressing_indirect(&mut self) -> (u16, Vec<BusOperation>) {
        let read_operand_low = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        let read_operand_high = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

        let operand_low: u16 = read_operand_low.value.into();
        let operand_high: u16 = read_operand_high.value.into();
        let address_low_pointer: u16 = (operand_high << 8) | operand_low;

        let read_address_low = self.bus_read(address_low_pointer);

        let operand_low: u16 = read_operand_low.value.wrapping_add(1).into();
        let operand_high: u16 = read_operand_high.value.into();
        let address_high_pointer: u16 = (operand_high << 8) | operand_low;

        let read_address_high = self.bus_read(address_high_pointer);

        let address_low: u16 = read_address_low.value.into();
        let address_high: u16 = read_address_high.value.into();

        let address: u16 = (address_high << 8) | address_low;

        (
            address,
            vec![
                read_operand_low,
                read_operand_high,
                read_address_low,
                read_address_high,
            ],
        )
    }

    fn addressing_indirect_indexed(&mut self, register: u8) -> (u16, Vec<BusOperation>) {
        let read_operand = self.bus_read(self.pc);
        self.pc = self.pc.wrapping_add(1);

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

            (
                address,
                vec![read_operand, read_address_low, read_address_high],
            )
        } else {
            const CARRY: u16 = 1 << 8;
            let address_low: u16 = read_address_low.value.wrapping_add(register).into();
            let address_high: u16 = read_address_high.value.into();

            let miss_address: u16 = (address_high << 8) | address_low;
            let miss_read_address = self.bus_read(miss_address);

            let address: u16 = miss_address.wrapping_add(CARRY);

            (
                address,
                vec![
                    read_operand,
                    read_address_low,
                    read_address_high,
                    miss_read_address,
                ],
            )
        }
    }
    fn jump(&mut self, address: u16) -> Option<BusOperation> {
        self.pc = address;
        None
    }
    fn register_compare(&mut self, register: Register, address: u16) -> Option<BusOperation> {
        let read_address = self.bus_read(address);

        let value = match register {
            Register::A => self.a,
            Register::X => self.x,
            Register::Y => self.y,
            Register::S => unreachable!("Cannot compare S register to memory"),
        };

        Self::register_flag_set(&mut self.p, StatusFlag::Carry, value >= read_address.value);
        Self::register_flag_set(&mut self.p, StatusFlag::Zero, value == read_address.value);

        let result = value.wrapping_sub(read_address.value);
        let is_negative = (result & StatusFlag::Negative as u8) != 0;
        Self::register_flag_set(&mut self.p, StatusFlag::Negative, is_negative);

        Some(read_address)
    }

    fn sbc(&mut self, address: u16) -> Option<BusOperation> {
        let read_address = self.bus_read(address);

        let (add_1, carry_1) = self.a.overflowing_sub(read_address.value);
        let carry: u8 = u8::from((self.p & StatusFlag::Carry as u8) != 1); // Note(Rok Kos): we
                                                                           // need to invert cary
        let (result, carry_2) = add_1.overflowing_sub(carry);

        let is_overflow = (0x80 & (result ^ self.a) & (result ^ !read_address.value)) == 0x80;
        Self::register_flag_set(&mut self.p, StatusFlag::Overflow, is_overflow);

        self.a = result;

        let has_cary = carry_1 || carry_2;
        Self::register_flag_set(&mut self.p, StatusFlag::Carry, !has_cary);

        Self::register_flag_set(&mut self.p, StatusFlag::Zero, self.a == 0);
        let is_negative = (self.a & StatusFlag::Negative as u8) != 0;
        Self::register_flag_set(&mut self.p, StatusFlag::Negative, is_negative);

        Some(read_address)
    }

    fn adc(&mut self, address: u16) -> Option<BusOperation> {
        let read_address = self.bus_read(address);

        let (add_1, carry_1) = self.a.overflowing_add(read_address.value);
        let carry: u8 = self.p & StatusFlag::Carry as u8;
        let (result, carry_2) = add_1.overflowing_add(carry);

        let is_overflow = (0x80 & (result ^ self.a) & (result ^ read_address.value)) == 0x80;
        Self::register_flag_set(&mut self.p, StatusFlag::Overflow, is_overflow);

        self.a = result;

        let has_cary = carry_1 || carry_2;
        Self::register_flag_set(&mut self.p, StatusFlag::Carry, has_cary);

        Self::register_flag_set(&mut self.p, StatusFlag::Zero, self.a == 0);
        let is_negative = (self.a & StatusFlag::Negative as u8) != 0;
        Self::register_flag_set(&mut self.p, StatusFlag::Negative, is_negative);

        Some(read_address)
    }

    // TODO(Rok Kos): I can maybe all have in a match case
    fn eor(&mut self, address: u16) -> Option<BusOperation> {
        let read_address = self.bus_read(address);
        self.a ^= read_address.value;
        let value = self.a;

        Self::register_flag_set(&mut self.p, StatusFlag::Zero, value == 0);
        let is_negative = (value & StatusFlag::Negative as u8) != 0;
        Self::register_flag_set(&mut self.p, StatusFlag::Negative, is_negative);

        Some(read_address)
    }

    fn or(&mut self, address: u16) -> Option<BusOperation> {
        let read_address = self.bus_read(address);
        self.a |= read_address.value;
        let value = self.a;

        Self::register_flag_set(&mut self.p, StatusFlag::Zero, value == 0);
        let is_negative = (value & StatusFlag::Negative as u8) != 0;
        Self::register_flag_set(&mut self.p, StatusFlag::Negative, is_negative);

        Some(read_address)
    }

    fn and(&mut self, address: u16) -> Option<BusOperation> {
        let read_address = self.bus_read(address);
        self.a &= read_address.value;
        let value = self.a;

        Self::register_flag_set(&mut self.p, StatusFlag::Zero, value == 0);
        let is_negative = (value & StatusFlag::Negative as u8) != 0;
        Self::register_flag_set(&mut self.p, StatusFlag::Negative, is_negative);

        Some(read_address)
    }

    fn register_load(&mut self, register: Register, address: u16) -> Option<BusOperation> {
        let read_address = self.bus_read(address);
        let value = read_address.value;

        match register {
            Register::A => self.a = value,
            Register::X => self.x = value,
            Register::Y => self.y = value,
            Register::S => self.s = value,
        };

        Self::register_flag_set(&mut self.p, StatusFlag::Zero, value == 0);
        let is_negative = (value & StatusFlag::Negative as u8) != 0;
        Self::register_flag_set(&mut self.p, StatusFlag::Negative, is_negative);

        Some(read_address)
    }

    fn register_transfer(&mut self, from: Register, to: Register) -> Option<BusOperation> {
        let value = match from {
            Register::A => self.a,
            Register::X => self.x,
            Register::Y => self.y,
            Register::S => self.s,
        };

        match to {
            Register::A => self.a = value,
            Register::X => self.x = value,
            Register::Y => self.y = value,
            Register::S => self.s = value,
        };

        dbg!(value);

        if to != Register::S {
            Self::register_flag_set(&mut self.p, StatusFlag::Zero, value == 0);
            let is_negative = (value & StatusFlag::Negative as u8) != 0;
            Self::register_flag_set(&mut self.p, StatusFlag::Negative, is_negative);
        }

        None
    }

    fn register_save(&mut self, register: Register, address: u16) -> Option<BusOperation> {
        let bus_operation = match register {
            Register::A => self.bus_write(address, self.a),
            Register::X => self.bus_write(address, self.x),
            Register::Y => self.bus_write(address, self.y),
            Register::S => self.bus_write(address, self.s),
        };

        Some(bus_operation)
    }
    fn flag_set(&mut self, flag: StatusFlag) -> Option<BusOperation> {
        match flag {
            StatusFlag::Carry | StatusFlag::DecimalMode => {
                Self::register_flag_set(&mut self.p, flag, true);
            }
            StatusFlag::Zero
            | StatusFlag::Overflow
            | StatusFlag::InterruptDisable
            | StatusFlag::Negative
            | StatusFlag::Break => {
                unreachable!("Cannot clear this flag")
            }
        };

        None
        // Some(bus_operation)
    }
    fn flag_clear(&mut self, flag: StatusFlag) -> Option<BusOperation> {
        match flag {
            StatusFlag::Carry | StatusFlag::DecimalMode | StatusFlag::Overflow => {
                Self::register_flag_set(&mut self.p, flag, false);
            }
            StatusFlag::Zero
            | StatusFlag::InterruptDisable
            | StatusFlag::Negative
            | StatusFlag::Break => {
                unreachable!("Cannot clear this flag")
            }
        };

        None
        // Some(bus_operation)
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
        "4c", "6c", "18", "38", "b8", "d8", "f8", "c9", "c5", "d5", "cd", "dd", "d9", "c1", "d1",
        "e0", "e4", "ec", "c0", "c4", "cc", "69", "65", "75", "6d", "7d", "79", "61", "71", "e9",
        "e5", "f5", "ed", "fd", "f9", "e1", "f1", "49", "45", "55", "4d", "5d", "59", "41", "51",
        "29", "25", "35", "2d", "3d", "39", "21", "31", "09", "05", "15", "0d", "1d", "19", "01",
        "11", "aa", "8a", "a8", "98", "ba", "9a", "9d", "85", "a0", "a4", "b4", "ac", "bc", "95",
        "8d", "99", "81", "91", "86", "96", "8e", "84", "94", "8c", "bc", "ac", "b4", "a4", "a0",
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
