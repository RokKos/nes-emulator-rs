#![deny(dead_code, unsafe_code, unused_must_use, clippy::all, clippy::nursery)]
#![allow(
    clippy::panic,
    clippy::missing_docs_in_private_items,
    clippy::option_if_let_else,
    clippy::too_many_lines,
    clippy::enum_glob_use
)]

use serde::Deserialize;
use serde_json::value;
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

#[derive(Debug, Clone, Copy)]
enum Register {
    A,
    X,
    Y,
}

#[derive(Debug, Clone, Copy)]
enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPageIndexed(Register),
    Absolute,
    AbsoluteIndexed(Register),
    IndirectIndexed(Register),
    IndexedIndirect(Register),
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
    format: &'static str,
}

impl Instruction {
    const fn new(opcode: Opcode, adressing_mode: AddressingMode, format: &'static str) -> Self {
        Self {
            opcode,
            adressing_mode,
            format,
        }
    }
}

const INSTRUCTION_XXX: Instruction = Instruction::new(Opcode::XXX, AddressingMode::Absolute, "XXX");

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
        table[0xA9] = i(LDA, Immediate, "LDA #{}");
        table[0xA5] = i(LDA, ZeroPage, "LDA {}");
        table[0xB5] = i(LDA, ZeroPageIndexed(X), "LDA {},X");
        table[0xAD] = i(LDA, Absolute, "LDA {}");
        table[0xBD] = i(LDA, AbsoluteIndexed(X), "LDA {},X");
        table[0xB9] = i(LDA, AbsoluteIndexed(Y), "LDA {},Y");
        table[0xA1] = i(LDA, IndexedIndirect(X), "LDA ({},X)");
        table[0xB1] = i(LDA, IndirectIndexed(Y), "LDA ({}),Y");

        // LDX - Load X Register
        table[0xA2] = i(LDX, Immediate, "LDX #{}");
        table[0xA6] = i(LDX, ZeroPage, "LDX {}");
        table[0xB6] = i(LDX, ZeroPageIndexed(Y), "LDX {},Y");
        table[0xAE] = i(LDX, Absolute, "LDX {}");
        table[0xBE] = i(LDX, AbsoluteIndexed(Y), "LDX {},Y");

        // LDY - Load Y Register
        table[0xA0] = i(LDY, Immediate, "LDY #{}");
        table[0xA4] = i(LDY, ZeroPage, "LDY {}");
        table[0xB4] = i(LDY, ZeroPageIndexed(X), "LDY {},X");
        table[0xAC] = i(LDY, Absolute, "LDY {}");
        table[0xBC] = i(LDY, AbsoluteIndexed(X), "LDY {},X");
        // STA - Store Accumulator
        table[0x85] = i(STA, ZeroPage, "STA {}");
        table[0x95] = i(STA, ZeroPageIndexed(X), "STA {},X");
        table[0x8D] = i(STA, Absolute, "STA {}");
        table[0x9D] = i(STA, AbsoluteIndexed(X), "STA {},X");
        table[0x99] = i(STA, AbsoluteIndexed(Y), "STA {},Y");
        table[0x81] = i(STA, IndexedIndirect(X), "STA ({},X)");
        table[0x91] = i(STA, IndirectIndexed(Y), "STA ({}),Y");

        // STX - Store X Register
        table[0x86] = i(STX, ZeroPage, "STX {}");
        table[0x96] = i(STX, ZeroPageIndexed(Y), "STX {},Y");
        table[0x8E] = i(STX, Absolute, "STX {}");

        // STY - Store Y Register
        table[0x84] = i(STY, ZeroPage, "STY {}");
        table[0x94] = i(STY, ZeroPageIndexed(X), "STY {},X");
        table[0x8C] = i(STY, Absolute, "STY {}");

        /*
                // --- Arithmetic and Logical Operations ---

                // ADC - Add with Carry
                table[0x69] = i(ADC, Immediate, "ADC #{}");
                table[0x65] = i(ADC, ZeroPage, "ADC {}");
                table[0x75] = i(ADC, ZeroPageIndexed(X), "ADC {},X");
                table[0x6D] = i(ADC, Absolute, "ADC {}");
                table[0x7D] = i(ADC, AbsoluteIndexed(X), "ADC {},X");
                table[0x79] = i(ADC, AbsoluteIndexed(Y), "ADC {},Y");
                table[0x61] = i(ADC, IndexedIndirect, "ADC ({},X)");
                table[0x71] = i(ADC, IndirectIndexed, "ADC ({}),Y");

                // SBC - Subtract with Carry
                table[0xE9] = i(SBC, Immediate, "SBC #{}");
                table[0xE5] = i(SBC, ZeroPage, "SBC {}");
                table[0xF5] = i(SBC, ZeroPageIndexed(X), "SBC {},X");
                table[0xED] = i(SBC, Absolute, "SBC {}");
                table[0xFD] = i(SBC, AbsoluteIndexed(X), "SBC {},X");
                table[0xF9] = i(SBC, AbsoluteIndexed(Y), "SBC {},Y");
                table[0xE1] = i(SBC, IndexedIndirect, "SBC ({},X)");
                table[0xF1] = i(SBC, IndirectIndexed, "SBC ({}),Y");
                table[0xEB] = i(USBC, Immediate, "SBC #{}"); // unofficial

                // AND - Logical AND
                table[0x29] = i(AND, Immediate, "AND #{}");
                table[0x25] = i(AND, ZeroPage, "AND {}");
                table[0x35] = i(AND, ZeroPageIndexed(X), "AND {},X");
                table[0x2D] = i(AND, Absolute, "AND {}");
                table[0x3D] = i(AND, AbsoluteIndexed(X), "AND {},X");
                table[0x39] = i(AND, AbsoluteIndexed(Y), "AND {},Y");
                table[0x21] = i(AND, IndexedIndirect, "AND ({},X)");
                table[0x31] = i(AND, IndirectIndexed, "AND ({}),Y");

                // ORA - Logical Inclusive OR
                table[0x09] = i(ORA, Immediate, "ORA #{}");
                table[0x05] = i(ORA, ZeroPage, "ORA {}");
                table[0x15] = i(ORA, ZeroPageIndexed(X), "ORA {},X");
                table[0x0D] = i(ORA, Absolute, "ORA {}");
                table[0x1D] = i(ORA, AbsoluteIndexed(X), "ORA {},X");
                table[0x19] = i(ORA, AbsoluteIndexed(Y), "ORA {},Y");
                table[0x01] = i(ORA, IndexedIndirect, "ORA ({},X)");
                table[0x11] = i(ORA, IndirectIndexed, "ORA ({}),Y");

                // EOR - Logical Exclusive OR
                table[0x49] = i(EOR, Immediate, "EOR #{}");
                table[0x45] = i(EOR, ZeroPage, "EOR {}");
                table[0x55] = i(EOR, ZeroPageIndexed(X), "EOR {},X");
                table[0x4D] = i(EOR, Absolute, "EOR {}");
                table[0x5D] = i(EOR, AbsoluteIndexed(X), "EOR {},X");
                table[0x59] = i(EOR, AbsoluteIndexed(Y), "EOR {},Y");
                table[0x41] = i(EOR, IndexedIndirect, "EOR ({},X)");
                table[0x51] = i(EOR, IndirectIndexed, "EOR ({}),Y");

                // --- Compare Operations ---

                // CMP - Compare Accumulator
                table[0xC9] = i(CMP, Immediate, "CMP #{}");
                table[0xC5] = i(CMP, ZeroPage, "CMP {}");
                table[0xD5] = i(CMP, ZeroPageIndexed(X), "CMP {},X");
                table[0xCD] = i(CMP, Absolute, "CMP {}");
                table[0xDD] = i(CMP, AbsoluteIndexed(X), "CMP {},X");
                table[0xD9] = i(CMP, AbsoluteIndexed(Y), "CMP {},Y");
                table[0xC1] = i(CMP, IndexedIndirect, "CMP ({},X)");
                table[0xD1] = i(CMP, IndirectIndexed, "CMP ({}),Y");

                // CPX - Compare X Register
                table[0xE0] = i(CPX, Immediate, "CPX #{}");
                table[0xE4] = i(CPX, ZeroPage, "CPX {}");
                table[0xEC] = i(CPX, Absolute, "CPX {}");

                // CPY - Compare Y Register
                table[0xC0] = i(CPY, Immediate, "CPY #{}");
                table[0xC4] = i(CPY, ZeroPage, "CPY {}");
                table[0xCC] = i(CPY, Absolute, "CPY {}");

                // BIT - Bit Test
                table[0x24] = i(BIT, ZeroPage, "BIT {}");
                table[0x2C] = i(BIT, Absolute, "BIT {}");

                // --- Shift and Rotate Operations ---

                // ASL - Arithmetic Shift Left
                table[0x0A] = i(ASL, Accumulator, "ASL A");
                table[0x06] = i(ASL, ZeroPage, "ASL {}");
                table[0x16] = i(ASL, ZeroPageIndexed(X), "ASL {},X");
                table[0x0E] = i(ASL, Absolute, "ASL {}");
                table[0x1E] = i(ASL, AbsoluteIndexed(X), "ASL {},X");

                // LSR - Logical Shift Right
                table[0x4A] = i(LSR, Accumulator, "LSR A");
                table[0x46] = i(LSR, ZeroPage, "LSR {}");
                table[0x56] = i(LSR, ZeroPageIndexed(X), "LSR {},X");
                table[0x4E] = i(LSR, Absolute, "LSR {}");
                table[0x5E] = i(LSR, AbsoluteIndexed(X), "LSR {},X");

                // ROL - Rotate Left
                table[0x2A] = i(ROL, Accumulator, "ROL A");
                table[0x26] = i(ROL, ZeroPage, "ROL {}");
                table[0x36] = i(ROL, ZeroPageIndexed(X), "ROL {},X");
                table[0x2E] = i(ROL, Absolute, "ROL {}");
                table[0x3E] = i(ROL, AbsoluteIndexed(X), "ROL {},X");

                // ROR - Rotate Right
                table[0x6A] = i(ROR, Accumulator, "ROR A");
                table[0x66] = i(ROR, ZeroPage, "ROR {}");
                table[0x76] = i(ROR, ZeroPageIndexed(X), "ROR {},X");
                table[0x6E] = i(ROR, Absolute, "ROR {}");
                table[0x7E] = i(ROR, AbsoluteIndexed(X), "ROR {},X");

                // --- Increment and Decrement Operations ---

                // INC - Increment Memory
                table[0xE6] = i(INC, ZeroPage, "INC {}");
                table[0xF6] = i(INC, ZeroPageIndexed(X), "INC {},X");
                table[0xEE] = i(INC, Absolute, "INC {}");
                table[0xFE] = i(INC, AbsoluteIndexed(X), "INC {},X");

                // DEC - Decrement Memory
                table[0xC6] = i(DEC, ZeroPage, "DEC {}");
                table[0xD6] = i(DEC, ZeroPageIndexed(X), "DEC {},X");
                table[0xCE] = i(DEC, Absolute, "DEC {}");
                table[0xDE] = i(DEC, AbsoluteIndexed(X), "DEC {},X");

                // INX, INY, DEX, DEY
                table[0xE8] = i(INX, Implied, "INX");
                table[0xC8] = i(INY, Implied, "INY");
                table[0xCA] = i(DEX, Implied, "DEX");
                table[0x88] = i(DEY, Implied, "DEY");

                // --- Branch Operations ---
                table[0x10] = i(BPL, Relative, "BPL {}");
                table[0x30] = i(BMI, Relative, "BMI {}");
                table[0x50] = i(BVC, Relative, "BVC {}");
                table[0x70] = i(BVS, Relative, "BVS {}");
                table[0x90] = i(BCC, Relative, "BCC {}");
                table[0xB0] = i(BCS, Relative, "BCS {}");
                table[0xD0] = i(BNE, Relative, "BNE {}");
                table[0xF0] = i(BEQ, Relative, "BEQ {}");

                // --- Jump and Subroutine Operations ---
                table[0x4C] = i(JMP, Absolute, "JMP {}");
                table[0x6C] = i(JMP, Indirect, "JMP ({})");
                table[0x20] = i(JSR, Absolute, "JSR {}");
                table[0x60] = i(RTS, Implied, "RTS");
                table[0x40] = i(RTI, Implied, "RTI");

                // --- Register Transfer Operations ---
                table[0xAA] = i(TAX, Implied, "TAX");
                table[0x8A] = i(TXA, Implied, "TXA");
                table[0xA8] = i(TAY, Implied, "TAY");
                table[0x98] = i(TYA, Implied, "TYA");
                table[0xBA] = i(TSX, Implied, "TSX");
                table[0x9A] = i(TXS, Implied, "TXS");

                // --- Stack Operations ---
                table[0x48] = i(PHA, Implied, "PHA");
                table[0x68] = i(PLA, Implied, "PLA");
                table[0x08] = i(PHP, Implied, "PHP");
                table[0x28] = i(PLP, Implied, "PLP");

                // --- Status Flag Operations ---
                table[0x18] = i(CLC, Implied, "CLC");
                table[0x38] = i(SEC, Implied, "SEC");
                table[0x58] = i(CLI, Implied, "CLI");
                table[0x78] = i(SEI, Implied, "SEI");
                table[0xB8] = i(CLV, Implied, "CLV");
                table[0xD8] = i(CLD, Implied, "CLD");
                table[0xF8] = i(SED, Implied, "SED");

                // --- System and NOP ---
                table[0x00] = i(BRK, Implied, "BRK");
                table[0xEA] = i(NOP, Implied, "NOP");

                // --- Illegal and Undocumented Opcodes ---

                // SLO (ASL + ORA)
                table[0x07] = i(SLO, ZeroPage, "SLO {}");
                table[0x17] = i(SLO, ZeroPageIndexed(X), "SLO {},X");
                table[0x0F] = i(SLO, Absolute, "SLO {}");
                table[0x1F] = i(SLO, AbsoluteIndexed(X), "SLO {},X");
                table[0x1B] = i(SLO, AbsoluteIndexed(Y), "SLO {},Y");
                table[0x03] = i(SLO, IndexedIndirect, "SLO ({},X)");
                table[0x13] = i(SLO, IndirectIndexed, "SLO ({}),Y");

                // RLA (ROL + AND)
                table[0x27] = i(RLA, ZeroPage, "RLA {}");
                table[0x37] = i(RLA, ZeroPageIndexed(X), "RLA {},X");
                table[0x2F] = i(RLA, Absolute, "RLA {}");
                table[0x3F] = i(RLA, AbsoluteIndexed(X), "RLA {},X");
                table[0x3B] = i(RLA, AbsoluteIndexed(Y), "RLA {},Y");
                table[0x23] = i(RLA, IndexedIndirect, "RLA ({},X)");
                table[0x33] = i(RLA, IndirectIndexed, "RLA ({}),Y");

                // SRE (LSR + EOR)
                table[0x47] = i(SRE, ZeroPage, "SRE {}");
                table[0x57] = i(SRE, ZeroPageIndexed(X), "SRE {},X");
                table[0x4F] = i(SRE, Absolute, "SRE {}");
                table[0x5F] = i(SRE, AbsoluteIndexed(X), "SRE {},X");
                table[0x5B] = i(SRE, AbsoluteIndexed(Y), "SRE {},Y");
                table[0x43] = i(SRE, IndexedIndirect, "SRE ({},X)");
                table[0x53] = i(SRE, IndirectIndexed, "SRE ({}),Y");

                // RRA (ROR + ADC)
                table[0x67] = i(RRA, ZeroPage, "RRA {}");
                table[0x77] = i(RRA, ZeroPageIndexed(X), "RRA {},X");
                table[0x6F] = i(RRA, Absolute, "RRA {}");
                table[0x7F] = i(RRA, AbsoluteIndexed(X), "RRA {},X");
                table[0x7B] = i(RRA, AbsoluteIndexed(Y), "RRA {},Y");
                table[0x63] = i(RRA, IndexedIndirect, "RRA ({},X)");
                table[0x73] = i(RRA, IndirectIndexed, "RRA ({}),Y");

                // SAX (Store A&X)
                table[0x87] = i(SAX, ZeroPage, "SAX {}");
                table[0x97] = i(SAX, ZeroPageIndexed(Y), "SAX {},Y");
                table[0x8F] = i(SAX, Absolute, "SAX {}");
                table[0x83] = i(SAX, IndexedIndirect, "SAX ({},X)");

                // LAX (LDA + LDX)
                table[0xA7] = i(LAX, ZeroPage, "LAX {}");
                table[0xB7] = i(LAX, ZeroPageIndexed(Y), "LAX {},Y");
                table[0xAF] = i(LAX, Absolute, "LAX {}");
                table[0xBF] = i(LAX, AbsoluteIndexed(Y), "LAX {},Y");
                table[0xA3] = i(LAX, IndexedIndirect, "LAX ({},X)");
                table[0xB3] = i(LAX, IndirectIndexed, "LAX ({}),Y");

                // DCP (DEC + CMP)
                table[0xC7] = i(DCP, ZeroPage, "DCP {}");
                table[0xD7] = i(DCP, ZeroPageIndexed(X), "DCP {},X");
                table[0xCF] = i(DCP, Absolute, "DCP {}");
                table[0xDF] = i(DCP, AbsoluteIndexed(X), "DCP {},X");
                table[0xDB] = i(DCP, AbsoluteIndexed(Y), "DCP {},Y");
                table[0xC3] = i(DCP, IndexedIndirect, "DCP ({},X)");
                table[0xD3] = i(DCP, IndirectIndexed, "DCP ({}),Y");

                // ISC (INC + SBC)
                table[0xE7] = i(ISC, ZeroPage, "ISC {}");
                table[0xF7] = i(ISC, ZeroPageIndexed(X), "ISC {},X");
                table[0xEF] = i(ISC, Absolute, "ISC {}");
                table[0xFF] = i(ISC, AbsoluteIndexed(X), "ISC {},X");
                table[0xFB] = i(ISC, AbsoluteIndexed(Y), "ISC {},Y");
                table[0xE3] = i(ISC, IndexedIndirect, "ISC ({},X)");
                table[0xF3] = i(ISC, IndirectIndexed, "ISC ({}),Y");

                // Misc Illegal Opcodes
                table[0x0B] = i(ANC, Immediate, "ANC #{}");
                table[0x2B] = i(ANC, Immediate, "ANC #{}");
                table[0x4B] = i(ALR, Immediate, "ALR #{}");
                table[0x6B] = i(ARR, Immediate, "ARR #{}");
                table[0x8B] = i(ANE, Immediate, "ANE #{}");
                table[0xAB] = i(LXA, Immediate, "LXA #{}");
                table[0xCB] = i(SBX, Immediate, "SBX #{}");

                // Illegal NOPs
                table[0x1A] = i(NOP, Implied, "NOP");
                table[0x3A] = i(NOP, Implied, "NOP");
                table[0x5A] = i(NOP, Implied, "NOP");
                table[0x7A] = i(NOP, Implied, "NOP");
                table[0xDA] = i(NOP, Implied, "NOP");
                table[0xFA] = i(NOP, Implied, "NOP");
                table[0x80] = i(NOP, Immediate, "NOP #{}");
                table[0x82] = i(NOP, Immediate, "NOP #{}");
                table[0x89] = i(NOP, Immediate, "NOP #{}");
                table[0xC2] = i(NOP, Immediate, "NOP #{}");
                table[0xE2] = i(NOP, Immediate, "NOP #{}");
                table[0x04] = i(NOP, ZeroPage, "NOP {}");
                table[0x44] = i(NOP, ZeroPage, "NOP {}");
                table[0x64] = i(NOP, ZeroPage, "NOP {}");
                table[0x14] = i(NOP, ZeroPageIndexed(X), "NOP {},X");
                table[0x34] = i(NOP, ZeroPageIndexed(X), "NOP {},X");
                table[0x54] = i(NOP, ZeroPageIndexed(X), "NOP {},X");
                table[0x74] = i(NOP, ZeroPageIndexed(X), "NOP {},X");
                table[0xD4] = i(NOP, ZeroPageIndexed(X), "NOP {},X");
                table[0xF4] = i(NOP, ZeroPageIndexed(X), "NOP {},X");
                table[0x0C] = i(NOP, Absolute, "NOP {}");
                table[0x1C] = i(NOP, AbsoluteIndexed(X), "NOP {},X");
                table[0x3C] = i(NOP, AbsoluteIndexed(X), "NOP {},X");
                table[0x5C] = i(NOP, AbsoluteIndexed(X), "NOP {},X");
                table[0x7C] = i(NOP, AbsoluteIndexed(X), "NOP {},X");
                table[0xDC] = i(NOP, AbsoluteIndexed(X), "NOP {},X");
                table[0xFC] = i(NOP, AbsoluteIndexed(X), "NOP {},X");

                // KIL (JAM/HLT) - These halt the CPU
                table[0x02] = i(KIL, Implied, "KIL");
                table[0x12] = i(KIL, Implied, "KIL");
                table[0x22] = i(KIL, Implied, "KIL");
                table[0x32] = i(KIL, Implied, "KIL");
                table[0x42] = i(KIL, Implied, "KIL");
                table[0x52] = i(KIL, Implied, "KIL");
                table[0x62] = i(KIL, Implied, "KIL");
                table[0x72] = i(KIL, Implied, "KIL");
                table[0x92] = i(KIL, Implied, "KIL");
                table[0xB2] = i(KIL, Implied, "KIL");
                table[0xD2] = i(KIL, Implied, "KIL");
                table[0xF2] = i(KIL, Implied, "KIL");
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
            Immediate => self.addressing_immediate(),
            ZeroPage => self.addressing_zeropage(),
            ZeroPageIndexed(X) => self.addressing_zeropage_indexed(self.x),
            ZeroPageIndexed(Y) => self.addressing_zeropage_indexed(self.y),
            Absolute => self.addressing_absolute(),
            AbsoluteIndexed(X) => self.addressing_absolute_indexed(self.x),
            AbsoluteIndexed(Y) => self.addressing_absolute_indexed(self.y),
            IndexedIndirect(X) => self.addressing_indexed_indirect(self.x),
            IndexedIndirect(Y) => self.addressing_indexed_indirect(self.y),
            IndirectIndexed(X) => self.addressing_indirect_indexed(self.x),
            IndirectIndexed(Y) => self.addressing_indirect_indexed(self.y),
            ZeroPageIndexed(A) | AbsoluteIndexed(A) | IndirectIndexed(A) | IndexedIndirect(A) => {
                panic!("This kind of addressing mode is not possible");
            }
        };

        bus_operations.append(&mut read_operations);
        // TODO(Rok Kos): in the future we can aughment this to also output the value of registers
        // for better debugging
        let instruction_display = instruction.format.replace("{}", &address.to_string());
        println!("{instruction_display }");

        let opcode_operation = match instruction.opcode {
            LDA => self.register_load(A, address),
            LDX => self.register_load(X, address),
            LDY => self.register_load(Y, address),
            STA => self.register_save(A, address),
            STX => self.register_save(X, address),
            STY => self.register_save(Y, address),
            _ => {
                panic!("Opcode not implemented");
            }
        };

        bus_operations.push(opcode_operation);

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

    fn register_load(&mut self, register: Register, address: u16) -> BusOperation {
        let read_address = self.bus_read(address);
        let value = read_address.value;

        match register {
            Register::A => self.a = value,
            Register::X => self.x = value,
            Register::Y => self.y = value,
        };

        Self::register_flag_set(&mut self.p, StatusFlag::Zero, value == 0);
        let is_negative = (value & StatusFlag::Negative as u8) != 0;
        Self::register_flag_set(&mut self.p, StatusFlag::Negative, is_negative);

        read_address
    }

    fn register_save(&mut self, register: Register, address: u16) -> BusOperation {
        match register {
            Register::A => self.bus_write(address, self.a),
            Register::X => self.bus_write(address, self.x),
            Register::Y => self.bus_write(address, self.y),
        }
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
        "85", "a0", "a4", "b4", "ac", "bc", "95", "8d", "9d", "99", "81", "91", "86", "96", "8e",
        "84", "94", "8b", "bc", "ac", "b4", "a4", "a0", "be", "ae", "b6", "a6", "b1", "a9", "a2",
        "a5", "b5", "ad", "bd", "b9", "a1",
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
