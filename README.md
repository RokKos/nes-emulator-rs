# NES Emulator  

A NES emulator written in Rust.

## Goals

The primary goal for me is to play [Super Mario Bros](https://en.wikipedia.org/wiki/Super_Mario_Bros.) on MY emulator and save the princes Peach. 

The tools and techniques used in this project are also choosen because I have these goals in mind. 

-   **Learn Rust:** To gain a deeper understanding and practical experience with the Rust programming language.
-   **Learn the 6502 CPU and PPU:** To understand the architecture and instruction set of the MOS Technology 6502 processor and the Picture Processing Unit (PPU) used in the NES.
-   **Compile to WebAssembly:** To enable the emulator to run in modern web browsers, making it easily accessible.

### Side Goals

-   **Cycle-Accurate Emulation:** To strive for an emulation that is precise to the clock cycle of the original hardware.
-   **Cross-Platform:** To ensure the emulator can be compiled and run on various operating systems.
-   **Debugger:** Have some way to check the internals or the amazing ROM created.

## Implemented Opcodes

The following table details the implementation status of the 6502 instruction set.

**Legend:**

* ✅ - Implemented
* ⚠️ - Implemented, but not yet cycle-accurate
* ❌ - Not Implemented

### 6502 Instruction Set

| off | 00 | 20 | 40 | 60 | 80 | a0 | c0 | e0 | Addressing Mode |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :--- |
| **+00** | ❌ BRK | ⚠️ JSR | ❌ RTI | ❌ RTS | ❌ NOP* | ✅ LDY | ✅ CPY | ✅ CPX | Implied/Immediate |
| **+01** | ✅ ORA | ✅ AND | ✅ EOR | ✅ ADC | ✅ STA | ✅ LDA | ✅ CMP | ✅ SBC | (Indirect,X) |
| **+02** | ❌  | ❌  | ❌  | ❌  | ❌ NOP* | ✅ LDX | ❌ NOP* | ❌ NOP* | ?/Immediate |
| **+03** | ❌ SLO* | ❌ RLA* | ❌ SRE* | ❌ RRA* | ❌ SAX* | ❌ LAX* | ❌ DCP* | ❌ ISB* | (Indirect,X) |
| **+04** | ❌ NOP* | ❌ BIT | ❌ NOP* | ❌ NOP* | ✅ STY | ✅ LDY | ✅ CPY | ✅ CPX | Zero Page |
| **+05** | ✅ ORA | ✅ AND | ✅ EOR | ✅ ADC | ✅ STA | ✅ LDA | ✅ CMP | ✅ SBC | Zero Page |
| **+06** | ❌ ASL | ❌ ROL | ❌ LSR | ❌ ROR | ✅ STX | ✅ LDX | ❌ DEC | ❌ INC | Zero Page |
| **+07** | ❌ SLO* | ❌ RLA* | ❌ SRE* | ❌ RRA* | ❌ SAX* | ❌ LAX* | ❌ DCP* | ❌ ISB* | Zero Page |
| **+08** | ❌ PHP | ❌ PLP | ❌ PHA | ❌ PLA | ✅ DEY | ✅ TAY | ❌ INY | ❌ INX | Implied |
| **+09** | ✅ ORA | ✅ AND | ✅ EOR | ✅ ADC | ❌ NOP* | ✅ LDA | ✅ CMP | ✅ SBC | Immediate |
| **+0a** | ❌ ASL | ❌ ROL | ❌ LSR | ❌ ROR | ✅ TXA | ✅ TAX | ✅ DEX | ❌ NOP | Accumulator/Implied |
| **+0b** | ❌ ANC** | ❌ ANC** | ❌ ASR** | ❌ ARR** | ❌ ANE** | ❌ LXA** | ❌ SBX** | ❌ SBC* | Immediate |
| **+0c** | ❌ NOP* | ❌ BIT | ✅ JMP | ✅ JMP () | ✅ STY | ✅ LDY | ✅ CPY | ✅ CPX | Absolute |
| **+0d** | ✅ ORA | ✅ AND | ✅ EOR | ✅ ADC | ✅ STA | ✅ LDA | ✅ CMP | ✅ SBC | Absolute |
| **+0e** | ❌ ASL | ❌ ROL | ❌ LSR | ❌ ROR | ✅ STX | ✅ LDX | ❌ DEC | ❌ INC | Absolute |
| **+0f** | ❌ SLO* | ❌ RLA* | ❌ SRE* | ❌ RRA* | ❌ SAX* | ❌ LAX* | ❌ DCP* | ❌ ISB* | Absolute |
| **+10** | ❌ BPL | ❌ BMI | ❌ BVC | ❌ BVS | ❌ BCC | ❌ BCS | ❌ BNE | ❌ BEQ | Relative |
| **+11** | ✅ ORA | ✅ AND | ✅ EOR | ✅ ADC | ✅ STA | ✅ LDA | ✅ CMP | ✅ SBC | (Indirect),Y |
| **+12** | ❌  | ❌  | ❌  | ❌  | ❌  | ❌  | ❌  | ❌  | ? |
| **+13** | ❌ SLO* | ❌ RLA* | ❌ SRE* | ❌ RRA* | ❌ SHA** | ❌ LAX* | ❌ DCP* | ❌ ISB* | (Indirect),Y |
| **+14** | ❌ NOP* | ❌ NOP* | ❌ NOP* | ❌ NOP* | ✅ STY | ✅ LDY | ❌ NOP* | ❌ NOP* | Zero Page,X |
| **+15** | ✅ ORA | ✅ AND | ✅ EOR | ✅ ADC | ✅ STA | ✅ LDA | ✅ CMP | ✅ SBC | Zero Page,X |
| **+16** | ❌ ASL | ❌ ROL | ❌ LSR | ❌ ROR | ✅ STX | ✅ LDX | ❌ DEC | ❌ INC | Zero Page,X |
| **+17** | ❌ SLO* | ❌ RLA* | ❌ SRE* | ❌ RRA* | ❌ SAX* | ❌ LAX* | ❌ DCP* | ❌ ISB* | Zero Page,X |
| **+18** | ✅ CLC | ✅ SEC | ❌ CLI | ❌ SEI | ✅ TYA | ✅ CLV | ✅ CLD | ✅ SED | Implied |
| **+19** | ✅ ORA | ✅ AND | ✅ EOR | ✅ ADC | ✅ STA | ✅ LDA | ✅ CMP | ✅ SBC | Absolute,Y |
| **+1a** | ❌ NOP* | ❌ NOP* | ❌ NOP* | ❌ NOP* | ✅ TXS | ✅ TSX | ❌ NOP* | ❌ NOP* | Implied |
| **+1b** | ❌ SLO* | ❌ RLA* | ❌ SRE* | ❌ RRA* | ❌ SHS** | ❌ LAS** | ❌ DCP* | ❌ ISB* | Absolute,Y |
| **+1c** | ❌ NOP* | ❌ NOP* | ❌ NOP* | ❌ NOP* | ❌ SHY** | ✅ LDY | ❌ NOP* | ❌ NOP* | Absolute,X |
| **+1d** | ✅ ORA | ✅ AND | ✅ EOR | ✅ ADC | ✅ STA | ✅ LDA | ✅ CMP | ✅ SBC | Absolute,X |
| **+1e** | ❌ ASL | ❌ ROL | ❌ LSR | ❌ ROR | ❌ SHX** | ✅ LDX | ❌ DEC | ❌ INC | Absolute,X |
| **+1f** | ❌ SLO* | ❌ RLA* | ❌ SRE* | ❌ RRA* | ❌ SHA** | ❌ LAX* | ❌ DCP* | ❌ ISB* | Absolute,X |
