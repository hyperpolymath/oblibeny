// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Assembly code emitter for x86-64.
//!
//! This module provides utilities for emitting x86-64 assembly instructions
//! in a type-safe manner.

use std::fmt::{self, Display, Write};

/// x86-64 register
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg {
    // 64-bit general purpose
    /// RAX - accumulator
    Rax,
    /// RBX - base
    Rbx,
    /// RCX - counter
    Rcx,
    /// RDX - data
    Rdx,
    /// RSI - source index
    Rsi,
    /// RDI - destination index
    Rdi,
    /// RSP - stack pointer
    Rsp,
    /// RBP - base pointer
    Rbp,
    /// R8
    R8,
    /// R9
    R9,
    /// R10
    R10,
    /// R11
    R11,
    /// R12
    R12,
    /// R13
    R13,
    /// R14
    R14,
    /// R15
    R15,
}

impl Reg {
    /// Get the 8-bit version of this register
    pub fn byte(&self) -> &'static str {
        match self {
            Reg::Rax => "al",
            Reg::Rbx => "bl",
            Reg::Rcx => "cl",
            Reg::Rdx => "dl",
            Reg::Rsi => "sil",
            Reg::Rdi => "dil",
            Reg::Rsp => "spl",
            Reg::Rbp => "bpl",
            Reg::R8 => "r8b",
            Reg::R9 => "r9b",
            Reg::R10 => "r10b",
            Reg::R11 => "r11b",
            Reg::R12 => "r12b",
            Reg::R13 => "r13b",
            Reg::R14 => "r14b",
            Reg::R15 => "r15b",
        }
    }

    /// Get the 32-bit version of this register
    pub fn dword(&self) -> &'static str {
        match self {
            Reg::Rax => "eax",
            Reg::Rbx => "ebx",
            Reg::Rcx => "ecx",
            Reg::Rdx => "edx",
            Reg::Rsi => "esi",
            Reg::Rdi => "edi",
            Reg::Rsp => "esp",
            Reg::Rbp => "ebp",
            Reg::R8 => "r8d",
            Reg::R9 => "r9d",
            Reg::R10 => "r10d",
            Reg::R11 => "r11d",
            Reg::R12 => "r12d",
            Reg::R13 => "r13d",
            Reg::R14 => "r14d",
            Reg::R15 => "r15d",
        }
    }

    /// Check if this is a callee-saved register
    pub fn is_callee_saved(&self) -> bool {
        matches!(
            self,
            Reg::Rbx | Reg::Rbp | Reg::R12 | Reg::R13 | Reg::R14 | Reg::R15
        )
    }

    /// Get argument registers in order (System V AMD64 ABI)
    pub fn arg_registers() -> &'static [Reg] {
        &[Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9]
    }

    /// Get caller-saved registers
    pub fn caller_saved() -> &'static [Reg] {
        &[
            Reg::Rax,
            Reg::Rcx,
            Reg::Rdx,
            Reg::Rsi,
            Reg::Rdi,
            Reg::R8,
            Reg::R9,
            Reg::R10,
            Reg::R11,
        ]
    }

    /// Get callee-saved registers
    pub fn callee_saved() -> &'static [Reg] {
        &[Reg::Rbx, Reg::R12, Reg::R13, Reg::R14, Reg::R15]
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Reg::Rax => "rax",
            Reg::Rbx => "rbx",
            Reg::Rcx => "rcx",
            Reg::Rdx => "rdx",
            Reg::Rsi => "rsi",
            Reg::Rdi => "rdi",
            Reg::Rsp => "rsp",
            Reg::Rbp => "rbp",
            Reg::R8 => "r8",
            Reg::R9 => "r9",
            Reg::R10 => "r10",
            Reg::R11 => "r11",
            Reg::R12 => "r12",
            Reg::R13 => "r13",
            Reg::R14 => "r14",
            Reg::R15 => "r15",
        };
        write!(f, "{}", name)
    }
}

/// Memory operand
#[derive(Debug, Clone)]
pub struct Mem {
    /// Base register
    pub base: Option<Reg>,
    /// Index register
    pub index: Option<Reg>,
    /// Scale factor (1, 2, 4, or 8)
    pub scale: u8,
    /// Displacement
    pub disp: i64,
}

impl Mem {
    /// Create a memory operand with just a base register
    pub fn base(reg: Reg) -> Self {
        Mem {
            base: Some(reg),
            index: None,
            scale: 1,
            disp: 0,
        }
    }

    /// Create a memory operand with base + displacement
    pub fn base_disp(reg: Reg, disp: i64) -> Self {
        Mem {
            base: Some(reg),
            index: None,
            scale: 1,
            disp,
        }
    }

    /// Create a stack slot reference
    pub fn stack(offset: i64) -> Self {
        Self::base_disp(Reg::Rbp, -offset)
    }
}

impl Display for Mem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;

        let mut need_plus = false;

        if let Some(base) = &self.base {
            write!(f, "{}", base)?;
            need_plus = true;
        }

        if let Some(index) = &self.index {
            if need_plus {
                write!(f, "+")?;
            }
            write!(f, "{}*{}", index, self.scale)?;
            need_plus = true;
        }

        if self.disp != 0 {
            if need_plus && self.disp > 0 {
                write!(f, "+")?;
            }
            write!(f, "{}", self.disp)?;
        } else if !need_plus {
            write!(f, "0")?;
        }

        write!(f, "]")
    }
}

/// Assembly emitter
pub struct Emitter {
    /// Output buffer
    output: String,
    /// Current indentation level
    indent: usize,
}

impl Emitter {
    /// Create a new emitter
    pub fn new() -> Self {
        Emitter {
            output: String::new(),
            indent: 0,
        }
    }

    /// Get the generated output
    pub fn finish(self) -> String {
        self.output
    }

    /// Emit a comment
    pub fn comment(&mut self, text: &str) {
        self.emit_indent();
        writeln!(&mut self.output, "; {}", text).unwrap();
    }

    /// Emit a label
    pub fn label(&mut self, name: &str) {
        writeln!(&mut self.output, "{}:", name).unwrap();
    }

    /// Emit a global directive
    pub fn global(&mut self, name: &str) {
        self.emit_indent();
        writeln!(&mut self.output, "global {}", name).unwrap();
    }

    /// Emit a section directive
    pub fn section(&mut self, name: &str) {
        writeln!(&mut self.output, "section {}", name).unwrap();
    }

    // --- Move instructions ---

    /// MOV reg, reg
    pub fn mov_rr(&mut self, dest: Reg, src: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "mov {}, {}", dest, src).unwrap();
    }

    /// MOV reg, imm
    pub fn mov_ri(&mut self, dest: Reg, imm: i64) {
        self.emit_indent();
        writeln!(&mut self.output, "mov {}, {}", dest, imm).unwrap();
    }

    /// MOV reg, mem
    pub fn mov_rm(&mut self, dest: Reg, src: &Mem) {
        self.emit_indent();
        writeln!(&mut self.output, "mov {}, {}", dest, src).unwrap();
    }

    /// MOV mem, reg
    pub fn mov_mr(&mut self, dest: &Mem, src: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "mov {}, {}", dest, src).unwrap();
    }

    /// MOV mem, imm
    pub fn mov_mi(&mut self, dest: &Mem, imm: i64) {
        self.emit_indent();
        writeln!(&mut self.output, "mov qword {}, {}", dest, imm).unwrap();
    }

    // --- Arithmetic instructions ---

    /// ADD reg, reg
    pub fn add_rr(&mut self, dest: Reg, src: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "add {}, {}", dest, src).unwrap();
    }

    /// ADD reg, imm
    pub fn add_ri(&mut self, dest: Reg, imm: i64) {
        self.emit_indent();
        writeln!(&mut self.output, "add {}, {}", dest, imm).unwrap();
    }

    /// SUB reg, reg
    pub fn sub_rr(&mut self, dest: Reg, src: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "sub {}, {}", dest, src).unwrap();
    }

    /// SUB reg, imm
    pub fn sub_ri(&mut self, dest: Reg, imm: i64) {
        self.emit_indent();
        writeln!(&mut self.output, "sub {}, {}", dest, imm).unwrap();
    }

    /// IMUL reg, reg
    pub fn imul_rr(&mut self, dest: Reg, src: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "imul {}, {}", dest, src).unwrap();
    }

    /// IDIV reg (rdx:rax / reg, quotient in rax, remainder in rdx)
    pub fn idiv(&mut self, divisor: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "idiv {}", divisor).unwrap();
    }

    /// CQO (sign-extend rax into rdx:rax)
    pub fn cqo(&mut self) {
        self.emit_indent();
        writeln!(&mut self.output, "cqo").unwrap();
    }

    /// NEG reg
    pub fn neg(&mut self, reg: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "neg {}", reg).unwrap();
    }

    // --- Bitwise instructions ---

    /// AND reg, reg
    pub fn and_rr(&mut self, dest: Reg, src: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "and {}, {}", dest, src).unwrap();
    }

    /// OR reg, reg
    pub fn or_rr(&mut self, dest: Reg, src: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "or {}, {}", dest, src).unwrap();
    }

    /// XOR reg, reg
    pub fn xor_rr(&mut self, dest: Reg, src: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "xor {}, {}", dest, src).unwrap();
    }

    /// NOT reg
    pub fn not(&mut self, reg: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "not {}", reg).unwrap();
    }

    /// SHL reg, cl
    pub fn shl(&mut self, dest: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "shl {}, cl", dest).unwrap();
    }

    /// SHR reg, cl
    pub fn shr(&mut self, dest: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "shr {}, cl", dest).unwrap();
    }

    /// SAR reg, cl
    pub fn sar(&mut self, dest: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "sar {}, cl", dest).unwrap();
    }

    // --- Comparison instructions ---

    /// CMP reg, reg
    pub fn cmp_rr(&mut self, left: Reg, right: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "cmp {}, {}", left, right).unwrap();
    }

    /// CMP reg, imm
    pub fn cmp_ri(&mut self, left: Reg, imm: i64) {
        self.emit_indent();
        writeln!(&mut self.output, "cmp {}, {}", left, imm).unwrap();
    }

    /// TEST reg, reg
    pub fn test_rr(&mut self, left: Reg, right: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "test {}, {}", left, right).unwrap();
    }

    /// SETcc byte reg
    pub fn setcc(&mut self, cc: &str, dest: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "set{} {}", cc, dest.byte()).unwrap();
    }

    /// MOVZX reg, byte reg
    pub fn movzx_byte(&mut self, dest: Reg, src: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "movzx {}, {}", dest, src.byte()).unwrap();
    }

    // --- Control flow ---

    /// JMP label
    pub fn jmp(&mut self, label: &str) {
        self.emit_indent();
        writeln!(&mut self.output, "jmp {}", label).unwrap();
    }

    /// JZ label
    pub fn jz(&mut self, label: &str) {
        self.emit_indent();
        writeln!(&mut self.output, "jz {}", label).unwrap();
    }

    /// JNZ label
    pub fn jnz(&mut self, label: &str) {
        self.emit_indent();
        writeln!(&mut self.output, "jnz {}", label).unwrap();
    }

    /// JE label
    pub fn je(&mut self, label: &str) {
        self.emit_indent();
        writeln!(&mut self.output, "je {}", label).unwrap();
    }

    /// JNE label
    pub fn jne(&mut self, label: &str) {
        self.emit_indent();
        writeln!(&mut self.output, "jne {}", label).unwrap();
    }

    /// CALL label
    pub fn call(&mut self, target: &str) {
        self.emit_indent();
        writeln!(&mut self.output, "call {}", target).unwrap();
    }

    /// CALL reg
    pub fn call_reg(&mut self, target: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "call {}", target).unwrap();
    }

    /// RET
    pub fn ret(&mut self) {
        self.emit_indent();
        writeln!(&mut self.output, "ret").unwrap();
    }

    // --- Stack operations ---

    /// PUSH reg
    pub fn push(&mut self, reg: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "push {}", reg).unwrap();
    }

    /// POP reg
    pub fn pop(&mut self, reg: Reg) {
        self.emit_indent();
        writeln!(&mut self.output, "pop {}", reg).unwrap();
    }

    // --- Miscellaneous ---

    /// NOP
    pub fn nop(&mut self) {
        self.emit_indent();
        writeln!(&mut self.output, "nop").unwrap();
    }

    /// UD2 (undefined instruction, for unreachable code)
    pub fn ud2(&mut self) {
        self.emit_indent();
        writeln!(&mut self.output, "ud2").unwrap();
    }

    /// Raw assembly line
    pub fn raw(&mut self, line: &str) {
        self.emit_indent();
        writeln!(&mut self.output, "{}", line).unwrap();
    }

    /// Emit indentation
    fn emit_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
    }

    /// Increase indentation
    pub fn indent(&mut self) {
        self.indent += 1;
    }

    /// Decrease indentation
    pub fn dedent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }
}

impl Default for Emitter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reg_display() {
        assert_eq!(format!("{}", Reg::Rax), "rax");
        assert_eq!(format!("{}", Reg::R15), "r15");
    }

    #[test]
    fn test_mem_display() {
        let mem = Mem::base_disp(Reg::Rbp, -8);
        assert_eq!(format!("{}", mem), "[rbp-8]");

        let mem = Mem::base(Reg::Rax);
        assert_eq!(format!("{}", mem), "[rax]");
    }

    #[test]
    fn test_emitter_basic() {
        let mut e = Emitter::new();
        e.global("main");
        e.label("main");
        e.indent();
        e.push(Reg::Rbp);
        e.mov_rr(Reg::Rbp, Reg::Rsp);
        e.mov_ri(Reg::Rax, 42);
        e.pop(Reg::Rbp);
        e.ret();

        let output = e.finish();
        assert!(output.contains("global main"));
        assert!(output.contains("main:"));
        assert!(output.contains("push rbp"));
        assert!(output.contains("mov rax, 42"));
    }
}
