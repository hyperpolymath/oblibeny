// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Oblíbený Compiler (oblc)
//!
//! Command-line interface for the Oblíbený compiler.

use std::process::ExitCode;

fn main() -> ExitCode {
    println!("oblc - Oblíbený Compiler v{}", oblibeny::VERSION);
    println!("Grammar version: {}", oblibeny::GRAMMAR_VERSION);
    println!();
    println!("Usage: oblc <source.obl>");
    println!();
    println!("Note: This is a work in progress. Parser and lexer are functional.");

    ExitCode::SUCCESS
}
