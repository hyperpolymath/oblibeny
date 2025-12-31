// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! Oblibeny Backend
//!
//! This is the Rust backend for the Oblibeny oblivious computing language.
//! It consumes OIR (Oblivious Intermediate Representation) from the OCaml
//! frontend and generates Rust code that uses the ORAM runtime.

mod oir;
mod codegen;
mod error;

use clap::Parser;
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "oblibeny-backend")]
#[command(author = "Hyperpolymath")]
#[command(version = "0.1.0")]
#[command(about = "Oblibeny backend - generates Rust from OIR")]
struct Args {
    /// Input OIR file (.oir.json)
    #[arg(required = true)]
    input: PathBuf,

    /// Output Rust file (default: <input>.rs)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Generate inline runtime (don't require external crate)
    #[arg(long)]
    inline_runtime: bool,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
}

fn main() -> Result<(), error::Error> {
    env_logger::init();
    let args = Args::parse();

    if args.verbose {
        eprintln!("Reading OIR from {:?}...", args.input);
    }

    // Read and parse OIR
    let oir_json = fs::read_to_string(&args.input)?;
    let module: oir::Module = serde_json::from_str(&oir_json)?;

    if args.verbose {
        eprintln!("Parsed module: {:?}", module.name);
        eprintln!("  {} structs", module.structs.len());
        eprintln!("  {} externs", module.externs.len());
        eprintln!("  {} functions", module.functions.len());
    }

    // Generate Rust code
    let mut generator = codegen::CodeGenerator::new();
    generator.set_inline_runtime(args.inline_runtime);
    let rust_code = generator.generate(&module)?;

    // Determine output path
    let output_path = args.output.unwrap_or_else(|| {
        let mut path = args.input.clone();
        path.set_extension("rs");
        path
    });

    if args.verbose {
        eprintln!("Writing Rust to {:?}...", output_path);
    }

    fs::write(&output_path, rust_code)?;

    if args.verbose {
        eprintln!("Done.");
    }

    Ok(())
}
