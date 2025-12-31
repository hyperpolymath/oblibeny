// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! Oblibeny Compiler Driver
//!
//! This is the main entry point for the Oblibeny compiler. It coordinates
//! the OCaml frontend and Rust backend to compile .obl source files.
//!
//! Pipeline:
//!   source.obl → [OCaml Frontend] → source.oir.json → [Rust Backend] → source.rs
//!
//! The driver handles:
//! - Finding and invoking the frontend/backend executables
//! - Managing intermediate files
//! - Providing a unified CLI experience

use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::process::{Command, ExitCode};
use tempfile::TempDir;

mod error;
mod pipeline;

use error::Error;

#[derive(Parser, Debug)]
#[command(name = "oblibeny")]
#[command(author = "Hyperpolymath")]
#[command(version = "0.1.0")]
#[command(about = "Oblibeny - Oblivious computing language compiler")]
#[command(long_about = r#"
Oblibeny is a language for writing programs with hidden access patterns.
It compiles to Rust code that uses ORAM (Oblivious RAM) to prevent
side-channel attacks based on memory access patterns.

Examples:
  oblibeny compile source.obl           Compile to Rust
  oblibeny compile source.obl -o out.rs Compile with custom output
  oblibeny check source.obl             Type-check and verify obliviousness
  oblibeny build source.obl             Compile and build executable
"#)]
struct Args {
    #[command(subcommand)]
    command: Commands,

    /// Verbose output
    #[arg(short, long, global = true)]
    verbose: bool,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Compile .obl source to Rust
    Compile {
        /// Input .obl file
        input: PathBuf,

        /// Output .rs file (default: <input>.rs)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Keep intermediate OIR file
        #[arg(long)]
        keep_oir: bool,

        /// Inline runtime (don't require oblibeny-runtime crate)
        #[arg(long)]
        inline_runtime: bool,
    },

    /// Type-check and verify obliviousness without generating code
    Check {
        /// Input .obl file
        input: PathBuf,
    },

    /// Compile and build executable
    Build {
        /// Input .obl file
        input: PathBuf,

        /// Output executable (default: <input> without extension)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Build in release mode
        #[arg(long)]
        release: bool,
    },

    /// Show compiler version and paths
    Info,
}

fn main() -> ExitCode {
    env_logger::init();
    let args = Args::parse();

    match run(args) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("error: {}", e);
            ExitCode::FAILURE
        }
    }
}

fn run(args: Args) -> Result<(), Error> {
    match args.command {
        Commands::Compile {
            input,
            output,
            keep_oir,
            inline_runtime,
        } => {
            let config = pipeline::CompileConfig {
                input,
                output,
                keep_oir,
                inline_runtime,
                verbose: args.verbose,
            };
            pipeline::compile(config)
        }

        Commands::Check { input } => {
            let config = pipeline::CheckConfig {
                input,
                verbose: args.verbose,
            };
            pipeline::check(config)
        }

        Commands::Build {
            input,
            output,
            release,
        } => {
            let config = pipeline::BuildConfig {
                input,
                output,
                release,
                verbose: args.verbose,
            };
            pipeline::build(config)
        }

        Commands::Info => {
            println!("Oblibeny Compiler v0.1.0");
            println!();
            println!("Frontend: oblibeny-frontend (OCaml)");
            println!("Backend:  oblibeny-backend (Rust)");
            println!("Runtime:  oblibeny-runtime (Rust)");
            println!();

            // Try to find components
            match which::which("oblibeny-frontend") {
                Ok(path) => println!("Frontend path: {}", path.display()),
                Err(_) => println!("Frontend path: not found in PATH"),
            }
            match which::which("oblibeny-backend") {
                Ok(path) => println!("Backend path: {}", path.display()),
                Err(_) => println!("Backend path: not found in PATH"),
            }

            Ok(())
        }
    }
}
