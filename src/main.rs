// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! Oblíbený Compiler (oblc)
//!
//! Command-line interface for the Oblíbený compiler.

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, Result};
use oblibeny::codegen::Target;
use oblibeny::driver::{CompileOptions, Compiler};
use oblibeny::opt::OptLevel;
use oblibeny::repl::Repl;
use std::path::PathBuf;
use std::process::ExitCode;

#[derive(Parser)]
#[command(name = "oblc")]
#[command(author = "Jonathan D.A. Jewell")]
#[command(version = oblibeny::VERSION)]
#[command(about = "Oblíbený: Dual-Language Paradigm compiler for secure edge computing")]
#[command(long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Build a source file to assembly/object code
    Build {
        /// Input source file (.obl)
        #[arg(required = true)]
        input: PathBuf,

        /// Output file
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Optimization level (0-3)
        #[arg(short = 'O', long, default_value = "1")]
        opt_level: u8,

        /// Target architecture
        #[arg(short, long, default_value = "x86-64")]
        target: String,

        /// Include debug information
        #[arg(short, long)]
        debug: bool,

        /// Skip termination verification
        #[arg(long)]
        no_verify_termination: bool,

        /// Skip resource verification
        #[arg(long)]
        no_verify_resources: bool,
    },

    /// Check a source file without generating code
    Check {
        /// Input source file (.obl)
        #[arg(required = true)]
        input: PathBuf,

        /// Skip termination verification
        #[arg(long)]
        no_verify_termination: bool,

        /// Skip resource verification
        #[arg(long)]
        no_verify_resources: bool,
    },

    /// Run the interpreter on a source file
    Run {
        /// Input source file (.obl)
        #[arg(required = true)]
        input: PathBuf,

        /// Arguments to pass to the program
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },

    /// Start the interactive REPL
    Repl {
        /// Show AST for each expression
        #[arg(long)]
        show_ast: bool,

        /// Show types for each expression
        #[arg(long)]
        show_types: bool,
    },

    /// Parse and dump the AST
    Parse {
        /// Input source file (.obl)
        #[arg(required = true)]
        input: PathBuf,
    },

    /// Lex and dump tokens
    Lex {
        /// Input source file (.obl)
        #[arg(required = true)]
        input: PathBuf,
    },

    /// Display version information
    Version,
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{:?}", e);
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Build {
            input,
            output,
            opt_level,
            target,
            debug,
            no_verify_termination,
            no_verify_resources,
        }) => {
            cmd_build(
                input,
                output,
                opt_level,
                target,
                debug,
                no_verify_termination,
                no_verify_resources,
            )
        }
        Some(Commands::Check {
            input,
            no_verify_termination,
            no_verify_resources,
        }) => cmd_check(input, no_verify_termination, no_verify_resources),
        Some(Commands::Run { input, args }) => cmd_run(input, args),
        Some(Commands::Repl { show_ast, show_types }) => cmd_repl(show_ast, show_types),
        Some(Commands::Parse { input }) => cmd_parse(input),
        Some(Commands::Lex { input }) => cmd_lex(input),
        Some(Commands::Version) | None => {
            print_version();
            Ok(())
        }
    }
}

fn print_version() {
    println!("oblc - Oblíbený Compiler v{}", oblibeny::VERSION);
    println!("Grammar version: {}", oblibeny::GRAMMAR_VERSION);
    println!();
    println!("A Dual-Language Paradigm compiler for secure edge computing.");
    println!();
    println!("Compile-time: Turing-complete metaprogramming");
    println!("Deploy-time:  Guaranteed termination, bounded resources");
    println!();
    println!("Use 'oblc --help' for usage information.");
}

fn cmd_build(
    input: PathBuf,
    output: Option<PathBuf>,
    opt_level: u8,
    target: String,
    debug: bool,
    no_verify_termination: bool,
    no_verify_resources: bool,
) -> Result<()> {
    let opt_level = match opt_level {
        0 => OptLevel::None,
        1 => OptLevel::Basic,
        2 => OptLevel::Standard,
        _ => OptLevel::Aggressive,
    };

    let target = match target.to_lowercase().as_str() {
        "x86-64" | "x86_64" | "amd64" => Target::X86_64,
        "aarch64" | "arm64" => Target::Arm64,
        "wasm" | "wasm32" => Target::Wasm,
        _ => {
            eprintln!("Unknown target: {}", target);
            eprintln!("Supported targets: x86-64, arm64, wasm");
            return Ok(());
        }
    };

    let output_path = output.unwrap_or_else(|| input.with_extension("s"));

    let options = CompileOptions {
        target,
        opt_level,
        debug_info: debug,
        verify_termination: !no_verify_termination,
        verify_resources: !no_verify_resources,
        output: Some(output_path.to_string_lossy().to_string()),
    };

    let mut compiler = Compiler::with_options(options);

    println!("Compiling {}...", input.display());

    let result = compiler.compile_file(&input).map_err(|e| {
        miette::miette!("{}", e)
    })?;

    // Write assembly output
    std::fs::write(&output_path, &result.assembly).into_diagnostic()?;

    // Report warnings
    for warning in &result.warnings {
        eprintln!("warning: {}", warning);
    }

    println!("Wrote {}", output_path.display());
    Ok(())
}

fn cmd_check(
    input: PathBuf,
    no_verify_termination: bool,
    no_verify_resources: bool,
) -> Result<()> {
    let source = std::fs::read_to_string(&input).into_diagnostic()?;

    let mut compiler = Compiler::new();

    println!("Checking {}...", input.display());

    // Parse
    let ast = compiler.parse(&source).map_err(|e| miette::miette!("{}", e))?;

    // Expand macros
    let ast = compiler.expand_macros(ast).map_err(|e| miette::miette!("{}", e))?;

    // Resolve names
    compiler.resolve_names(&ast).map_err(|e| miette::miette!("{}", e))?;

    // Type check
    compiler.type_check(&ast).map_err(|e| miette::miette!("{}", e))?;

    // Validate phases
    compiler.validate_phases(&ast).map_err(|e| miette::miette!("{}", e))?;

    // Termination checking
    if !no_verify_termination {
        compiler.check_termination(&ast).map_err(|e| miette::miette!("{}", e))?;
    }

    // Resource analysis
    if !no_verify_resources {
        compiler.analyze_resources(&ast).map_err(|e| miette::miette!("{}", e))?;
    }

    // Report warnings
    for warning in compiler.warnings() {
        eprintln!("warning: {}", warning);
    }

    println!("OK - {} type-checked successfully", input.display());
    Ok(())
}

fn cmd_run(input: PathBuf, _args: Vec<String>) -> Result<()> {
    let source = std::fs::read_to_string(&input).into_diagnostic()?;

    let mut compiler = Compiler::new();

    // Parse
    let ast = compiler.parse(&source).map_err(|e| miette::miette!("{}", e))?;

    // Expand macros
    let ast = compiler.expand_macros(ast).map_err(|e| miette::miette!("{}", e))?;

    // Run through interpreter
    let mut interpreter = oblibeny::Interpreter::new();

    // Load definitions
    for form in &ast {
        interpreter.load_definition(form);
    }

    // Look for main function
    let result = interpreter.call_function("main", vec![]);

    match result {
        Ok(value) => {
            if !value.is_unit() {
                println!("{}", value);
            }
        }
        Err(e) => {
            eprintln!("Runtime error: {}", e);
        }
    }

    Ok(())
}

fn cmd_repl(show_ast: bool, show_types: bool) -> Result<()> {
    let mut config = oblibeny::repl::ReplConfig::default();
    config.show_ast = show_ast;
    config.show_types = show_types;

    let mut repl = Repl::with_config(config);
    let _ = repl.run();

    Ok(())
}

fn cmd_parse(input: PathBuf) -> Result<()> {
    let source = std::fs::read_to_string(&input).into_diagnostic()?;

    let mut compiler = Compiler::new();

    // Parse
    let ast = compiler.parse(&source).map_err(|e| miette::miette!("{}", e))?;

    // Print AST
    for (i, form) in ast.iter().enumerate() {
        println!("=== Form {} ===", i);
        println!("{:#?}", form);
        println!();
    }

    Ok(())
}

fn cmd_lex(input: PathBuf) -> Result<()> {
    let source = std::fs::read_to_string(&input).into_diagnostic()?;

    let lexer = oblibeny::lexer::Lexer::new(&source);

    println!("Tokens:");
    for token in lexer {
        println!("  {:?}", token);
    }

    Ok(())
}
