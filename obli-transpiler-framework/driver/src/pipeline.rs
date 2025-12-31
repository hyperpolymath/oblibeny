// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! Compilation pipeline implementation

use crate::error::Error;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

/// Configuration for compile command
pub struct CompileConfig {
    pub input: PathBuf,
    pub output: Option<PathBuf>,
    pub keep_oir: bool,
    pub inline_runtime: bool,
    pub verbose: bool,
}

/// Configuration for check command
pub struct CheckConfig {
    pub input: PathBuf,
    pub verbose: bool,
}

/// Configuration for build command
pub struct BuildConfig {
    pub input: PathBuf,
    pub output: Option<PathBuf>,
    pub release: bool,
    pub verbose: bool,
}

/// Find the frontend executable
fn find_frontend() -> Result<PathBuf, Error> {
    // Try several locations
    let candidates = [
        // In PATH
        which::which("oblibeny-frontend").ok(),
        // Relative to driver (for development)
        std::env::current_exe()
            .ok()
            .and_then(|p| p.parent().map(|p| p.join("oblibeny-frontend"))),
        // In frontend/_build
        Some(PathBuf::from("frontend/_build/default/bin/main.exe")),
    ];

    for candidate in candidates.into_iter().flatten() {
        if candidate.exists() {
            return Ok(candidate);
        }
    }

    Err(Error::FrontendNotFound(
        "oblibeny-frontend not found. Build the frontend first.".to_string(),
    ))
}

/// Find the backend executable
fn find_backend() -> Result<PathBuf, Error> {
    let candidates = [
        which::which("oblibeny-backend").ok(),
        std::env::current_exe()
            .ok()
            .and_then(|p| p.parent().map(|p| p.join("oblibeny-backend"))),
        Some(PathBuf::from("backend/target/release/oblibeny-backend")),
        Some(PathBuf::from("backend/target/debug/oblibeny-backend")),
    ];

    for candidate in candidates.into_iter().flatten() {
        if candidate.exists() {
            return Ok(candidate);
        }
    }

    Err(Error::BackendNotFound(
        "oblibeny-backend not found. Build the backend first.".to_string(),
    ))
}

/// Compile .obl to .rs
pub fn compile(config: CompileConfig) -> Result<(), Error> {
    if !config.input.exists() {
        return Err(Error::InputNotFound(config.input.display().to_string()));
    }

    let frontend = find_frontend()?;
    let backend = find_backend()?;

    // Determine output paths
    let oir_path = config.input.with_extension("oir.json");
    let rs_path = config.output.unwrap_or_else(|| config.input.with_extension("rs"));

    if config.verbose {
        eprintln!("Using frontend: {}", frontend.display());
        eprintln!("Using backend: {}", backend.display());
        eprintln!("Input: {}", config.input.display());
        eprintln!("OIR: {}", oir_path.display());
        eprintln!("Output: {}", rs_path.display());
    }

    // Run frontend
    if config.verbose {
        eprintln!("\n=== Running frontend ===");
    }

    let frontend_status = Command::new(&frontend)
        .arg(&config.input)
        .arg("-o")
        .arg(&oir_path)
        .args(if config.verbose { vec!["-v"] } else { vec![] })
        .status()?;

    if !frontend_status.success() {
        return Err(Error::FrontendFailed(format!(
            "exit code: {:?}",
            frontend_status.code()
        )));
    }

    // Run backend
    if config.verbose {
        eprintln!("\n=== Running backend ===");
    }

    let mut backend_cmd = Command::new(&backend);
    backend_cmd.arg(&oir_path).arg("-o").arg(&rs_path);

    if config.inline_runtime {
        backend_cmd.arg("--inline-runtime");
    }
    if config.verbose {
        backend_cmd.arg("-v");
    }

    let backend_status = backend_cmd.status()?;

    if !backend_status.success() {
        return Err(Error::BackendFailed(format!(
            "exit code: {:?}",
            backend_status.code()
        )));
    }

    // Clean up OIR if not keeping
    if !config.keep_oir && oir_path.exists() {
        std::fs::remove_file(&oir_path)?;
    }

    if config.verbose {
        eprintln!("\nCompilation successful: {}", rs_path.display());
    }

    Ok(())
}

/// Type-check without code generation
pub fn check(config: CheckConfig) -> Result<(), Error> {
    if !config.input.exists() {
        return Err(Error::InputNotFound(config.input.display().to_string()));
    }

    let frontend = find_frontend()?;

    if config.verbose {
        eprintln!("Using frontend: {}", frontend.display());
        eprintln!("Checking: {}", config.input.display());
    }

    let status = Command::new(&frontend)
        .arg(&config.input)
        .arg("--check")
        .args(if config.verbose { vec!["-v"] } else { vec![] })
        .status()?;

    if !status.success() {
        return Err(Error::FrontendFailed(format!(
            "check failed with exit code: {:?}",
            status.code()
        )));
    }

    println!("Check passed: {}", config.input.display());
    Ok(())
}

/// Compile and build executable
pub fn build(config: BuildConfig) -> Result<(), Error> {
    // First compile to Rust
    let rs_path = config.input.with_extension("rs");

    compile(CompileConfig {
        input: config.input.clone(),
        output: Some(rs_path.clone()),
        keep_oir: false,
        inline_runtime: true, // Inline for standalone build
        verbose: config.verbose,
    })?;

    // Determine output executable name
    let exe_path = config.output.unwrap_or_else(|| {
        let stem = config.input.file_stem().unwrap_or_default();
        PathBuf::from(stem)
    });

    if config.verbose {
        eprintln!("\n=== Building executable ===");
    }

    // Compile with rustc
    let mut rustc_cmd = Command::new("rustc");
    rustc_cmd
        .arg(&rs_path)
        .arg("-o")
        .arg(&exe_path)
        .arg("--edition=2021");

    if config.release {
        rustc_cmd.arg("-O");
    }

    // Add runtime dependencies
    rustc_cmd
        .arg("--extern")
        .arg("subtle=libsubtle.rlib")
        .arg("--extern")
        .arg("zeroize=libzeroize.rlib");

    let status = rustc_cmd.status()?;

    if !status.success() {
        // Try with cargo instead
        if config.verbose {
            eprintln!("Direct rustc failed, trying with cargo...");
        }

        // Create a temporary Cargo project
        let temp_dir = TempDir::new()?;
        let project_dir = temp_dir.path();

        // Create Cargo.toml
        let cargo_toml = format!(
            r#"[package]
name = "oblibeny_output"
version = "0.1.0"
edition = "2021"

[dependencies]
subtle = "2.5"
zeroize = "1.7"

[[bin]]
name = "output"
path = "src/main.rs"
"#
        );

        std::fs::create_dir_all(project_dir.join("src"))?;
        std::fs::write(project_dir.join("Cargo.toml"), cargo_toml)?;
        std::fs::copy(&rs_path, project_dir.join("src/main.rs"))?;

        // Build with cargo
        let cargo_status = Command::new("cargo")
            .current_dir(project_dir)
            .arg("build")
            .args(if config.release {
                vec!["--release"]
            } else {
                vec![]
            })
            .status()?;

        if !cargo_status.success() {
            return Err(Error::RustcFailed("cargo build failed".to_string()));
        }

        // Copy the built executable
        let build_mode = if config.release { "release" } else { "debug" };
        let built_exe = project_dir.join(format!("target/{}/output", build_mode));
        std::fs::copy(built_exe, &exe_path)?;
    }

    if config.verbose {
        eprintln!("\nBuild successful: {}", exe_path.display());
    }

    Ok(())
}
