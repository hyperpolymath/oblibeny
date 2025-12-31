// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! Error types for the driver

use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Frontend not found: {0}")]
    FrontendNotFound(String),

    #[error("Backend not found: {0}")]
    BackendNotFound(String),

    #[error("Frontend failed: {0}")]
    FrontendFailed(String),

    #[error("Backend failed: {0}")]
    BackendFailed(String),

    #[error("Rust compiler failed: {0}")]
    RustcFailed(String),

    #[error("Input file not found: {0}")]
    InputNotFound(String),

    #[error("Invalid input: {0}")]
    InvalidInput(String),
}
