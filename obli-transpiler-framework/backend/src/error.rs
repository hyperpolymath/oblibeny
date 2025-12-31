// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! Error types for the Oblibeny backend

use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("JSON parse error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("Code generation error: {0}")]
    CodeGen(String),

    #[error("Invalid OIR: {0}")]
    InvalidOir(String),

    #[error("Unsupported feature: {0}")]
    Unsupported(String),
}

impl Error {
    pub fn codegen(msg: impl Into<String>) -> Self {
        Error::CodeGen(msg.into())
    }

    pub fn invalid_oir(msg: impl Into<String>) -> Self {
        Error::InvalidOir(msg.into())
    }

    pub fn unsupported(msg: impl Into<String>) -> Self {
        Error::Unsupported(msg.into())
    }
}
