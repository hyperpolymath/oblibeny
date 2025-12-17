// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

//! REPL (Read-Eval-Print Loop) for Oblíbený.
//!
//! This module provides an interactive environment for evaluating
//! Oblíbený expressions and exploring the language.
//!
//! # Features
//!
//! - Interactive expression evaluation
//! - Multi-line input support
//! - Command system for control operations
//! - History tracking
//! - Type and value inspection
//!
//! # Example
//!
//! ```ignore
//! use oblibeny::Repl;
//!
//! let mut repl = Repl::new();
//! repl.run();
//! ```

use crate::error::Result;
use crate::interpreter::{Interpreter, Value};
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, BufRead, Write};

/// REPL configuration options
#[derive(Debug, Clone)]
pub struct ReplConfig {
    /// Show AST for each expression
    pub show_ast: bool,
    /// Show type information for results
    pub show_types: bool,
    /// Enable verbose output
    pub verbose: bool,
    /// Maximum evaluation depth
    pub max_depth: usize,
    /// Maximum iterations for loops
    pub max_iterations: u64,
    /// Custom prompt string
    pub prompt: String,
    /// Continuation prompt for multi-line input
    pub continuation_prompt: String,
}

impl Default for ReplConfig {
    fn default() -> Self {
        ReplConfig {
            show_ast: false,
            show_types: false,
            verbose: false,
            max_depth: 1000,
            max_iterations: 1_000_000,
            prompt: "oblíbený> ".to_string(),
            continuation_prompt: "       ... ".to_string(),
        }
    }
}

/// REPL command result
#[derive(Debug)]
pub enum ReplCommand {
    /// Continue normal operation
    Continue,
    /// Exit the REPL
    Exit,
    /// Show help
    Help,
    /// Clear the screen
    Clear,
    /// Reset the environment
    Reset,
    /// Toggle type display
    ToggleTypes,
    /// Toggle verbose mode
    ToggleVerbose,
    /// Show defined bindings
    ShowBindings,
    /// Load a file
    Load(String),
    /// Not a command, evaluate as expression
    NotACommand,
}

/// REPL history entry
#[derive(Debug, Clone)]
pub struct HistoryEntry {
    /// Input text
    pub input: String,
    /// Result (if successful)
    pub result: Option<String>,
    /// Error (if failed)
    pub error: Option<String>,
}

/// Interactive REPL for Oblíbený
pub struct Repl {
    /// Configuration
    config: ReplConfig,
    /// Interpreter instance
    interpreter: Interpreter,
    /// Command history
    history: Vec<HistoryEntry>,
    /// Accumulated input for multi-line
    input_buffer: String,
    /// Paren nesting depth
    paren_depth: i32,
    /// Whether REPL is running
    running: bool,
}

impl Repl {
    /// Create a new REPL with default configuration
    pub fn new() -> Self {
        Repl {
            config: ReplConfig::default(),
            interpreter: Interpreter::new(),
            history: Vec::new(),
            input_buffer: String::new(),
            paren_depth: 0,
            running: false,
        }
    }

    /// Create a REPL with custom configuration
    pub fn with_config(config: ReplConfig) -> Self {
        let mut interpreter = Interpreter::new();
        interpreter.set_max_depth(config.max_depth);
        interpreter.set_max_iterations(config.max_iterations);

        Repl {
            config,
            interpreter,
            history: Vec::new(),
            input_buffer: String::new(),
            paren_depth: 0,
            running: false,
        }
    }

    /// Get a reference to the interpreter
    pub fn interpreter(&self) -> &Interpreter {
        &self.interpreter
    }

    /// Get a mutable reference to the interpreter
    pub fn interpreter_mut(&mut self) -> &mut Interpreter {
        &mut self.interpreter
    }

    /// Get the command history
    pub fn history(&self) -> &[HistoryEntry] {
        &self.history
    }

    /// Run the REPL with standard input/output
    pub fn run(&mut self) -> io::Result<()> {
        let stdin = io::stdin();
        let mut stdout = io::stdout();

        self.print_banner(&mut stdout)?;
        self.running = true;

        while self.running {
            // Print prompt
            let prompt = if self.input_buffer.is_empty() {
                &self.config.prompt
            } else {
                &self.config.continuation_prompt
            };
            write!(stdout, "{}", prompt)?;
            stdout.flush()?;

            // Read line
            let mut line = String::new();
            let bytes_read = stdin.lock().read_line(&mut line)?;

            if bytes_read == 0 {
                // EOF
                self.running = false;
                writeln!(stdout)?;
                break;
            }

            // Process line
            self.process_line(&line, &mut stdout)?;
        }

        Ok(())
    }

    /// Run the REPL with custom input/output streams
    pub fn run_with_io<R: BufRead, W: Write>(
        &mut self,
        input: &mut R,
        output: &mut W,
    ) -> io::Result<()> {
        self.print_banner(output)?;
        self.running = true;

        while self.running {
            // Print prompt
            let prompt = if self.input_buffer.is_empty() {
                &self.config.prompt
            } else {
                &self.config.continuation_prompt
            };
            write!(output, "{}", prompt)?;
            output.flush()?;

            // Read line
            let mut line = String::new();
            let bytes_read = input.read_line(&mut line)?;

            if bytes_read == 0 {
                // EOF
                self.running = false;
                writeln!(output)?;
                break;
            }

            // Process line
            self.process_line(&line, output)?;
        }

        Ok(())
    }

    /// Print the REPL banner
    fn print_banner<W: Write>(&self, output: &mut W) -> io::Result<()> {
        writeln!(
            output,
            "Oblíbený REPL v{} (Grammar v{})",
            crate::VERSION,
            crate::GRAMMAR_VERSION
        )?;
        writeln!(output, "Type :help for help, :quit to exit")?;
        writeln!(output)?;
        Ok(())
    }

    /// Process a line of input
    fn process_line<W: Write>(&mut self, line: &str, output: &mut W) -> io::Result<()> {
        let trimmed = line.trim();

        // Check for commands (only when buffer is empty)
        if self.input_buffer.is_empty() && trimmed.starts_with(':') {
            match self.parse_command(trimmed) {
                ReplCommand::Exit => {
                    self.running = false;
                    writeln!(output, "Goodbye!")?;
                    return Ok(());
                }
                ReplCommand::Help => {
                    self.print_help(output)?;
                    return Ok(());
                }
                ReplCommand::Clear => {
                    // ANSI clear screen
                    write!(output, "\x1B[2J\x1B[1;1H")?;
                    return Ok(());
                }
                ReplCommand::Reset => {
                    self.interpreter = Interpreter::new();
                    self.interpreter.set_max_depth(self.config.max_depth);
                    self.interpreter
                        .set_max_iterations(self.config.max_iterations);
                    writeln!(output, "Environment reset")?;
                    return Ok(());
                }
                ReplCommand::ToggleTypes => {
                    self.config.show_types = !self.config.show_types;
                    writeln!(
                        output,
                        "Type display: {}",
                        if self.config.show_types { "on" } else { "off" }
                    )?;
                    return Ok(());
                }
                ReplCommand::ToggleVerbose => {
                    self.config.verbose = !self.config.verbose;
                    writeln!(
                        output,
                        "Verbose mode: {}",
                        if self.config.verbose { "on" } else { "off" }
                    )?;
                    return Ok(());
                }
                ReplCommand::ShowBindings => {
                    self.show_bindings(output)?;
                    return Ok(());
                }
                ReplCommand::Load(path) => {
                    self.load_file(&path, output)?;
                    return Ok(());
                }
                ReplCommand::Continue | ReplCommand::NotACommand => {}
            }
        }

        // Update paren depth
        for ch in line.chars() {
            match ch {
                '(' | '[' | '{' => self.paren_depth += 1,
                ')' | ']' | '}' => self.paren_depth -= 1,
                _ => {}
            }
        }

        // Accumulate input
        self.input_buffer.push_str(line);

        // If parens are balanced, evaluate
        if self.paren_depth <= 0 {
            let input = std::mem::take(&mut self.input_buffer);
            self.paren_depth = 0;

            if !input.trim().is_empty() {
                self.evaluate_and_print(&input, output)?;
            }
        }

        Ok(())
    }

    /// Parse a REPL command
    fn parse_command(&self, input: &str) -> ReplCommand {
        let parts: Vec<&str> = input.split_whitespace().collect();
        if parts.is_empty() {
            return ReplCommand::NotACommand;
        }

        match parts[0] {
            ":quit" | ":q" | ":exit" => ReplCommand::Exit,
            ":help" | ":h" | ":?" => ReplCommand::Help,
            ":clear" | ":cls" => ReplCommand::Clear,
            ":reset" => ReplCommand::Reset,
            ":type" | ":t" => ReplCommand::ToggleTypes,
            ":verbose" | ":v" => ReplCommand::ToggleVerbose,
            ":bindings" | ":b" => ReplCommand::ShowBindings,
            ":load" | ":l" => {
                if parts.len() > 1 {
                    ReplCommand::Load(parts[1].to_string())
                } else {
                    ReplCommand::NotACommand
                }
            }
            _ => ReplCommand::NotACommand,
        }
    }

    /// Print help information
    fn print_help<W: Write>(&self, output: &mut W) -> io::Result<()> {
        writeln!(output, "Oblíbený REPL Commands:")?;
        writeln!(output)?;
        writeln!(output, "  :help, :h, :?     Show this help")?;
        writeln!(output, "  :quit, :q, :exit  Exit the REPL")?;
        writeln!(output, "  :clear, :cls      Clear the screen")?;
        writeln!(output, "  :reset            Reset the environment")?;
        writeln!(output, "  :type, :t         Toggle type display")?;
        writeln!(output, "  :verbose, :v      Toggle verbose mode")?;
        writeln!(output, "  :bindings, :b     Show defined bindings")?;
        writeln!(output, "  :load, :l <file>  Load and execute a file")?;
        writeln!(output)?;
        writeln!(output, "Enter Oblíbený expressions to evaluate them.")?;
        writeln!(output, "Multi-line input is supported (balanced parens).")?;
        writeln!(output)?;
        writeln!(output, "Examples:")?;
        writeln!(output, "  (+ 1 2 3)           => 6")?;
        writeln!(output, "  (let x 10 (* x x))  => 100")?;
        writeln!(output, "  (defun sq (n) (* n n))")?;
        writeln!(output, "  (sq 5)              => 25")?;
        Ok(())
    }

    /// Show defined bindings
    fn show_bindings<W: Write>(&self, output: &mut W) -> io::Result<()> {
        let bindings = self.interpreter.get_bindings();

        if bindings.is_empty() {
            writeln!(output, "No user-defined bindings")?;
        } else {
            writeln!(output, "Defined bindings:")?;
            for (name, value) in bindings {
                writeln!(output, "  {} = {}", name, value)?;
            }
        }
        Ok(())
    }

    /// Load and execute a file
    fn load_file<W: Write>(&mut self, path: &str, output: &mut W) -> io::Result<()> {
        match std::fs::read_to_string(path) {
            Ok(contents) => {
                writeln!(output, "Loading {}...", path)?;
                self.evaluate_and_print(&contents, output)?;
            }
            Err(e) => {
                writeln!(output, "Error loading file: {}", e)?;
            }
        }
        Ok(())
    }

    /// Evaluate input and print the result
    fn evaluate_and_print<W: Write>(&mut self, input: &str, output: &mut W) -> io::Result<()> {
        // Try to parse as a program (top-level forms)
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        match parser.parse_program() {
            Ok(forms) => {
                // Evaluate each form
                for form in forms {
                    match self.interpreter.eval_toplevel(&form) {
                        Ok(value) => {
                            // Record in history
                            self.history.push(HistoryEntry {
                                input: input.to_string(),
                                result: Some(format!("{}", value)),
                                error: None,
                            });

                            // Print result (unless unit)
                            if !matches!(value, Value::Unit) {
                                if self.config.show_types {
                                    writeln!(output, "=> {} : {}", value, value.type_name())?;
                                } else {
                                    writeln!(output, "=> {}", value)?;
                                }
                            }
                        }
                        Err(e) => {
                            // Record error in history
                            self.history.push(HistoryEntry {
                                input: input.to_string(),
                                result: None,
                                error: Some(format!("{}", e)),
                            });

                            writeln!(output, "Error: {}", e)?;
                        }
                    }
                }
            }
            Err(_) => {
                // If parsing as program fails, try parsing as an expression
                let lexer = Lexer::new(input);
                let mut parser = Parser::new(lexer);

                match parser.parse_repl_expression() {
                    Ok(expr) => {
                        let env = crate::interpreter::Environment::new();
                        match self.interpreter.eval_expr(&expr, &env) {
                            Ok(value) => {
                                // Record in history
                                self.history.push(HistoryEntry {
                                    input: input.to_string(),
                                    result: Some(format!("{}", value)),
                                    error: None,
                                });

                                // Print result (unless unit)
                                if !matches!(value, Value::Unit) {
                                    if self.config.show_types {
                                        writeln!(output, "=> {} : {}", value, value.type_name())?;
                                    } else {
                                        writeln!(output, "=> {}", value)?;
                                    }
                                }
                            }
                            Err(e) => {
                                // Record error in history
                                self.history.push(HistoryEntry {
                                    input: input.to_string(),
                                    result: None,
                                    error: Some(format!("{}", e)),
                                });

                                writeln!(output, "Error: {}", e)?;
                            }
                        }
                    }
                    Err(e) => {
                        // Record parse error in history
                        self.history.push(HistoryEntry {
                            input: input.to_string(),
                            result: None,
                            error: Some(format!("{}", e)),
                        });

                        writeln!(output, "Parse error: {}", e)?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Evaluate a single expression and return the result
    pub fn eval(&mut self, input: &str) -> Result<Value> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        // Try to parse as a program (top-level forms)
        match parser.parse_program() {
            Ok(forms) => {
                let mut result = Value::Unit;
                for form in forms {
                    result = self.interpreter.eval_toplevel(&form)?;
                }
                Ok(result)
            }
            Err(_) => {
                // If that fails, try to parse as an expression directly
                // Re-lex and parse as expression
                let lexer = Lexer::new(input);
                let mut parser = Parser::new(lexer);
                if let Ok(expr) = parser.parse_repl_expression() {
                    let env = crate::interpreter::Environment::new();
                    self.interpreter.eval_expr(&expr, &env)
                } else {
                    // Return the original error
                    let lexer = Lexer::new(input);
                    let mut parser = Parser::new(lexer);
                    let forms = parser.parse_program()?;
                    let mut result = Value::Unit;
                    for form in forms {
                        result = self.interpreter.eval_toplevel(&form)?;
                    }
                    Ok(result)
                }
            }
        }
    }

    /// Check if the REPL is currently running
    pub fn is_running(&self) -> bool {
        self.running
    }

    /// Stop the REPL
    pub fn stop(&mut self) {
        self.running = false;
    }
}

impl Default for Repl {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_repl_creation() {
        let repl = Repl::new();
        assert!(!repl.is_running());
        assert!(repl.history().is_empty());
    }

    #[test]
    fn test_repl_eval() {
        // Test simple arithmetic
        {
            let mut repl = Repl::new();
            // Note: (* ...) conflicts with block comment syntax, use (+ ...) instead
            let result = repl.eval("(+ 1 2)");
            assert_eq!(result.unwrap(), Value::Int(3));
        }

        {
            let mut repl = Repl::new();
            let result = repl.eval("(- 10 3)");
            assert_eq!(result.unwrap(), Value::Int(7));
        }
    }

    #[test]
    fn test_repl_commands() {
        let repl = Repl::new();

        assert!(matches!(repl.parse_command(":quit"), ReplCommand::Exit));
        assert!(matches!(repl.parse_command(":q"), ReplCommand::Exit));
        assert!(matches!(repl.parse_command(":help"), ReplCommand::Help));
        assert!(matches!(repl.parse_command(":reset"), ReplCommand::Reset));
        assert!(matches!(repl.parse_command(":type"), ReplCommand::ToggleTypes));
        assert!(matches!(
            repl.parse_command(":load test.obl"),
            ReplCommand::Load(_)
        ));
    }

    #[test]
    fn test_repl_with_io() {
        let mut repl = Repl::new();

        let input = b"(+ 1 2)\n:quit\n";
        let mut input_cursor = Cursor::new(&input[..]);
        let mut output = Vec::new();

        repl.run_with_io(&mut input_cursor, &mut output).unwrap();

        let output_str = String::from_utf8(output).unwrap();
        eprintln!("Output: {}", output_str);
        assert!(output_str.contains("=> 3"), "Expected '=> 3' in output: {}", output_str);
        assert!(output_str.contains("Goodbye!"), "Expected 'Goodbye!' in output: {}", output_str);
    }

    #[test]
    fn test_repl_multiline() {
        let mut repl = Repl::new();

        let input = b"(+ 1\n   2)\n:quit\n";
        let mut input_cursor = Cursor::new(&input[..]);
        let mut output = Vec::new();

        repl.run_with_io(&mut input_cursor, &mut output).unwrap();

        let output_str = String::from_utf8(output).unwrap();
        assert!(output_str.contains("=> 3"));
    }

    #[test]
    fn test_repl_config() {
        let config = ReplConfig {
            show_ast: false,
            show_types: true,
            verbose: true,
            max_depth: 500,
            max_iterations: 100_000,
            prompt: "> ".to_string(),
            continuation_prompt: "| ".to_string(),
        };

        let repl = Repl::with_config(config.clone());
        assert!(repl.config.show_types);
        assert!(repl.config.verbose);
    }
}
