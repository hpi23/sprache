use std::{fmt::Display, path::PathBuf};

use anyhow::anyhow;
use clap::{Args, Parser, Subcommand, ValueEnum};

#[derive(Parser, Debug)]
#[clap(author, version, about)]
pub struct Cli {
    /// rush Subcommands
    #[clap(subcommand)]
    pub command: Command,
    /// Enables time tracking for benchmarking
    #[clap(short, long, value_parser)]
    pub time: bool,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Run the source file
    #[clap(alias = "r")]
    Run(RunArgs),
    /// Check the source code without compilation
    #[clap(alias = "c")]
    Check {
        /// Rush Source file
        file: PathBuf,
    },
    /// Launches the rush language server
    Ls,
}

#[derive(Args, Debug)]
pub struct RunArgs {
    /// Path to rush source file
    pub path: PathBuf,
}
