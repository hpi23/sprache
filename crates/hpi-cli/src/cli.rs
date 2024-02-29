use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

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
    /// Transpile the source file
    #[clap(alias = "t")]
    Transpile(TranspileArgs),

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

#[derive(Args, Debug)]
pub struct TranspileArgs {
    /// Path to rush source file
    pub path: PathBuf,
    pub cflags: Vec<String>,
}

impl From<RunArgs> for TranspileArgs {
    fn from(value: RunArgs) -> Self {
        Self {
            path: value.path,
            cflags: vec![],
        }
    }
}
