use std::{collections::HashMap, env, fs, io, process, str::FromStr, time::Instant};

use anyhow::{bail, Context};
use clap::Parser;
use cli::{Cli, Command};

use hpi_analyzer::{ast::AnalyzedProgram, Diagnostic};
use hpi_interpreter_tree::{HPIHttpClient, Interpreter};
use hpi_transpiler_c::StyleConfig;
use reqwest::{
    header::{HeaderMap, HeaderName, HeaderValue},
    Method,
};

mod c;
mod cli;

struct InterpreterHttpClient {}

impl HPIHttpClient for InterpreterHttpClient {
    fn request(
        &self,
        method: String,
        url: &str,
        body: String,
        headers: HashMap<String, String>,
    ) -> Result<(u16, String), String> {
        let client = reqwest::blocking::Client::builder()
            .build()
            .map_err(|err| err.to_string())?;

        let mut header_map = HeaderMap::new();
        for (key, value) in headers {
            header_map.insert(
                HeaderName::from_str(&key).map_err(|err| err.to_string())?,
                HeaderValue::from_str(&value).map_err(|err| err.to_string())?,
            );
        }

        let res = client
            .request(
                Method::from_str(&method).map_err(|err| err.to_string())?,
                url,
            )
            .body(body)
            .headers(header_map)
            .send()
            .map_err(|err| err.to_string())?;
        Ok((
            res.status().as_u16(),
            res.text().map_err(|err| err.to_string())?,
        ))
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let root_args = Cli::parse();

    match root_args.command {
        Command::Transpile(args) => {
            let total_start = Instant::now();
            let mut start = Instant::now();

            let path = args.path.clone();
            let path_str = path.to_string_lossy().to_string();

            let code =
                fs::read_to_string(path).with_context(|| "Could not read source file")?;

            let file_read_time = start.elapsed();
            start = Instant::now();

            let (out, diagnostics) = hpi_transpiler_c::transpile(
                &code,
                &path_str,
                StyleConfig {
                    emit_comments: true,
                    emit_readable_names: true,
                },
            )
            .unwrap_or_else(|diagnostics| {
                println!(
                    "{}",
                    diagnostics
                        .iter()
                        .map(|d| format!("{d:#}"))
                        .collect::<Vec<String>>()
                        .join("\n\n")
                );
                process::exit(1)
            });

            println!(
                "{}",
                diagnostics
                    .iter()
                    .map(|d| format!("{d:#}"))
                    .collect::<Vec<String>>()
                    .join("\n\n")
            );

            // let total_start = Instant::now();
            // let mut start = Instant::now();
            //
            // let text = fs::read_to_string(&args.path)?;
            //
            // let file_read_time = start.elapsed();
            // start = Instant::now();
            //
            // let tree = analyze(&text, &path)?;
            //
            // let analyze_time = start.elapsed();
            // start = Instant::now();
            //
            // let exit_code = match transpiler {
            //     Ok(code) => code,
            //     Err(err) => bail!(format!("Laufzeitumgebung abgestürtzt: {err}")),
            // };

            if root_args.time {
                eprintln!("Datei Einlesen: {file_read_time:?}");
                eprintln!("Transpilierung: {:?}", start.elapsed());
                eprintln!(
                    "\x1b[90mGes:          {:?}\x1b[0m",
                    total_start.elapsed()
                );
            }

            // Ok(exit_code)

            // println!("tanspile: {:?}", start.elapsed());
            fs::write("output.c", out).unwrap();
        }
        Command::Run(args) => {
            let path = args.path.clone();
            let path = path.to_string_lossy();

            let run_func = || -> anyhow::Result<i64> {
                let total_start = Instant::now();
                let mut start = Instant::now();

                let text = fs::read_to_string(&args.path)?;

                let file_read_time = start.elapsed();
                start = Instant::now();

                let tree = analyze(&text, &path)?;

                let analyze_time = start.elapsed();
                start = Instant::now();

                let http_client = InterpreterHttpClient {};

                let env_vars = env::vars().map(|v| v).collect::<HashMap<String, String>>();

                let exit_code =
                    match Interpreter::new(io::stdout(), http_client, env_vars).run(tree) {
                        Ok(code) => code,
                        Err(err) => bail!(format!("Laufzeitumgebung abgestürtzt: {err}")),
                    };

                if root_args.time {
                    eprintln!("Datei Einlesen:            {file_read_time:?}");
                    eprintln!("Syntaktische / Semantische Analyse:              {analyze_time:?}");
                    eprintln!("Ausführung:        {:?}", start.elapsed());
                    eprintln!(
                        "\x1b[90mGes:                {:?}\x1b[0m",
                        total_start.elapsed()
                    );
                }

                Ok(exit_code)
            };

            let code = run_func()
                .with_context(|| format!("Ausführen der `{path}` war nicht erfolgreich.",))?;
            process::exit(code as i32);
        }
        Command::Check { file: path } => {
            let check_func = || -> anyhow::Result<()> {
                let total_start = Instant::now();
                let mut start = Instant::now();

                let text = fs::read_to_string(&path)?;

                let file_read_time = start.elapsed();
                start = Instant::now();

                let path = path.to_string_lossy();
                analyze(&text, &path)?;

                if root_args.time {
                    eprintln!("Datei Einlese:        {file_read_time:?}");
                    eprintln!(
                        "Syntaktische / Semantische Analyse:          {:?}",
                        start.elapsed()
                    );
                    eprintln!("\x1b[90mGes:            {:?}\x1b[0m", total_start.elapsed());
                }

                Ok(())
            };

            check_func().with_context(|| {
                format!(
                    "Prüfen der Datei `{file}` spürte Mängel auf.",
                    file = path.to_string_lossy()
                )
            })?;
        }
        Command::Ls => hpi_ls::start_service().await,
    }

    Ok(())
}

/// Analyzes the given rush source code, printing diagnostics alongside the way.
fn analyze<'src>(text: &'src str, path: &'src str) -> anyhow::Result<AnalyzedProgram<'src>> {
    match hpi_analyzer::analyze(text, path) {
        Ok((program, diagnostics)) => {
            print_diagnostics(&diagnostics);
            Ok(program)
        }
        Err(diagnostics) => {
            print_diagnostics(&diagnostics);
            bail!("Invalides Programm: Das Analysewerkzeug hat Mängel aufgespürt.")
        }
    }
}

#[inline]
/// Prints the given diagnostics to stderr.
fn print_diagnostics(diagnostics: &[Diagnostic]) {
    for d in diagnostics {
        eprintln!("{d:#}")
    }
}
