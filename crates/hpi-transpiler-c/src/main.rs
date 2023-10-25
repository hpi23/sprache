use std::{env, fs, process, time::Instant};

use hpi_transpiler_c::StyleConfig;

fn main() {
    let path = env::args().nth(1).unwrap();
    let code = fs::read_to_string(&path).unwrap();
    let start = Instant::now();

    let style_config = StyleConfig {
        emit_comments: true,
        emit_readable_names: true,
    };

    let (out, diagnostics) = hpi_transpiler_c::transpile(&code, &path, style_config)
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

    println!("tanspile: {:?}", start.elapsed());
    fs::write("output.c", out).unwrap();
}
