use std::{env, fs, process, time::Instant};

use hpi_transpiler_c::TranspileArgs;

fn main() {
    let path = env::args().nth(1).expect("<file-path> is required");
    let code = fs::read_to_string(&path).unwrap();
    let start = Instant::now();

    let config = TranspileArgs {
        emit_comments: env::args().nth(2).expect("<emit-comments> is required") == "1",
        emit_readable_names: env::args().nth(3).expect("<emit-readable-names> is required") == "1",
        gc_enable: env::args().nth(4).expect("<gc-enable> is required") == "1",
        gc_cleanup_on_exit: env::args().nth(5).expect("<gc-cleanup-on-exit> is required") == "1",
    };

    let (out, diagnostics) =
        hpi_transpiler_c::transpile(&code, &path, config).unwrap_or_else(|diagnostics| {
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
