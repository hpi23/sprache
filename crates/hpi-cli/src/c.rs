use std::{
    fs,
    process::{Command, Stdio},
};

use anyhow::{bail, Context};
use include_dir::Dir;

static LIBSAP_DIR: Dir<'_> = include_dir::include_dir!("$CARGO_MANIFEST_DIR/../hpi-transpiler-c/libSAP");

pub fn compile_binary(c_program: &str, bin_output: &str) -> anyhow::Result<()> {
    let tmpdir = tempfile::tempdir()?;

    let c_path = tmpdir.path().join("output.c");
    fs::write(&c_path, c_program)
        .with_context(|| format!("cannot write to `{file}`", file = c_path.to_string_lossy()))?;

    let libsap_file = tmpdir.path().join("libsap.a");

    let libsap_file_contents = LIBSAP_DIR.get_file("libSAP.a").unwrap().contents();

    fs::write(&libsap_file, libsap_file_contents).with_context(|| {
        format!(
            "cannot write to `{file}`",
            file = libsap_file.to_string_lossy()
        )
    })?;

    println!("compiling... {}", tmpdir.path().to_string_lossy());

    let libsap_base_path = tmpdir.path().join("libSAP");
    fs::create_dir_all(libsap_base_path.clone()).unwrap();
    LIBSAP_DIR.extract(libsap_base_path).unwrap();

    let process = Command::new("gcc")
        .arg(c_path)
        .arg(libsap_file)
        .arg("-lcurl")
        .arg("-lm")
        .arg("-o")
        .arg(bin_output)
        .stderr(Stdio::piped())
        .spawn()
        .with_context(|| "could not invoke `gcc`")?;

    let out = process.wait_with_output()?;
    match out.status.success() {
        true => {}
        false => bail!(
            "compiling C to binary terminated with code {}: {}",
            out.status.code().unwrap_or(1),
            String::from_utf8_lossy(&out.stderr),
        ),
    }

    Ok(())
}
