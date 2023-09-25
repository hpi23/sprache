use std::{env, fs, process, time::Instant, io};

fn main() {
    let start = Instant::now();
    let path = env::args().nth(1).unwrap();
    let code = fs::read_to_string(&path).unwrap();
    let exit_code = hpi_interpreter_tree::run(&code, &path, io::stdout()).unwrap().0;
    println!("Program exited with code {exit_code}");
    println!("{:?}", start.elapsed());
    process::exit(exit_code as i32);
}
