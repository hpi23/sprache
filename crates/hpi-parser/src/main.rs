use std::{fs, env::args};

use rush_parser::{Parser, Lexer};

fn main() {
    let path = args().nth(1).unwrap();
    let file = fs::read_to_string(&path).unwrap();

    let lexer = Lexer::new(&file, &path);
    let parser = Parser::new(lexer);
    let (ast, errs) = parser.parse();
    if let Err(err) = ast {
        println!("{err:?}");
        println!("l {} | c {}", err.span.start.line, err.span.start.column);
        return;
    }
    for err in errs {
        println!("{err:?}");
        println!("l {} | c {}", err.span.start.line, err.span.start.column);
    }
    println!("{ast:#?}", ast = ast)
}
