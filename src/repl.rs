use crate::lexer;
use crate::parser;
use std::io::BufRead;

const PROMPT: &str = ">> ";

pub fn start<I: std::io::Read, O: std::io::Write>(input: &mut I, output: &mut O) {
    let mut scanner = std::io::BufReader::new(input);
    loop {
        write!(output, "{}", PROMPT).unwrap();
        output.flush().unwrap();
        let mut line = String::new();
        match scanner.read_line(&mut line) {
            Ok(_) => {
                let l = lexer::new(line);
                let mut p = parser::new(l);
                match p.parse_program() {
                    Ok(program) => {
                        writeln!(output, "{}", program).unwrap();
                        output.flush().unwrap();
                    }
                    Err(err) => {
                        writeln!(output, "{}", err).unwrap();
                        output.flush().unwrap();
                    }
                }
            }
            _ => return,
        }
    }
}
