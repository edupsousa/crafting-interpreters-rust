mod ast;
mod interpreter;
mod parser;
mod printer;
mod scanner;
mod tokens;
use interpreter::Interpreter;
use parser::Parser;
use printer::AstPrinter;
use scanner::Scanner;
use std::io::Write;
use std::{fs, io};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut program = Program::new();
    match args.len() {
        1 => {
            println!("Prompt mode");
            program.run_prompt();
        }
        2 => {
            let path = &args[1];
            println!("Input file: {}", path);
            program.run_file(path);
        }
        _ => {
            println!("Usage: {} [path]", args[0]);
        }
    }
}

struct Program {
    interpreter: Interpreter,
    show_ast: bool,
}

impl Program {
    fn new() -> Self {
        Self {
            interpreter: Interpreter::new(),
            show_ast: false,
        }
    }

    fn run_file(&mut self, path: &str) {
        let file = fs::read_to_string(path).expect("Failed to read file");
        self.run(file.as_str());
    }

    fn run_prompt(&mut self) {
        loop {
            print!("> ");
            let _ = io::stdout().flush();
            let mut input = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");
            if input.trim().is_empty() {
                break;
            }
            self.run(input.as_str());
        }
    }

    fn run(&mut self, source: &str) {
        let mut scanner = Scanner::new(source);
        scanner.scan_tokens();

        if scanner.has_errors() {
            for error in scanner.errors {
                println!("{}", error);
            }
        }

        let mut parser = Parser::new(scanner.tokens);
        let statements = parser.parse();
        if self.show_ast {
            let mut printer = AstPrinter::new();
            println!("{}", printer.print_statements(&statements));
        }
        self.interpreter.interpret(&statements);
    }
}
