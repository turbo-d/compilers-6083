use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

use compiler::codegen::CodeGen;
use compiler::error::CompilerError;
use compiler::llparser::LLParser;
use compiler::scanner::{Scan, Scanner};
use compiler::token::Token;
use compiler::typechecker::TypeChecker;

use inkwell::context::Context;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut debug = false;
    let file_path;
    if args.len() == 3 {
        file_path = &args[2];
        debug = true;
    } else if args.len() == 2 {
        file_path = &args[1];
    } else {
        panic!("Unexpected arguments");
    }

    println!("Compiling {}", file_path);
    let contents = fs::read_to_string(file_path)
        .expect("Unable to read the file");

    if debug {
        // test drive scanner
        let mut s = Scanner::new(contents.clone());
        let mut out_line = 1;
        let mut tok = s.scan();
        let scan_line = s.line();
        for _ in out_line..scan_line {
            print!("\n");
        }
        out_line = scan_line;
        print!("{tok:?}, ");
        while tok != Token::EOF {
            tok = s.scan();
            let scan_line = s.line();
            for _ in out_line..scan_line {
                print!("\n");
            }
            out_line = scan_line;
            print!("{tok:?}, ");
        }
        print!("\n");
        io::stdout().flush().unwrap();
    }

    let s = Scanner::new(contents);
    let mut p = LLParser::new(Box::new(s));
    let mut ast = match p.parse() {
        Ok(ast) => {
            display_errors(file_path, p.get_errors());
            ast
        },
        Err(_) => {
            display_errors(file_path, p.get_errors());
            eprintln!("Parse failed");
            process::exit(1);
        },
    };

    if debug {
        println!("{ast}");
    }

    let mut tc = TypeChecker::new();
    match ast.accept(&mut tc) {
        Ok(_) => {
            display_errors(file_path, tc.get_errors());
        },
        Err(_) => {
            display_errors(file_path, tc.get_errors());
            eprintln!("Semantic checking failed");
            process::exit(1);
        }
    }

    if debug {
        println!("{ast}");
    }

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("tmp");
    let mut codegen = CodeGen::new(&context, &builder, &module);
    ast.accept(&mut codegen);

    println!("Done");
}

fn display_errors(file_path: &str, errs: &Vec<CompilerError>) {
    for err in errs {
        match err {
            CompilerError::Error { line, msg } => eprintln!("ERROR: {msg}\n  --> {file_path}:{line}:0"),
            CompilerError::Warning { line, msg } => eprintln!("WARNING: {msg}\n  --> {file_path}:{line}:0"),
        }
    }
}
