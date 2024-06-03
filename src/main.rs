use std::env;
use std::fs;
use std::io;
use std::io::Write;

use compiler::codegen::CodeGen;
use compiler::llparser::LLParser;
use compiler::scanner::Scanner;
use compiler::symtable::SymTable;
use compiler::token::Token;
//use compiler::typechecker::TypeChecker;
use compiler::types::Types;

use inkwell::context::Context;
//use inkwell::module::Module;

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
        print!("<{tok:?}> ");
        while tok != Token::EOF {
            tok = s.scan();
            let scan_line = s.line();
            for _ in out_line..scan_line {
                print!("\n");
            }
            out_line = scan_line;
            print!("<{tok:?}> ");
        }
        print!("\n");
        io::stdout().flush().unwrap();
    }

    let s = Scanner::new(contents);
    let mut p = LLParser::new(s);
    let ast = p.parse();

    let mut st = SymTable::<Types, Types>::new_with_runtime();
    ast.type_check(&mut st);

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("tmp");
    let _codegen = CodeGen::new(&context, &builder, &module);

    println!("Parse completed!");
}
