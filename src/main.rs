use std::env;
use std::fs;

use compiler::codegen::CodeGen;
use compiler::llparser::LLParser;
use compiler::scanner::Scanner;
use compiler::symtable::SymTable;
//use compiler::token::Token;
//use compiler::typechecker::TypeChecker;
use compiler::types::Types;

use inkwell::context::Context;
//use inkwell::module::Module;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    println!("Compiling {}", file_path);
    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");
    //println!("{}", contents);
    let s = Scanner::new(contents);

    // test drive scanner
    //let mut tok = s.scan();
    //println!("{}", tok);
    //while tok != Token::EOF {
    //    tok = s.scan();
    //    println!("{}", tok);
    //}

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("tmp");
    let codegen = CodeGen::new(&context, &builder, &module);

    let mut p = LLParser::new(s);
    let ast = p.parse();

    let mut st = SymTable::new_with_runtime();
    ast.type_check(&mut st);

    //let tc = TypeChecker::new();

    println!("Parse completed!");
}
