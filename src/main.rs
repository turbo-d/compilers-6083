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

    let mut p = LLParser::new(s, codegen);
    let ast = p.parse();

    let mut st = SymTable::new();
    // TODO: Delete this once runtime is finished.
    // This is just for testing
    let _ = st.insert_global(String::from("getbool"), Types::Proc(Box::new(Types::Bool), Vec::new()));
    let _ = st.insert_global(String::from("getinteger"), Types::Proc(Box::new(Types::Int), Vec::new()));
    let _ = st.insert_global(String::from("getfloat"), Types::Proc(Box::new(Types::Float), Vec::new()));
    let _ = st.insert_global(String::from("getstring"), Types::Proc(Box::new(Types::String), Vec::new()));
    let _ = st.insert_global(String::from("putbool"), Types::Proc(Box::new(Types::Bool), vec![Types::Bool]));
    let _ = st.insert_global(String::from("putinteger"), Types::Proc(Box::new(Types::Bool), vec![Types::Int]));
    let _ = st.insert_global(String::from("putfloat"), Types::Proc(Box::new(Types::Bool), vec![Types::Float]));
    let _ = st.insert_global(String::from("putstring"), Types::Proc(Box::new(Types::Bool), vec![Types::String]));
    let _ = st.insert_global(String::from("sqrt"), Types::Proc(Box::new(Types::Float), vec![Types::Int]));
    ast.type_check(&mut st);

    //let tc = TypeChecker::new();

    println!("Parse completed!");
}
