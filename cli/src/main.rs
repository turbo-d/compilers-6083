use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::path::Path;
use std::process::{self, Command};

use compiler::codegen::CodeGen;
use compiler::error::CompilerError;
use compiler::llparser::LLParser;
use compiler::scanner::{Scan, Scanner};
use compiler::token::Token;
use compiler::typechecker::TypeChecker;

use inkwell::context::Context;
use inkwell::OptimizationLevel;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};

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
            eprintln!("Failed");
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
            eprintln!("Failed");
            process::exit(1);
        }
    }

    if debug {
        println!("{ast}");
    }

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("tmp");
    module.set_source_file_name(file_path);
    let mut codegen = CodeGen::new(&context, &builder, &module);
    ast.accept(&mut codegen);

    if debug {
        println!("{}", module.to_string());
    }

    if let Err(e) = module.verify() {
        eprintln!("{}", e.to_string());
        eprintln!("Failed");
        process::exit(1);
    }

    Target::initialize_all(&InitializationConfig::default());
    let target_triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&target_triple).unwrap();
    let target_machine = target.create_target_machine(&target_triple, "generic", "", OptimizationLevel::None, RelocMode::PIC, CodeModel::Default).unwrap();
    module.set_data_layout(&target_machine.get_target_data().get_data_layout());
    module.set_triple(&target_triple);

    let passes: &[&str] = &[
        "instcombine",
        "reassociate",
        "gvn",
        "simplifycfg",
        "mem2reg",
    ];

    module.run_passes(passes.join(",").as_str(), &target_machine, PassBuilderOptions::create()).unwrap();

    if debug {
        println!("{}", module.to_string());
    }

    let llvm_ir_path = Path::new("./out").with_extension("ll");
    module.print_to_file(&llvm_ir_path).expect("Error printing ll file");

    let runtime_path = Path::new("./target/debug/libruntime").with_extension("a");

    let output = Command::new("clang")
        .current_dir(env::current_dir().expect("failed to find current dir"))
        .arg(&llvm_ir_path)
        .arg(&runtime_path)
        .output()
        .expect("failed to execute linker");
    let status = output.status;
    if !status.success() {
        println!("link error: {}", std::str::from_utf8(&output.stderr).unwrap());
        process::exit(1);
    }

    fs::remove_file(llvm_ir_path).expect("failed to remove temporary ll file");

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
