use std::env;
use std::fs;

use compiler::{Token, Scanner};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    println!("Compiling {}", file_path);
    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");
    //println!("{}", contents);
    let mut s = Scanner::new(contents);
    let mut tok = s.scan();
    //println!("{}", tok);
    while tok != Token::EOF {
        tok = s.scan();
        //println!("{}", tok);
    }
}
