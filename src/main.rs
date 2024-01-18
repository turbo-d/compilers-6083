use std::env;
use std::fs;
use std::fmt;

#[derive(Debug, PartialEq)]
enum Token {
    EOF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
       match self {
           Token::EOF => write!(f, "EOF"),
       }
    }
}

struct Scanner {
    stream: String,
    i: i32,
}

impl Scanner {
    fn scan(&mut self) -> Token {
        return Token::EOF;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    println!("Compiling {}", file_path);
    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");
    //println!("{}", contents);
    let mut s = Scanner {
        stream: contents,
        i: 0,
    };
    let mut tok = s.scan();
    println!("{}", tok);
    while tok != Token::EOF {
        println!("{}", tok);
        tok = s.scan();
    }
}
