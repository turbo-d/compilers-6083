use std::env;
use std::fs;
use std::fmt;

#[derive(PartialEq)]
enum Token {
    EOF,
    Identifier,
    Colon,
    Semicolon,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
       match self {
           Token::EOF => write!(f, "EOF"),
           Token::Identifier => write!(f, "Identifier"),
           Token::Colon => write!(f, "Colon"),
           Token::Semicolon => write!(f, "Semicolon"),
       }
    }
}

struct Scanner {
    stream: String,
    i: usize,
}

impl Scanner {
    fn scan(&mut self) -> Token {
        while self.i < self.stream.len() && (self.stream.chars().nth(self.i).unwrap() == ' ' || self.stream.chars().nth(self.i).unwrap() == '\n') {
            self.i += 1;
        }
        let start = self.i;
        if self.i < self.stream.len() && ((self.stream.chars().nth(self.i).unwrap() >= 'a' && self.stream.chars().nth(self.i).unwrap() <= 'z') || (self.stream.chars().nth(self.i).unwrap() >= 'A' && self.stream.chars().nth(self.i).unwrap() <= 'Z')) {
            self.i += 1;
            while self.i < self.stream.len() && ((self.stream.chars().nth(self.i).unwrap() >= 'a' && self.stream.chars().nth(self.i).unwrap() <= 'z') || (self.stream.chars().nth(self.i).unwrap() >= 'A' && self.stream.chars().nth(self.i).unwrap() <= 'Z')) {
                self.i += 1;
            }
            let slice = &self.stream[start..self.i];
            println!("{}", slice);
            return Token::Identifier;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ':' {
            self.i += 1;
            println!(":");
            return Token::Colon;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ';' {
            self.i += 1;
            println!(":");
            return Token::Semicolon;
        }

        println!("EOF");
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
    //println!("{}", tok);
    while tok != Token::EOF {
        tok = s.scan();
        //println!("{}", tok);
    }
}
