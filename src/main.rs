use std::env;
use std::fs;
use std::fmt;

#[derive(PartialEq)]
enum Token {
    EOF,
    Identifier,
    Number,
    String,
    Colon,
    Semicolon,
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    Plus,
    Minus,
    Assign,
    LT,
    LTE,
    GT,
    GTE,
    Equal,
    NotEqual,
}

//impl fmt::Display for Token {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//       match self {
//           Token::EOF => write!(f, "EOF"),
//           Token::Identifier => write!(f, "Identifier"),
//           Token::Number => write!(f, "Number"),
//           Token::Colon => write!(f, "Colon"),
//           Token::Semicolon => write!(f, "Semicolon"),
//           Token::LeftParen => write!(f, "LeftParen"),
//           Token::RightParen => write!(f, "RightParen"),
//           Token::LeftSquare => write!(f, "LeftSquare"),
//           Token::RightSquare => write!(f, "RightSquare"),
//           Token::Plus => write!(f, "Plus"),
//           Token::Minus => write!(f, "Minus"),
//           Token::Assign => write!(f, "Assign"),
//       }
//    }
//}

struct Scanner {
    stream: String,
    i: usize,
}

impl Scanner {
    fn scan(&mut self) -> Token {
        // Ignore spaces, tabs, and newlines
        while self.i < self.stream.len() && (self.stream.chars().nth(self.i).unwrap() == ' ' || self.stream.chars().nth(self.i).unwrap() == '\n' || self.stream.chars().nth(self.i).unwrap() == '\t') {
            self.i += 1;

            // Ignore single-line comments
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '/' {
                self.i += 1;
                if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '/' {
                    self.i += 1;
                    while self.i < self.stream.len() && (self.stream.chars().nth(self.i).unwrap() != '\n') {
                        self.i += 1;
                    }
                }
            }
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
        else if self.i < self.stream.len() && (self.stream.chars().nth(self.i).unwrap() >= '0' && self.stream.chars().nth(self.i).unwrap() <= '9') {
            while self.i < self.stream.len() && (self.stream.chars().nth(self.i).unwrap() >= '0' && self.stream.chars().nth(self.i).unwrap() <= '9') {
                self.i += 1;
            }
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '.' {
                self.i += 1;
                while self.i < self.stream.len() && (self.stream.chars().nth(self.i).unwrap() >= '0' && self.stream.chars().nth(self.i).unwrap() <= '9') {
                    self.i += 1;
                }
            }
            let slice = &self.stream[start..self.i];
            println!("{}", slice);
            return Token::Number;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '"' {
            self.i += 1;
            while self.i < self.stream.len() && (self.stream.chars().nth(self.i).unwrap() != '"') {
                self.i += 1;
            }
            self.i += 1;
            let slice = &self.stream[start..self.i];
            println!("{}", slice);
            return Token::String;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ':' {
            self.i += 1;
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
                self.i += 1;
                println!(":=");
                return Token::Assign;
            }
            println!(":");
            return Token::Colon;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ';' {
            self.i += 1;
            println!(";");
            return Token::Semicolon;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '(' {
            self.i += 1;
            println!("(");
            return Token::LeftParen;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ')' {
            self.i += 1;
            println!(")");
            return Token::RightParen;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '[' {
            self.i += 1;
            println!("[");
            return Token::LeftSquare;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ']' {
            self.i += 1;
            println!("]");
            return Token::RightSquare;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '+' {
            self.i += 1;
            println!("+");
            return Token::Plus;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '-' {
            self.i += 1;
            println!("-");
            return Token::Minus;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '<' {
            self.i += 1;
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
                self.i += 1;
                println!("<=");
                return Token::LTE;
            }
            println!("<");
            return Token::LT;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '>' {
            self.i += 1;
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
                self.i += 1;
                println!(">=");
                return Token::GTE;
            }
            println!(">");
            return Token::GT;
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
            self.i += 1;
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
                self.i += 1;
                println!("==");
                return Token::Equal;
            }
        }
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '!' {
            self.i += 1;
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
                self.i += 1;
                println!("!=");
                return Token::NotEqual;
            }
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
