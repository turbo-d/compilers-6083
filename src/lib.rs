#[derive(PartialEq)]
pub enum Token {
    EOF,
    Bool,
    Type,
    Program,
    Is,
    Global,
    Procedure,
    Variable,
    Begin,
    EndProgram,
    EndProcedure,
    EndIf,
    EndFor,
    If,
    Then,
    Else,
    For,
    Return,
    Identifier,
    Number,
    String,
    Colon,
    Semicolon,
    Period,
    Comma,
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    Plus,
    Minus,
    Multiply,
    Divide,
    And,
    Or,
    Not,
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

pub struct Scanner {
    pub stream: String,
    pub i: usize,
}

impl Scanner {
    // Ignore spaces, tabs, and newlines
    fn skip_whitespace(&mut self) {
        while self.i < self.stream.len() && (self.stream.chars().nth(self.i).unwrap() == ' ' || self.stream.chars().nth(self.i).unwrap() == '\n' || self.stream.chars().nth(self.i).unwrap() == '\t' || self.stream.chars().nth(self.i).unwrap() == '\r') {
            self.i += 1;
        }
    }

    pub fn scan(&mut self) -> Token {
        // skip comments and whitespace
        while self.i < self.stream.len() && (self.stream.chars().nth(self.i).unwrap() == ' ' || self.stream.chars().nth(self.i).unwrap() == '\n' || self.stream.chars().nth(self.i).unwrap() == '\t' || self.stream.chars().nth(self.i).unwrap() == '/' || self.stream.chars().nth(self.i).unwrap() == '\r') {
            self.skip_whitespace();

            // Ignore comments
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '/' {
                self.i += 1;

                // Ignore single-line comments
                if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '/' {
                    self.i += 1;

                    // skip everything until newline
                    while self.i < self.stream.len() && (self.stream.chars().nth(self.i).unwrap() != '\n') {
                        self.i += 1;
                    }
                }

                // Ignore multi-line comments
                else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '*' {
                    // start multi-line comment
                    let mut nesting = 1;
                    self.i += 1;

                    while nesting > 0 {
                        // skip everything in between
                        while self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() != '/' && self.stream.chars().nth(self.i).unwrap() != '*' {
                            self.i += 1;
                        }

                        // multi-line comment close
                        if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '*' {
                            self.i += 1;
                            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '/' {
                                // end multi-line comment
                                nesting -= 1;

                                self.i += 1;
                            }
                        }

                        // multi-line comment open
                        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '/' {
                            self.i += 1;
                            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '*' {
                                // nested multi-line comment
                                nesting += 1;

                                self.i += 1;
                            }
                        }
                    }
                }
            }
        }

        let start = self.i;

        // identifier
        if self.i < self.stream.len() && ((self.stream.chars().nth(self.i).unwrap() >= 'a' && self.stream.chars().nth(self.i).unwrap() <= 'z') || (self.stream.chars().nth(self.i).unwrap() >= 'A' && self.stream.chars().nth(self.i).unwrap() <= 'Z')) {
            self.i += 1;
            while self.i < self.stream.len() && ((self.stream.chars().nth(self.i).unwrap() >= 'a' && self.stream.chars().nth(self.i).unwrap() <= 'z') || (self.stream.chars().nth(self.i).unwrap() >= 'A' && self.stream.chars().nth(self.i).unwrap() <= 'Z') || (self.stream.chars().nth(self.i).unwrap() >= '0' && self.stream.chars().nth(self.i).unwrap() <= '9') || self.stream.chars().nth(self.i).unwrap() == '_') {
                self.i += 1;
            }
            let slice = &self.stream[start..self.i];
            println!("{}", slice);
            return Token::Identifier;
        }

        // number
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

        // string
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

        // assignment and colon
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

        // semicolon
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ';' {
            self.i += 1;
            println!(";");
            return Token::Semicolon;
        }

       // period
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '.' {
            self.i += 1;
            println!(".");
            return Token::Period;
        }

       // comma
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ',' {
            self.i += 1;
            println!(",");
            return Token::Comma;
        }

        // left parenthesis
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '(' {
            self.i += 1;
            println!("(");
            return Token::LeftParen;
        }

        // right parenthesis
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ')' {
            self.i += 1;
            println!(")");
            return Token::RightParen;
        }

        // left square bracket
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '[' {
            self.i += 1;
            println!("[");
            return Token::LeftSquare;
        }

        // right square bracket
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ']' {
            self.i += 1;
            println!("]");
            return Token::RightSquare;
        }

        // plus
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '+' {
            self.i += 1;
            println!("+");
            return Token::Plus;
        }

        // minus
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '-' {
            self.i += 1;
            println!("-");
            return Token::Minus;
        }

        // multiply
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '*' {
            self.i += 1;
            println!("*");
            return Token::Multiply;
        }

        // and
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '&' {
            self.i += 1;
            println!("&");
            return Token::And;
        }

        // or
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '|' {
            self.i += 1;
            println!("|");
            return Token::Or;
        }

        // lt and lte
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

        // gt and gte
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

        // equality
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
            self.i += 1;
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
                self.i += 1;
                println!("==");
                return Token::Equal;
            }
        }

        // not equal
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_add() {
        let mut s = Scanner {
            stream: String::from("+"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Plus);
    }

    #[test]
    fn scan_sub() {
        let mut s = Scanner {
            stream: String::from("-"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Minus);
    }

    #[test]
    fn scan_mul() {
        let mut s = Scanner {
            stream: String::from("*"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Multiply);
    }

    #[test]
    fn scan_div() {
        let mut s = Scanner {
            stream: String::from("/"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Divide);
    }

    #[test]
    fn scan_lt() {
        let mut s = Scanner {
            stream: String::from("<"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::LT);
    }

    #[test]
    fn scan_lte() {
        let mut s = Scanner {
            stream: String::from("<="),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::LTE);
    }

    #[test]
    fn scan_gt() {
        let mut s = Scanner {
            stream: String::from(">"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::GT);
    }

    #[test]
    fn scan_gte() {
        let mut s = Scanner {
            stream: String::from(">="),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::GTE);
    }

    #[test]
    fn scan_equal() {
        let mut s = Scanner {
            stream: String::from("=="),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Equal);
    }

    #[test]
    fn scan_notequal() {
        let mut s = Scanner {
            stream: String::from("!="),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::NotEqual);
    }

    #[test]
    fn scan_and() {
        let mut s = Scanner {
            stream: String::from("&"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::And);
    }

    #[test]
    fn scan_or() {
        let mut s = Scanner {
            stream: String::from("|"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Or);
    }

    #[test]
    fn scan_not() {
        let mut s = Scanner {
            stream: String::from("not"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Not);
    }

    #[test]
    fn scan_bool_true() {
        let mut s = Scanner {
            stream: String::from("true"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Bool);
    }

    #[test]
    fn scan_bool_false() {
        let mut s = Scanner {
            stream: String::from("false"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Bool);
    }

    #[test]
    fn scan_type_integer() {
        let mut s = Scanner {
            stream: String::from("integer"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Type);
    }

    #[test]
    fn scan_type_float() {
        let mut s = Scanner {
            stream: String::from("float"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Type);
    }

    #[test]
    fn scan_type_string() {
        let mut s = Scanner {
            stream: String::from("string"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Type);
    }

    #[test]
    fn scan_type_bool() {
        let mut s = Scanner {
            stream: String::from("bool"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Type);
    }

    #[test]
    fn scan_program() {
        let mut s = Scanner {
            stream: String::from("program"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Program);
    }

    #[test]
    fn scan_is() {
        let mut s = Scanner {
            stream: String::from("is"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Is);
    }

    #[test]
    fn scan_global() {
        let mut s = Scanner {
            stream: String::from("global"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Global);
    }

    #[test]
    fn scan_procedure() {
        let mut s = Scanner {
            stream: String::from("procedure"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Procedure);
    }

    #[test]
    fn scan_variable() {
        let mut s = Scanner {
            stream: String::from("variable"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Variable);
    }

    #[test]
    fn scan_begin() {
        let mut s = Scanner {
            stream: String::from("begin"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Begin);
    }

    #[test]
    fn scan_endprogram() {
        let mut s = Scanner {
            stream: String::from("end program"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::EndProgram);
    }

    #[test]
    fn scan_endprocedure() {
        let mut s = Scanner {
            stream: String::from("end procedure"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::EndProcedure);
    }

    #[test]
    fn scan_endif() {
        let mut s = Scanner {
            stream: String::from("end if"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::EndIf);
    }

    #[test]
    fn scan_endfor() {
        let mut s = Scanner {
            stream: String::from("end for"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::EndFor);
    }

    #[test]
    fn scan_if() {
        let mut s = Scanner {
            stream: String::from("if"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::If);
    }

    #[test]
    fn scan_then() {
        let mut s = Scanner {
            stream: String::from("then"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Then);
    }

    #[test]
    fn scan_else() {
        let mut s = Scanner {
            stream: String::from("else"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Else);
    }

    #[test]
    fn scan_for() {
        let mut s = Scanner {
            stream: String::from("for"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::For);
    }

    #[test]
    fn scan_return() {
        let mut s = Scanner {
            stream: String::from("return"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Return);
    }

    #[test]
    fn scan_period() {
        let mut s = Scanner {
            stream: String::from("."),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Period);
    }

    #[test]
    fn scan_semicolon() {
        let mut s = Scanner {
            stream: String::from(";"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Semicolon);
    }

    #[test]
    fn scan_colon() {
        let mut s = Scanner {
            stream: String::from(":"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Colon);
    }

    #[test]
    fn scan_leftparen() {
        let mut s = Scanner {
            stream: String::from("("),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::LeftParen);
    }

    #[test]
    fn scan_rightparen() {
        let mut s = Scanner {
            stream: String::from(")"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::RightParen);
    }

    #[test]
    fn scan_leftsquare() {
        let mut s = Scanner {
            stream: String::from("["),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::LeftSquare);
    }

    #[test]
    fn scan_rightsquare() {
        let mut s = Scanner {
            stream: String::from("]"),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::RightSquare);
    }

    #[test]
    fn scan_assignment() {
        let mut s = Scanner {
            stream: String::from(":="),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Assign);
    }

    #[test]
    fn scan_comma() {
        let mut s = Scanner {
            stream: String::from(","),
            i: 0,
        };
        let tok = s.scan();
        assert!(tok == Token::Comma);
    }
}
