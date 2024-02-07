#[derive(PartialEq)]
pub enum Token {
    EOF,
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
    And,
    Or,
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
}
