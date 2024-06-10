use crate::token::Token;

use std::collections::HashMap;

pub trait Scan {
    fn scan(&mut self) -> Token;
    fn line(&self) -> u32;
}

pub struct Scanner {
    stream: String,
    i: usize,
    reserved_words: HashMap<String, Token>,
    line: u32,
}

impl Scanner {
    pub fn new(contents: String) -> Scanner {
        let mut reserved_words = HashMap::new();

        reserved_words.insert(String::from("program"), Token::Program);
        reserved_words.insert(String::from("is"), Token::Is);
        reserved_words.insert(String::from("global"), Token::Global);
        reserved_words.insert(String::from("procedure"), Token::Procedure);
        reserved_words.insert(String::from("variable"), Token::Variable);
        reserved_words.insert(String::from("begin"), Token::Begin);
        reserved_words.insert(String::from("if"), Token::If);
        reserved_words.insert(String::from("then"), Token::Then);
        reserved_words.insert(String::from("else"), Token::Else);
        reserved_words.insert(String::from("for"), Token::For);
        reserved_words.insert(String::from("return"), Token::Return);
        reserved_words.insert(String::from("end program"), Token::EndProgram);
        reserved_words.insert(String::from("end procedure"), Token::EndProcedure);
        reserved_words.insert(String::from("end if"), Token::EndIf);
        reserved_words.insert(String::from("end for"), Token::EndFor);
        reserved_words.insert(String::from("true"), Token::True);
        reserved_words.insert(String::from("false"), Token::False);
        reserved_words.insert(String::from("not"), Token::Not);
        reserved_words.insert(String::from("integer"), Token::IntType);
        reserved_words.insert(String::from("float"), Token::FloatType);
        reserved_words.insert(String::from("string"), Token::StringType);
        reserved_words.insert(String::from("bool"), Token::BoolType);

        Scanner {
            stream: contents,
            i: 0,
            reserved_words: reserved_words,
            line: 1,
        }
    }

    // Ignore spaces, tabs, and newlines
    fn skip_whitespace(&mut self) -> bool {
        let start = self.i;
        while let Some(c) = self.read_ch() {
            if c == '\n' {
                self.line += 1;
                continue;
            } else if c == ' ' || c == '\t' || c == '\r' {
                continue;
            } else {
                self.unread_ch();
                break;
            }
        }
        start != self.i
    }

    fn skip_comment(&mut self) -> bool {
        let start = self.i;
        if let Some(c) = self.read_ch() {
            if c == '/' {
                if let Some(c) = self.read_ch() {
                    if c == '/' { // Ignore single-line comments
                        while let Some(c) = self.read_ch() {
                            // skip everything until newline
                            if c != '\n' {
                                continue;
                            } else {
                                self.line += 1;
                                break;
                            }
                        }
                    }
                    else if c == '*' { // Ignore multi-line comments
                        let mut nesting = 1;
                        while let Some(c) = self.read_ch() {
                            if c == '/' {
                                if let Some(c) = self.read_ch() {
                                    if c == '*' {
                                        // nested multi-line comment
                                        nesting += 1;
                                    }
                                }
                            } else if c == '*' {
                                if let Some(c) = self.read_ch() {
                                    if c == '/' {
                                        // end multi-line comment
                                        nesting -= 1;
                                        if nesting == 0 {
                                            break;
                                        }
                                    }
                                }
                            } else if c == '\n' {
                                self.line += 1;
                                continue;
                            } else {
                                continue;
                            }
                        }
                    }
                    else {
                        self.unread_ch();
                        self.unread_ch();
                    }
                } else {
                    self.unread_ch();
                }
            }
            else {
                self.unread_ch();
            }
        }
        start != self.i
    }

    fn skip_whitespace_and_comments(&mut self) {
        while self.skip_whitespace() || self.skip_comment() {}
    }

    fn is_digit(&self, c: char) -> bool {
        if c >= '0' && c <= '9' {
            return true;
        }
        return false;
    }

    fn is_alpha(&self, c: char) -> bool {
        if c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' {
            return true;
        }
        return false;
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        if self.is_digit(c) || self.is_alpha(c) {
            return true;
        }
        return false;
    }

    fn read_ch(&mut self) -> Option<char> {
        if self.i >= self.stream.len() {
            return None;
        }
        let c = self.stream.chars().nth(self.i).unwrap();
        self.i += 1;
        return Some(c);
    }

    fn unread_ch(&mut self) {
        self.i -= 1;
    }

    fn peek_matches(&self, peek: usize, c: char) -> bool {
        peek < self.stream.len() && self.stream.chars().nth(peek).unwrap() == c
    }
}

impl Scan for Scanner {
    fn line(&self) -> u32 {
        self.line
    }

    fn scan(&mut self) -> Token {
        self.skip_whitespace_and_comments();

        let c = match self.read_ch() {
            Some(c) => c,
            None => return Token::EOF,
        };

        let start = self.i-1;
        match c {
            // identifier and keywords
            'a'..='z' | 'A'..='Z' => {
                while let Some(c) = self.read_ch() {
                    if self.is_alphanumeric(c) || c == '_' {
                        continue;
                    } else {
                        self.unread_ch();
                        break;
                    }
                }

                let mut slice = &self.stream[start..self.i];
                if slice.to_lowercase() == "end" {
                    let mut peek = self.i;
                    if self.peek_matches(peek, ' ') {
                        peek += 1;
                        if self.peek_matches(peek, 'f') || self.peek_matches(peek, 'F') {
                            let end = self.i + 4;
                            if end - 1 < self.stream.len() && &self.stream[start..end].to_lowercase() == "end for" {
                                slice = &self.stream[start..end];
                                self.i = end;
                            }
                        }
                        else if self.peek_matches(peek, 'i') || self.peek_matches(peek, 'I') {
                            let end = self.i + 3;
                            if end - 1 < self.stream.len() && &self.stream[start..end].to_lowercase() == "end if" {
                                slice = &self.stream[start..end];
                                self.i = end;
                            }
                        }
                        else if self.peek_matches(peek, 'p') || self.peek_matches(peek, 'P') {
                            peek += 3;
                            if self.peek_matches(peek, 'c') || self.peek_matches(peek, 'C') {
                                let end = self.i + 10;
                                if end - 1 < self.stream.len() && &self.stream[start..end].to_lowercase() == "end procedure" {
                                    slice = &self.stream[start..end];
                                    self.i = end;
                                }
                            }
                            else if self.peek_matches(peek, 'g') {
                                let end = self.i + 8;
                                if end - 1 < self.stream.len() && &self.stream[start..end].to_lowercase() == "end program" {
                                    slice = &self.stream[start..end];
                                    self.i = end;
                                }
                            }
                        }
                    }
                }

                match self.reserved_words.get(&slice.to_lowercase()) {
                    Some(tok) => return tok.clone(),
                    _ => (),
                }

                return Token::Identifier(String::from(slice));
            },

            // number
            '0'..='9' => {
                while let Some(c) = self.read_ch() {
                    if self.is_digit(c) || c == '_' {
                        continue;
                    } else if c == '.' {
                        while let Some(c) = self.read_ch() {
                            if self.is_digit(c) || c == '_' {
                                continue;
                            } else {
                                self.unread_ch();
                                break;
                            }
                        }
                        break;
                    } else {
                        self.unread_ch();
                        break;
                    }
                }

                let slice = &self.stream[start..self.i];
                let slice = slice.replace("_", "");
                let num: f32 = match slice.parse() {
                    Ok(v) => v,
                    Err(_) => panic!("Error parsing number"),
                };

                if !slice.contains(".") {
                    return Token::IntLiteral(num as i32);
                }

                return Token::FloatLiteral(num);
            },

            // string
            '"' => {
                let mut c = match self.read_ch() {
                    Some(c) => c,
                    None => {
                        let slice = &self.stream[start..self.i];
                        return Token::Invalid(String::from(slice));
                    },
                };
                while c != '"' {
                    c = match self.read_ch() {
                        Some(c) => c,
                        None => {
                            let slice = &self.stream[start..self.i];
                            return Token::Invalid(String::from(slice));
                        },
                    };
                }
                let slice = &self.stream[start+1..self.i-1];
                Token::String(String::from(slice))
            },

            // assignment and colon
            ':' => {
                match self.read_ch() {
                    Some(c) => {
                        match c {
                            '=' => Token::Assign,
                            _ => {
                                self.unread_ch();
                                Token::Colon
                            },
                        }
                    },
                    None => Token::Colon,
                }
            },

            ';' => Token::Semicolon,
            '.' => Token::Period,
            ',' => Token::Comma,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '[' => Token::LSquare,
            ']' => Token::RSquare,
            '+' => Token::Add,
            '-' => Token::Sub,
            '*' => Token::Mul,
            '/' => Token::Div,
            '&' => Token::And,
            '|' => Token::Or,
            // lt and lte
            '<' => {
                match self.read_ch() {
                    Some(c) => {
                        match c {
                            '=' => Token::LTE,
                            _ => {
                                self.unread_ch();
                                Token::LT
                            },
                        }
                    },
                    None => Token::LT,
                }
            },

            // gt and gte
            '>' => {
                match self.read_ch() {
                    Some(c) => {
                        match c {
                            '=' => Token::GTE,
                            _ => {
                                self.unread_ch();
                                Token::GT
                            },
                        }
                    },
                    None => Token::GT,
                }
            },

            // assign and equality
            '=' => {
                match self.read_ch() {
                    Some(c) => {
                        match c {
                            '=' => Token::Eq,
                            _ => {
                                self.unread_ch();
                                Token::Invalid(String::from("="))
                            },
                        }
                    },
                    None => Token::Invalid(String::from("="))
                }
            },

            // not equal
            '!' => {
                match self.read_ch() {
                    Some(c) => {
                        match c {
                            '=' => Token::NotEq,
                            _ => {
                                self.unread_ch();
                                Token::Invalid(String::from("!"))
                            },
                        }
                    },
                    None => Token::Invalid(String::from("!"))
                }
            },

            // invalid char
            _ => {
                let slice = &self.stream[start..self.i];
                Token::Invalid(String::from(slice))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_add() {
        let mut s = Scanner::new(String::from("+"));
        let tok = s.scan();
        assert_eq!(tok, Token::Add);
    }

    #[test]
    fn scan_sub() {
        let mut s = Scanner::new(String::from("-"));
        let tok = s.scan();
        assert_eq!(tok, Token::Sub);
    }

    #[test]
    fn scan_mul() {
        let mut s = Scanner::new(String::from("*"));
        let tok = s.scan();
        assert_eq!(tok, Token::Mul);
    }

    #[test]
    fn scan_div() {
        let mut s = Scanner::new(String::from("/"));
        let tok = s.scan();
        assert_eq!(tok, Token::Div);
    }

    #[test]
    fn scan_lt() {
        let mut s = Scanner::new(String::from("<"));
        let tok = s.scan();
        assert_eq!(tok, Token::LT);
    }

    #[test]
    fn scan_lte() {
        let mut s = Scanner::new(String::from("<="));
        let tok = s.scan();
        assert_eq!(tok, Token::LTE);
    }

    #[test]
    fn scan_gt() {
        let mut s = Scanner::new(String::from(">"));
        let tok = s.scan();
        assert_eq!(tok, Token::GT);
    }

    #[test]
    fn scan_gte() {
        let mut s = Scanner::new(String::from(">="));
        let tok = s.scan();
        assert_eq!(tok, Token::GTE);
    }

    #[test]
    fn scan_equal() {
        let mut s = Scanner::new(String::from("=="));
        let tok = s.scan();
        assert_eq!(tok, Token::Eq);
    }

    #[test]
    fn scan_notequal() {
        let mut s = Scanner::new(String::from("!="));
        let tok = s.scan();
        assert_eq!(tok, Token::NotEq);
    }

    #[test]
    fn scan_and() {
        let mut s = Scanner::new(String::from("&"));
        let tok = s.scan();
        assert_eq!(tok, Token::And);
    }

    #[test]
    fn scan_or() {
        let mut s = Scanner::new(String::from("|"));
        let tok = s.scan();
        assert_eq!(tok, Token::Or);
    }

    #[test]
    fn scan_not() {
        let mut s = Scanner::new(String::from("not"));
        let tok = s.scan();
        assert_eq!(tok, Token::Not);
    }

    #[test]
    fn scan_bool_true() {
        let mut s = Scanner::new(String::from("true"));
        let tok = s.scan();
        assert_eq!(tok, Token::True);
    }

    #[test]
    fn scan_bool_false() {
        let mut s = Scanner::new(String::from("false"));
        let tok = s.scan();
        assert_eq!(tok, Token::False);
    }

    #[test]
    fn scan_type_integer() {
        let mut s = Scanner::new(String::from("integer"));
        let tok = s.scan();
        assert_eq!(tok, Token::IntType);
    }

    #[test]
    fn scan_type_float() {
        let mut s = Scanner::new(String::from("float"));
        let tok = s.scan();
        assert_eq!(tok, Token::FloatType);
    }

    #[test]
    fn scan_type_string() {
        let mut s = Scanner::new(String::from("string"));
        let tok = s.scan();
        assert_eq!(tok, Token::StringType);
    }

    #[test]
    fn scan_type_bool() {
        let mut s = Scanner::new(String::from("bool"));
        let tok = s.scan();
        assert_eq!(tok, Token::BoolType);
    }

    #[test]
    fn scan_program() {
        let mut s = Scanner::new(String::from("program"));
        let tok = s.scan();
        assert_eq!(tok, Token::Program);
    }

    #[test]
    fn scan_is() {
        let mut s = Scanner::new(String::from("is"));
        let tok = s.scan();
        assert_eq!(tok, Token::Is);
    }

    #[test]
    fn scan_global() {
        let mut s = Scanner::new(String::from("global"));
        let tok = s.scan();
        assert_eq!(tok, Token::Global);
    }

    #[test]
    fn scan_procedure() {
        let mut s = Scanner::new(String::from("procedure"));
        let tok = s.scan();
        assert_eq!(tok, Token::Procedure);
    }

    #[test]
    fn scan_variable() {
        let mut s = Scanner::new(String::from("variable"));
        let tok = s.scan();
        assert_eq!(tok, Token::Variable);
    }

    #[test]
    fn scan_begin() {
        let mut s = Scanner::new(String::from("begin"));
        let tok = s.scan();
        assert_eq!(tok, Token::Begin);
    }

    #[test]
    fn scan_endprogram() {
        let mut s = Scanner::new(String::from("end program"));
        let tok = s.scan();
        assert_eq!(tok, Token::EndProgram);
    }

    #[test]
    fn scan_endprocedure() {
        let mut s = Scanner::new(String::from("end procedure"));
        let tok = s.scan();
        assert_eq!(tok, Token::EndProcedure);
    }

    #[test]
    fn scan_endif() {
        let mut s = Scanner::new(String::from("end if"));
        let tok = s.scan();
        assert_eq!(tok, Token::EndIf);
    }

    #[test]
    fn scan_endfor() {
        let mut s = Scanner::new(String::from("end for"));
        let tok = s.scan();
        assert_eq!(tok, Token::EndFor);
    }

    #[test]
    fn scan_if() {
        let mut s = Scanner::new(String::from("if"));
        let tok = s.scan();
        assert_eq!(tok, Token::If);
    }

    #[test]
    fn scan_then() {
        let mut s = Scanner::new(String::from("then"));
        let tok = s.scan();
        assert_eq!(tok, Token::Then);
    }

    #[test]
    fn scan_else() {
        let mut s = Scanner::new(String::from("else"));
        let tok = s.scan();
        assert_eq!(tok, Token::Else);
    }

    #[test]
    fn scan_for() {
        let mut s = Scanner::new(String::from("for"));
        let tok = s.scan();
        assert_eq!(tok, Token::For);
    }

    #[test]
    fn scan_return() {
        let mut s = Scanner::new(String::from("return"));
        let tok = s.scan();
        assert_eq!(tok, Token::Return);
    }

    #[test]
    fn scan_period() {
        let mut s = Scanner::new(String::from("."));
        let tok = s.scan();
        assert_eq!(tok, Token::Period);
    }

    #[test]
    fn scan_semicolon() {
        let mut s = Scanner::new(String::from(";"));
        let tok = s.scan();
        assert_eq!(tok, Token::Semicolon);
    }

    #[test]
    fn scan_colon() {
        let mut s = Scanner::new(String::from(":"));
        let tok = s.scan();
        assert_eq!(tok, Token::Colon);
    }

    #[test]
    fn scan_leftparen() {
        let mut s = Scanner::new(String::from("("));
        let tok = s.scan();
        assert_eq!(tok, Token::LParen);
    }

    #[test]
    fn scan_rightparen() {
        let mut s = Scanner::new(String::from(")"));
        let tok = s.scan();
        assert_eq!(tok, Token::RParen);
    }

    #[test]
    fn scan_leftsquare() {
        let mut s = Scanner::new(String::from("["));
        let tok = s.scan();
        assert_eq!(tok, Token::LSquare);
    }

    #[test]
    fn scan_rightsquare() {
        let mut s = Scanner::new(String::from("]"));
        let tok = s.scan();
        assert_eq!(tok, Token::RSquare);
    }

    #[test]
    fn scan_assignment() {
        let mut s = Scanner::new(String::from(":="));
        let tok = s.scan();
        assert_eq!(tok, Token::Assign);
    }

    #[test]
    fn scan_comma() {
        let mut s = Scanner::new(String::from(","));
        let tok = s.scan();
        assert_eq!(tok, Token::Comma);
    }

    #[test]
    fn scan_intliteral_singledigit() {
        let mut s = Scanner::new(String::from("9"));
        let tok = s.scan();
        match tok {
            Token::IntLiteral(val) => assert_eq!(val, 9),
            _ => panic!("Expected Token::IntLiteral")
        }
    }

    #[test]
    fn scan_intliteral_multidigit() {
        let mut s = Scanner::new(String::from("23459"));
        let tok = s.scan();
        match tok {
            Token::IntLiteral(val) => assert_eq!(val, 23459),
            _ => panic!("Expected Token::IntLiteral")
        }
    }

    #[test]
    fn scan_intliteral_multidigit_underscores() {
        let mut s = Scanner::new(String::from("23_459_"));
        let tok = s.scan();
        match tok {
            Token::IntLiteral(val) => assert_eq!(val, 23459),
            _ => panic!("Expected Token::IntLiteral")
        }
    }

    #[test]
    fn scan_floatliteral_multidigit_decimalpoint() {
        let mut s = Scanner::new(String::from("15429."));
        let tok = s.scan();
        match tok {
            Token::FloatLiteral(val) => assert_eq!(val, 15429.),
            _ => panic!("Expected Token::FloatLiteral")
        }
    }

    #[test]
    fn scan_floatliteral_multidigit_decimalpoint_underscores() {
        let mut s = Scanner::new(String::from("15_42_9._"));
        let tok = s.scan();
        match tok {
            Token::FloatLiteral(val) => assert_eq!(val, 15429.),
            _ => panic!("Expected Token::FloatLiteral")
        }
    }

    #[test]
    fn scan_floatliteral_multidigit_decimalpoint_singledigit() {
        let mut s = Scanner::new(String::from("92345.1"));
        let tok = s.scan();
        match tok {
            Token::FloatLiteral(val) => assert_eq!(val, 92345.1),
            _ => panic!("Expected Token::FloatLiteral")
        }
    }

    #[test]
    fn scan_floatliteral_multidigit_decimalpoint_singledigit_underscores() {
        let mut s = Scanner::new(String::from("9__2345._1_"));
        let tok = s.scan();
        match tok {
            Token::FloatLiteral(val) => assert_eq!(val, 92345.1),
            _ => panic!("Expected Token::FloatLiteral")
        }
    }

    #[test]
    fn scan_floatliteral_multidigit_decimalpoint_multidigit() {
        let mut s = Scanner::new(String::from("9345.23456"));
        let tok = s.scan();
        match tok {
            Token::FloatLiteral(val) => assert_eq!(val, 9345.23456),
            _ => panic!("Expected Token::FloatLiteral")
        }
    }

    #[test]
    fn scan_floatliteral_multidigit_decimalpoint_multidigit_underscores() {
        let mut s = Scanner::new(String::from("934__5_.2_3456_"));
        let tok = s.scan();
        match tok {
            Token::FloatLiteral(val) => assert_eq!(val, 9345.23456),
            _ => panic!("Expected Token::FloatLiteral")
        }
    }

    #[test]
    fn scan_string_empty() {
        let mut s = Scanner::new(String::from("\"\""));
        let tok = s.scan();
        match tok {
            Token::String(val) => assert_eq!(val, String::from("")),
            _ => panic!("Expected Token::String")
        }
    }

    #[test]
    fn scan_string_singlechar() {
        let mut s = Scanner::new(String::from("\"a\""));
        let tok = s.scan();
        match tok {
            Token::String(val) => assert_eq!(val, String::from("a")),
            _ => panic!("Expected Token::String")
        }
    }

    #[test]
    fn scan_string_multichar() {
        let mut s = Scanner::new(String::from("\"asdgqerygsh\""));
        let tok = s.scan();
        match tok {
            Token::String(val) => assert_eq!(val, String::from("asdgqerygsh")),
            _ => panic!("Expected Token::String")
        }
    }

    #[test]
    fn scan_identifier_single_lower() {
        let mut s = Scanner::new(String::from("a"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("a")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_identifier_single_upper() {
        let mut s = Scanner::new(String::from("A"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("A")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_identifier_multi_lower() {
        let mut s = Scanner::new(String::from("awerthsdf"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("awerthsdf")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_identifier_multi_upper() {
        let mut s = Scanner::new(String::from("ASGSDFIWERYHEHA"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("ASGSDFIWERYHEHA")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_identifier_multi_alpha() {
        let mut s = Scanner::new(String::from("aaGWErsGBHq"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("aaGWErsGBHq")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_identifier_multi_alphanumeric() {
        let mut s = Scanner::new(String::from("aa4GWErs467GBHq78"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("aa4GWErs467GBHq78")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_identifier_multi_all() {
        let mut s = Scanner::new(String::from("aa4__GWErs467GBHq7_8"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("aa4__GWErs467GBHq7_8")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_identifier_multi_underscores() {
        let mut s = Scanner::new(String::from("a_____________"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("a_____________")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_eof() {
        let mut s = Scanner::new(String::from(""));
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
    }

    #[test]
    fn scan_eof_repeated() {
        let mut s = Scanner::new(String::from(""));
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
    }

    #[test]
    fn scan_whitespace_spaces() {
        let mut s = Scanner::new(String::from("     a       "));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("a")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_whitespace_tabs() {
        let mut s = Scanner::new(String::from("\t\t\t\ta\t\t\t\t"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("a")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_whitespace_linefeed() {
        let mut s = Scanner::new(String::from("\n\n\na\n\n\n"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("a")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_whitespace_aftersinglelinecomment() {
        let mut s = Scanner::new(String::from("   \t\t // this is a comment\n  \t  a"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("a")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_whitespace_carriagereturn() {
        let mut s = Scanner::new(String::from("\r\r\r\ra\r\r\r\r"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("a")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_comment_single_nolinefeed() {
        let mut s = Scanner::new(String::from("// comment asdf"));
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
    }

    #[test]
    fn scan_comment_single_linefeed() {
        let mut s = Scanner::new(String::from("// comment asdf\n"));
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
    }

    #[test]
    fn scan_comment_single_innermulti() {
        let mut s = Scanner::new(String::from("// comment /* comment /* comment */"));
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
    }

    #[test]
    fn scan_comment_single_nonalphanumeric() {
        let mut s = Scanner::new(String::from("// asdg256 ##&* 2@5 ab hao (())[]][{}} %*_~~????><"));
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
    }

    #[test]
    fn scan_comment_multi_nolinefeed() {
        let mut s = Scanner::new(String::from("/* comment asdf*/"));
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
    }

    #[test]
    fn scan_comment_multi_linefeed() {
        let mut s = Scanner::new(String::from("/* comment asdf\naasdb aagg\nasdgab eg*/"));
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
    }

    #[test]
    fn scan_comment_multi_innersingle() {
        let mut s = Scanner::new(String::from("/* comment asdf // aasdb aagg // asdgab eg*/"));
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
    }

    #[test]
    fn scan_comment_multi_nonalphanumeric() {
        let mut s = Scanner::new(String::from("/* asdg256 ##&* 2@5 ab hao (())[]][{}} %*_~~????><*/"));
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
    }

    #[test]
    fn scan_comment_nestedmulti_nolinefeed() {
        let mut s = Scanner::new(String::from("/* level1 /* level2 /* level3a */ level2 /* level3b */ level 2*/ level1 */"));
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
    }

    #[test]
    fn scan_comment_nestedmulti_linefeed() {
        let mut s = Scanner::new(String::from("/* level1 \n /* level2 /* le\nvel3a */ level2 \n/* level3b */ level \n2*/ level1 */"));
        let tok = s.scan();
        assert_eq!(tok, Token::EOF);
    }

    #[test]
    fn scan_invalid_underscore() {
        let mut s = Scanner::new(String::from("_"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("_")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_equals_sign() {
        let mut s = Scanner::new(String::from("="));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("=")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_exclamation_mark() {
        let mut s = Scanner::new(String::from("!"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("!")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_at_sign() {
        let mut s = Scanner::new(String::from("@"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("@")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_number_sign() {
        let mut s = Scanner::new(String::from("#"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("#")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_percent_sign() {
        let mut s = Scanner::new(String::from("%"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("%")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_circumflex() {
        let mut s = Scanner::new(String::from("^"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("^")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_tilde() {
        let mut s = Scanner::new(String::from("~"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("~")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_backtick() {
        let mut s = Scanner::new(String::from("`"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("`")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_leftcurly() {
        let mut s = Scanner::new(String::from("{"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("{")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_rightcurly() {
        let mut s = Scanner::new(String::from("}"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("}")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_backslash() {
        let mut s = Scanner::new(String::from("\\"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("\\")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_question_mark() {
        let mut s = Scanner::new(String::from("?"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("?")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_single_quote() {
        let mut s = Scanner::new(String::from("'"));
        let tok = s.scan();
        match tok {
            Token::Invalid(c) => assert_eq!(c, String::from("'")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_invalid_eofmidstring() {
        let mut s = Scanner::new(String::from("\"this is a strin"));
        let tok = s.scan();
        match tok {
            Token::Invalid(s) => assert_eq!(s, String::from("\"this is a strin")),
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_linecount_newline() {
        let mut s = Scanner::new(String::from("line_1_identifier \n\n\n line_4_identifier"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => {
                assert_eq!(id, String::from("line_1_identifier"));
                assert_eq!(s.line(), 1);
            }
            _ => panic!("Expected Token::Invalid")
        }
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => {
                assert_eq!(id, String::from("line_4_identifier"));
                assert_eq!(s.line(), 4);
            }
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_linecount_singlelinecomment() {
        let mut s = Scanner::new(String::from("line_1_identifier // this is a comment \n line_2_identifier"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => {
                assert_eq!(id, String::from("line_1_identifier"));
                assert_eq!(s.line(), 1);
            }
            _ => panic!("Expected Token::Invalid")
        }
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => {
                assert_eq!(id, String::from("line_2_identifier"));
                assert_eq!(s.line(), 2);
            }
            _ => panic!("Expected Token::Invalid")
        }
    }

    #[test]
    fn scan_linecount_multilinecomment() {
        let mut s = Scanner::new(String::from("line_1_identifier /* this\nis\na\ncomment\n*/ line_5_identifier"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => {
                assert_eq!(id, String::from("line_1_identifier"));
                assert_eq!(s.line(), 1);
            }
            _ => panic!("Expected Token::Invalid")
        }
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => {
                assert_eq!(id, String::from("line_5_identifier"));
                assert_eq!(s.line(), 5);
            }
            _ => panic!("Expected Token::Invalid")
        }
    }
}
