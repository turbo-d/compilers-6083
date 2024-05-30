use crate::token::Token;

use std::collections::HashMap;

pub struct Scanner {
    stream: String,
    i: usize,
    reserved_words: HashMap<String, Token>,
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
        }
    }

    // Ignore spaces, tabs, and newlines
    fn skip_whitespace(&mut self) {
        while self.matches(' ') || self.matches('\n') || self.matches('\t') || self.matches('\r') {
            self.i += 1;
        }
    }

    fn is_digit(&self) -> bool {
        if self.i >= self.stream.len() {
            return false;
        }

        let c = self.stream.chars().nth(self.i).unwrap();
        if c >= '0' && c <= '9' {
            return true;
        }

        return false;
    }

    fn is_alpha(&self) -> bool {
        if self.i >= self.stream.len() {
            return false;
        }

        let c = self.stream.chars().nth(self.i).unwrap();
        if c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' {
            return true;
        }

        return false;
    }

    fn is_alphanumeric(&self) -> bool {
        if self.is_digit() || self.is_alpha() {
            return true;
        }

        return false;
    }

    //fn read_ch(&mut self) -> Option<char> {
    //    if self.i <= self.stream.len() {
    //        return None;
    //    }
    //    let c = self.stream.chars().nth(self.i);
    //    self.i += 1;
    //    return c;
    //}

    //fn unread_ch(&mut self) {
    //    self.i -= 1;
    //}

    fn matches(&self, c: char) -> bool {
        self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == c
    }

    fn not_matches(&self, c: char) -> bool {
        self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() != c
    }

    fn peek_matches(&self, peek: usize, c: char) -> bool {
        peek < self.stream.len() && self.stream.chars().nth(peek).unwrap() == c
    }

    pub fn scan(&mut self) -> Token {
        // skip comments and whitespace
        while self.matches(' ') || self.matches('\n') || self.matches('\t') || self.matches('/') || self.matches('\r') {
            self.skip_whitespace();

            // Ignore comments
            if self.matches('/') {
                self.i += 1;

                // Ignore single-line comments
                if self.matches('/') {
                    self.i += 1;

                    // skip everything until newline
                    while self.not_matches('\n') {
                        self.i += 1;
                    }
                }

                // Ignore multi-line comments
                else if self.matches('*') {
                    // start multi-line comment
                    let mut nesting = 1;
                    self.i += 1;

                    while nesting > 0 {
                        // skip everything in between
                        while self.not_matches('/') && self.not_matches('*') {
                            self.i += 1;
                        }

                        // multi-line comment close
                        if self.matches('*') {
                            self.i += 1;
                            if self.matches('/') {
                                // end multi-line comment
                                nesting -= 1;

                                self.i += 1;
                            }
                        }

                        // multi-line comment open
                        else if self.matches('/') {
                            self.i += 1;
                            if self.matches('*') {
                                // nested multi-line comment
                                nesting += 1;

                                self.i += 1;
                            }
                        }
                    }
                }

                // div
                else {
                    return Token::Div;
                }
            }
        }

        let start = self.i;

        // identifier and keywords
        if self.is_alpha() {
            self.i += 1;
            while self.is_alphanumeric() || self.matches('_') {
                self.i += 1;
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

            return Token::Identifier(String::from(slice.to_lowercase()));
        }

        // number
        else if self.is_digit() {
            while self.is_digit() || self.matches('_') {
                self.i += 1;
            }
            if self.matches('.') {
                self.i += 1;
                while self.is_digit() || self.matches('_') {
                    self.i += 1;
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
        }

        // string
        else if self.matches('"') {
            self.i += 1;
            while self.not_matches('"') {
                self.i += 1;
            }
            self.i += 1;
            let slice = &self.stream[start+1..self.i-1];
            return Token::String(String::from(slice));
        }

        // assignment and colon
        else if self.matches(':') {
            self.i += 1;
            if self.matches('=') {
                self.i += 1;
                return Token::Assign;
            }
            return Token::Colon;
        }

        // semicolon
        else if self.matches(';') {
            self.i += 1;
            return Token::Semicolon;
        }

       // period
        else if self.matches('.') {
            self.i += 1;
            return Token::Period;
        }

       // comma
        else if self.matches(',') {
            self.i += 1;
            return Token::Comma;
        }

        // left parenthesis
        else if self.matches('(') {
            self.i += 1;
            return Token::LParen;
        }

        // right parenthesis
        else if self.matches(')') {
            self.i += 1;
            return Token::RParen;
        }

        // left square bracket
        else if self.matches('[') {
            self.i += 1;
            return Token::LSquare;
        }

        // right square bracket
        else if self.matches(']') {
            self.i += 1;
            return Token::RSquare;
        }

        // plus
        else if self.matches('+') {
            self.i += 1;
            return Token::Add;
        }

        // minus
        else if self.matches('-') {
            self.i += 1;
            return Token::Sub;
        }

        // multiply
        else if self.matches('*') {
            self.i += 1;
            return Token::Mul;
        }

        // and
        else if self.matches('&') {
            self.i += 1;
            return Token::And;
        }

        // or
        else if self.matches('|') {
            self.i += 1;
            return Token::Or;
        }

        // lt and lte
        else if self.matches('<') {
            self.i += 1;
            if self.matches('=') {
                self.i += 1;
                return Token::LTE;
            }
            return Token::LT;
        }

        // gt and gte
        else if self.matches('>') {
            self.i += 1;
            if self.matches('=') {
                self.i += 1;
                return Token::GTE;
            }
            return Token::GT;
        }

        // assign and equality
        else if self.matches('=') {
            self.i += 1;
            if self.matches('=') {
                self.i += 1;
                return Token::Eq;
            }
            return Token::Invalid(String::from("="));
        }

        // not equal
        else if self.matches('!') {
            self.i += 1;
            if self.matches('=') {
                self.i += 1;
                return Token::NotEq;
            }
            return Token::Invalid(String::from("!"));
        }

        // invalid char (this must be the last else if)
        else if self.i < self.stream.len() {
            self.i += 1;
            let slice = &self.stream[start..self.i];
            return Token::Invalid(String::from(slice));
        }

        return Token::EOF;
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
            Token::Identifier(id) => assert_eq!(id, String::from("a")),
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
            Token::Identifier(id) => assert_eq!(id, String::from("asgsdfiweryheha")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_identifier_multi_alpha() {
        let mut s = Scanner::new(String::from("aaGWErsGBHq"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("aagwersgbhq")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_identifier_multi_alphanumeric() {
        let mut s = Scanner::new(String::from("aa4GWErs467GBHq78"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("aa4gwers467gbhq78")),
            _ => panic!("Expected Token::Identifier")
        }
    }

    #[test]
    fn scan_identifier_multi_all() {
        let mut s = Scanner::new(String::from("aa4__GWErs467GBHq7_8"));
        let tok = s.scan();
        match tok {
            Token::Identifier(id) => assert_eq!(id, String::from("aa4__gwers467gbhq7_8")),
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

    // TODO: tests for being in the middle of scanning a number, id, or string (or maybe other
    // multi-char tokens) and we hit the end of the char stream
}
