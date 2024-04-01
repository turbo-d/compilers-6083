use crate::symtable::SymTable;
use crate::token::Token;

pub struct Scanner {
    stream: String,
    i: usize,
    table: SymTable,
}

impl Scanner {
    pub fn new(contents: String) -> Scanner {
        let mut table = SymTable::new();

        let _ = table.insert(String::from("program"), Token::Program);
        let _ = table.insert(String::from("is"), Token::Is);
        let _ = table.insert(String::from("global"), Token::Global);
        let _ = table.insert(String::from("procedure"), Token::Procedure);
        let _ = table.insert(String::from("variable"), Token::Variable);
        let _ = table.insert(String::from("begin"), Token::Begin);
        let _ = table.insert(String::from("if"), Token::If);
        let _ = table.insert(String::from("then"), Token::Then);
        let _ = table.insert(String::from("else"), Token::Else);
        let _ = table.insert(String::from("for"), Token::For);
        let _ = table.insert(String::from("return"), Token::Return);
        let _ = table.insert(String::from("end program"), Token::EndProgram);
        let _ = table.insert(String::from("end procedure"), Token::EndProcedure);
        let _ = table.insert(String::from("end if"), Token::EndIf);
        let _ = table.insert(String::from("end for"), Token::EndFor);
        let _ = table.insert(String::from("true"), Token::True);
        let _ = table.insert(String::from("false"), Token::False);
        let _ = table.insert(String::from("not"), Token::Not);
        let _ = table.insert(String::from("integer"), Token::IntType);
        let _ = table.insert(String::from("float"), Token::FloatType);
        let _ = table.insert(String::from("string"), Token::StringType);
        let _ = table.insert(String::from("bool"), Token::BoolType);

        Scanner {
            stream: contents,
            i: 0,
            table: table,
        }
    }

    // Ignore spaces, tabs, and newlines
    fn skip_whitespace(&mut self) {
        while self.i < self.stream.len() && (self.stream.chars().nth(self.i).unwrap() == ' ' || self.stream.chars().nth(self.i).unwrap() == '\n' || self.stream.chars().nth(self.i).unwrap() == '\t' || self.stream.chars().nth(self.i).unwrap() == '\r') {
            self.i += 1;
        }
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

                // div
                else {
                    return Token::Div;
                }
            }
        }

        let start = self.i;

        // identifier and keywords
        if self.i < self.stream.len() && self.is_alpha(self.stream.chars().nth(self.i).unwrap()) {
            self.i += 1;
            while self.i < self.stream.len() && (self.is_alphanumeric(self.stream.chars().nth(self.i).unwrap()) || self.stream.chars().nth(self.i).unwrap() == '_') {
                self.i += 1;
            }

            let mut slice = &self.stream[start..self.i];
            if slice.to_lowercase() == "end" {
                let mut peek = self.i;
                if peek < self.stream.len() && self.stream.chars().nth(peek).unwrap() == ' ' {
                    peek += 1;
                    if peek < self.stream.len() && self.stream.chars().nth(peek).unwrap() == 'f' || self.stream.chars().nth(peek).unwrap() == 'F'{
                        let end = self.i + 4;
                        if end - 1 < self.stream.len() && &self.stream[start..end].to_lowercase() == "end for" {
                            slice = &self.stream[start..end];
                            self.i = end;
                        }
                    }
                    else if peek < self.stream.len() && self.stream.chars().nth(peek).unwrap() == 'i' || self.stream.chars().nth(peek).unwrap() == 'I' {
                        let end = self.i + 3;
                        if end - 1 < self.stream.len() && &self.stream[start..end].to_lowercase() == "end if" {
                            slice = &self.stream[start..end];
                            self.i = end;
                        }
                    }
                    else if peek < self.stream.len() && self.stream.chars().nth(peek).unwrap() == 'p' || self.stream.chars().nth(peek).unwrap() == 'P' {
                        peek += 3;
                        if peek < self.stream.len() && self.stream.chars().nth(peek).unwrap() == 'c' || self.stream.chars().nth(peek).unwrap() == 'C' {
                            let end = self.i + 10;
                            if end - 1 < self.stream.len() && &self.stream[start..end].to_lowercase() == "end procedure" {
                                slice = &self.stream[start..end];
                                self.i = end;
                            }
                        }
                        else if peek < self.stream.len() && self.stream.chars().nth(peek).unwrap() == 'g' {
                            let end = self.i + 8;
                            if end - 1 < self.stream.len() && &self.stream[start..end].to_lowercase() == "end program" {
                                slice = &self.stream[start..end];
                                self.i = end;
                            }
                        }
                    }
                }
            }

            match self.table.get(&slice.to_lowercase()) {
                Some(tok) => return tok.clone(),
                _ => (),
            }

            return Token::Identifier(String::from(slice.to_lowercase()));
        }

        // number
        else if self.i < self.stream.len() && self.is_digit(self.stream.chars().nth(self.i).unwrap()) {
            while self.i < self.stream.len() && self.is_digit(self.stream.chars().nth(self.i).unwrap()) {
                self.i += 1;
            }
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '.' {
                self.i += 1;
                while self.i < self.stream.len() && self.is_digit(self.stream.chars().nth(self.i).unwrap()) {
                    self.i += 1;
                }
            }
            let slice = &self.stream[start..self.i];
            return Token::Number(String::from(slice));
        }

        // string
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '"' {
            self.i += 1;
            while self.i < self.stream.len() && (self.stream.chars().nth(self.i).unwrap() != '"') {
                self.i += 1;
            }
            self.i += 1;
            let slice = &self.stream[start..self.i];
            return Token::String(String::from(slice));
        }

        // assignment and colon
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ':' {
            self.i += 1;
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
                self.i += 1;
                return Token::Assign;
            }
            return Token::Colon;
        }

        // semicolon
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ';' {
            self.i += 1;
            return Token::Semicolon;
        }

       // period
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '.' {
            self.i += 1;
            return Token::Period;
        }

       // comma
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ',' {
            self.i += 1;
            return Token::Comma;
        }

        // left parenthesis
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '(' {
            self.i += 1;
            return Token::LParen;
        }

        // right parenthesis
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ')' {
            self.i += 1;
            return Token::RParen;
        }

        // left square bracket
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '[' {
            self.i += 1;
            return Token::LSquare;
        }

        // right square bracket
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ']' {
            self.i += 1;
            return Token::RSquare;
        }

        // plus
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '+' {
            self.i += 1;
            return Token::Add;
        }

        // minus
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '-' {
            self.i += 1;
            return Token::Sub;
        }

        // multiply
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '*' {
            self.i += 1;
            return Token::Mul;
        }

        // and
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '&' {
            self.i += 1;
            return Token::And;
        }

        // or
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '|' {
            self.i += 1;
            return Token::Or;
        }

        // lt and lte
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '<' {
            self.i += 1;
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
                self.i += 1;
                return Token::LTE;
            }
            return Token::LT;
        }

        // gt and gte
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '>' {
            self.i += 1;
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
                self.i += 1;
                return Token::GTE;
            }
            return Token::GT;
        }

        // assign and equality
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
            self.i += 1;
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
                self.i += 1;
                return Token::Eq;
            }
        }

        // not equal
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '!' {
            self.i += 1;
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
                self.i += 1;
                return Token::NotEq;
            }
        }

        // invalid char (this must be the last else if)
        else if self.i < self.stream.len() {
            eprintln!("invalid character");
            // return error with message so that main can exit
            // do we continue scanning?
            //std::process::exit(1);
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
        assert!(tok == Token::Add);
    }

    #[test]
    fn scan_sub() {
        let mut s = Scanner::new(String::from("-"));
        let tok = s.scan();
        assert!(tok == Token::Sub);
    }

    #[test]
    fn scan_mul() {
        let mut s = Scanner::new(String::from("*"));
        let tok = s.scan();
        assert!(tok == Token::Mul);
    }

    #[test]
    fn scan_div() {
        let mut s = Scanner::new(String::from("/"));
        let tok = s.scan();
        assert!(tok == Token::Div);
    }

    #[test]
    fn scan_lt() {
        let mut s = Scanner::new(String::from("<"));
        let tok = s.scan();
        assert!(tok == Token::LT);
    }

    #[test]
    fn scan_lte() {
        let mut s = Scanner::new(String::from("<="));
        let tok = s.scan();
        assert!(tok == Token::LTE);
    }

    #[test]
    fn scan_gt() {
        let mut s = Scanner::new(String::from(">"));
        let tok = s.scan();
        assert!(tok == Token::GT);
    }

    #[test]
    fn scan_gte() {
        let mut s = Scanner::new(String::from(">="));
        let tok = s.scan();
        assert!(tok == Token::GTE);
    }

    #[test]
    fn scan_equal() {
        let mut s = Scanner::new(String::from("=="));
        let tok = s.scan();
        assert!(tok == Token::Eq);
    }

    #[test]
    fn scan_notequal() {
        let mut s = Scanner::new(String::from("!="));
        let tok = s.scan();
        assert!(tok == Token::NotEq);
    }

    #[test]
    fn scan_and() {
        let mut s = Scanner::new(String::from("&"));
        let tok = s.scan();
        assert!(tok == Token::And);
    }

    #[test]
    fn scan_or() {
        let mut s = Scanner::new(String::from("|"));
        let tok = s.scan();
        assert!(tok == Token::Or);
    }

    #[test]
    fn scan_not() {
        let mut s = Scanner::new(String::from("not"));
        let tok = s.scan();
        assert!(tok == Token::Not);
    }

    #[test]
    fn scan_bool_true() {
        let mut s = Scanner::new(String::from("true"));
        let tok = s.scan();
        assert!(tok == Token::True);
    }

    #[test]
    fn scan_bool_false() {
        let mut s = Scanner::new(String::from("false"));
        let tok = s.scan();
        assert!(tok == Token::False);
    }

    #[test]
    fn scan_type_integer() {
        let mut s = Scanner::new(String::from("integer"));
        let tok = s.scan();
        assert!(tok == Token::IntType);
    }

    #[test]
    fn scan_type_float() {
        let mut s = Scanner::new(String::from("float"));
        let tok = s.scan();
        assert!(tok == Token::FloatType);
    }

    #[test]
    fn scan_type_string() {
        let mut s = Scanner::new(String::from("string"));
        let tok = s.scan();
        assert!(tok == Token::StringType);
    }

    #[test]
    fn scan_type_bool() {
        let mut s = Scanner::new(String::from("bool"));
        let tok = s.scan();
        assert!(tok == Token::BoolType);
    }

    #[test]
    fn scan_program() {
        let mut s = Scanner::new(String::from("program"));
        let tok = s.scan();
        assert!(tok == Token::Program);
    }

    #[test]
    fn scan_is() {
        let mut s = Scanner::new(String::from("is"));
        let tok = s.scan();
        assert!(tok == Token::Is);
    }

    #[test]
    fn scan_global() {
        let mut s = Scanner::new(String::from("global"));
        let tok = s.scan();
        assert!(tok == Token::Global);
    }

    #[test]
    fn scan_procedure() {
        let mut s = Scanner::new(String::from("procedure"));
        let tok = s.scan();
        assert!(tok == Token::Procedure);
    }

    #[test]
    fn scan_variable() {
        let mut s = Scanner::new(String::from("variable"));
        let tok = s.scan();
        assert!(tok == Token::Variable);
    }

    #[test]
    fn scan_begin() {
        let mut s = Scanner::new(String::from("begin"));
        let tok = s.scan();
        assert!(tok == Token::Begin);
    }

    #[test]
    fn scan_endprogram() {
        let mut s = Scanner::new(String::from("end program"));
        let tok = s.scan();
        assert!(tok == Token::EndProgram);
    }

    #[test]
    fn scan_endprocedure() {
        let mut s = Scanner::new(String::from("end procedure"));
        let tok = s.scan();
        assert!(tok == Token::EndProcedure);
    }

    #[test]
    fn scan_endif() {
        let mut s = Scanner::new(String::from("end if"));
        let tok = s.scan();
        assert!(tok == Token::EndIf);
    }

    #[test]
    fn scan_endfor() {
        let mut s = Scanner::new(String::from("end for"));
        let tok = s.scan();
        assert!(tok == Token::EndFor);
    }

    #[test]
    fn scan_if() {
        let mut s = Scanner::new(String::from("if"));
        let tok = s.scan();
        assert!(tok == Token::If);
    }

    #[test]
    fn scan_then() {
        let mut s = Scanner::new(String::from("then"));
        let tok = s.scan();
        assert!(tok == Token::Then);
    }

    #[test]
    fn scan_else() {
        let mut s = Scanner::new(String::from("else"));
        let tok = s.scan();
        assert!(tok == Token::Else);
    }

    #[test]
    fn scan_for() {
        let mut s = Scanner::new(String::from("for"));
        let tok = s.scan();
        assert!(tok == Token::For);
    }

    #[test]
    fn scan_return() {
        let mut s = Scanner::new(String::from("return"));
        let tok = s.scan();
        assert!(tok == Token::Return);
    }

    #[test]
    fn scan_period() {
        let mut s = Scanner::new(String::from("."));
        let tok = s.scan();
        assert!(tok == Token::Period);
    }

    #[test]
    fn scan_semicolon() {
        let mut s = Scanner::new(String::from(";"));
        let tok = s.scan();
        assert!(tok == Token::Semicolon);
    }

    #[test]
    fn scan_colon() {
        let mut s = Scanner::new(String::from(":"));
        let tok = s.scan();
        assert!(tok == Token::Colon);
    }

    #[test]
    fn scan_leftparen() {
        let mut s = Scanner::new(String::from("("));
        let tok = s.scan();
        assert!(tok == Token::LParen);
    }

    #[test]
    fn scan_rightparen() {
        let mut s = Scanner::new(String::from(")"));
        let tok = s.scan();
        assert!(tok == Token::RParen);
    }

    #[test]
    fn scan_leftsquare() {
        let mut s = Scanner::new(String::from("["));
        let tok = s.scan();
        assert!(tok == Token::LSquare);
    }

    #[test]
    fn scan_rightsquare() {
        let mut s = Scanner::new(String::from("]"));
        let tok = s.scan();
        assert!(tok == Token::RSquare);
    }

    #[test]
    fn scan_assignment() {
        let mut s = Scanner::new(String::from(":="));
        let tok = s.scan();
        assert!(tok == Token::Assign);
    }

    #[test]
    fn scan_comma() {
        let mut s = Scanner::new(String::from(","));
        let tok = s.scan();
        assert!(tok == Token::Comma);
    }

    #[test]
    fn scan_number_singledigit() {
        let mut s = Scanner::new(String::from("9"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Number(_)));
    }

    #[test]
    fn scan_number_multidigit() {
        let mut s = Scanner::new(String::from("23459"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Number(_)));
    }

    #[test]
    fn scan_number_multidigit_decimalpoint() {
        let mut s = Scanner::new(String::from("15429."));
        let tok = s.scan();
        assert!(matches!(tok, Token::Number(_)));
    }

    #[test]
    fn scan_number_multidigit_decimalpoint_singledigit() {
        let mut s = Scanner::new(String::from("92345.1"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Number(_)));
    }

    #[test]
    fn scan_number_multidigit_decimalpoint_multidigit() {
        let mut s = Scanner::new(String::from("9345.23456"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Number(_)));
    }

    #[test]
    fn scan_string_empty() {
        let mut s = Scanner::new(String::from("\"\""));
        let tok = s.scan();
        assert!(matches!(tok, Token::String(_)));
    }

    #[test]
    fn scan_string_singlechar() {
        let mut s = Scanner::new(String::from("\"a\""));
        let tok = s.scan();
        assert!(matches!(tok, Token::String(_)));
    }

    #[test]
    fn scan_string_multichar() {
        let mut s = Scanner::new(String::from("\"asdgqerygsh\""));
        let tok = s.scan();
        assert!(matches!(tok, Token::String(_)));
    }

    #[test]
    fn scan_identifier_single_lower() {
        let mut s = Scanner::new(String::from("a"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Identifier(_)));
    }

    #[test]
    fn scan_identifier_single_upper() {
        let mut s = Scanner::new(String::from("A"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Identifier(_)));
    }

    #[test]
    fn scan_identifier_multi_lower() {
        let mut s = Scanner::new(String::from("awerthsdf"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Identifier(_)));
    }

    #[test]
    fn scan_identifier_multi_upper() {
        let mut s = Scanner::new(String::from("ASGSDFIWERYHEHA"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Identifier(_)));
    }

    #[test]
    fn scan_identifier_multi_alpha() {
        let mut s = Scanner::new(String::from("aaGWErsGBHq"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Identifier(_)));
    }

    #[test]
    fn scan_identifier_multi_alphanumeric() {
        let mut s = Scanner::new(String::from("aa4GWErs467GBHq78"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Identifier(_)));
    }

    #[test]
    fn scan_identifier_multi_all() {
        let mut s = Scanner::new(String::from("aa4__GWErs467GBHq7_8"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Identifier(_)));
    }

    #[test]
    fn scan_identifier_multi_underscores() {
        let mut s = Scanner::new(String::from("a_____________"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Identifier(_)));
    }

    #[test]
    fn scan_eof() {
        let mut s = Scanner::new(String::from(""));
        let tok = s.scan();
        assert!(tok == Token::EOF);
    }

    #[test]
    fn scan_whitespace_spaces() {
        let mut s = Scanner::new(String::from("     a       "));
        let tok = s.scan();
        assert!(matches!(tok, Token::Identifier(_)));
    }

    #[test]
    fn scan_whitespace_tabs() {
        let mut s = Scanner::new(String::from("\t\t\t\ta\t\t\t\t"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Identifier(_)));
    }

    #[test]
    fn scan_whitespace_linefeed() {
        let mut s = Scanner::new(String::from("\n\n\na\n\n\n"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Identifier(_)));
    }

    #[test]
    fn scan_whitespace_carriagereturn() {
        let mut s = Scanner::new(String::from("\r\r\r\ra\r\r\r\r"));
        let tok = s.scan();
        assert!(matches!(tok, Token::Identifier(_)));
    }

    #[test]
    fn scan_comment_single_nolinefeed() {
        let mut s = Scanner::new(String::from("// comment asdf"));
        let tok = s.scan();
        assert!(tok == Token::EOF);
    }

    #[test]
    fn scan_comment_single_linefeed() {
        let mut s = Scanner::new(String::from("// comment asdf\n"));
        let tok = s.scan();
        assert!(tok == Token::EOF);
    }

    #[test]
    fn scan_comment_single_innermulti() {
        let mut s = Scanner::new(String::from("// comment /* comment /* comment */"));
        let tok = s.scan();
        assert!(tok == Token::EOF);
    }

    #[test]
    fn scan_comment_single_nonalphanumeric() {
        let mut s = Scanner::new(String::from("// asdg256 ##&* 2@5 ab hao (())[]][{}} %*_~~????><"));
        let tok = s.scan();
        assert!(tok == Token::EOF);
    }

    #[test]
    fn scan_comment_multi_nolinefeed() {
        let mut s = Scanner::new(String::from("/* comment asdf*/"));
        let tok = s.scan();
        assert!(tok == Token::EOF);
    }

    #[test]
    fn scan_comment_multi_linefeed() {
        let mut s = Scanner::new(String::from("/* comment asdf\naasdb aagg\nasdgab eg*/"));
        let tok = s.scan();
        assert!(tok == Token::EOF);
    }

    #[test]
    fn scan_comment_multi_innersingle() {
        let mut s = Scanner::new(String::from("/* comment asdf // aasdb aagg // asdgab eg*/"));
        let tok = s.scan();
        assert!(tok == Token::EOF);
    }

    #[test]
    fn scan_comment_multi_nonalphanumeric() {
        let mut s = Scanner::new(String::from("/* asdg256 ##&* 2@5 ab hao (())[]][{}} %*_~~????><*/"));
        let tok = s.scan();
        assert!(tok == Token::EOF);
    }

    #[test]
    fn scan_comment_nestedmulti_nolinefeed() {
        let mut s = Scanner::new(String::from("/* level1 /* level2 /* level3a */ level2 /* level3b */ level 2*/ level1 */"));
        let tok = s.scan();
        assert!(tok == Token::EOF);
    }

    #[test]
    fn scan_comment_nestedmulti_linefeed() {
        let mut s = Scanner::new(String::from("/* level1 \n /* level2 /* le\nvel3a */ level2 \n/* level3b */ level \n2*/ level1 */"));
        let tok = s.scan();
        assert!(tok == Token::EOF);
    }

    #[test]
    fn scan_error_invalidchar() {
        let mut s = Scanner::new(String::from("@"));
        let _tok = s.scan();
        assert!(false);
    }

    // TODO: tests for error cases

    // TODO: tests for case insensitivity
}
