use std::collections::HashMap;

pub enum TokenTag {
    True,
    False,
    Integer,
    Float,
    String,
    Bool,
    Program,
    Is,
    Global,
    Procedure,
    Variable,
    Begin,
    If,
    Then,
    Else,
    For,
    Return,
    EndProgram,
    EndProcedure,
    EndIf,
    EndFor,
    Not,
    Plus,
    Minus,
    Multiply,
    Divide,
}

#[derive(Clone, PartialEq)]
pub enum Token {
    EOF,
    Keyword,
    Type,
    Identifier,
    Number,
    String,
    Colon,
    Semicolon,
    Period,
    Comma,
    LParen,
    RParen,
    LSquare,
    RSquare,
    AddOp,
    MulOp,
    And,
    Or,
    Assign,
    LT,
    LTE,
    GT,
    GTE,
    Eq,
    NotEq,
}

//impl fmt::Display for Token {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//       match self {
//           Token::EOF => write!(f, "EOF"),
//           Token::Identifier => write!(f, "Identifier"),
//           Token::Number => write!(f, "Number"),
//           Token::Colon => write!(f, "Colon"),
//           Token::Semicolon => write!(f, "Semicolon"),
//           Token::LParen => write!(f, "LParen"),
//           Token::RParen => write!(f, "RParen"),
//           Token::LSquare => write!(f, "LSquare"),
//           Token::RSquare => write!(f, "RSquare"),
//           Token::Plus => write!(f, "Plus"),
//           Token::Minus => write!(f, "Minus"),
//           Token::Assign => write!(f, "Assign"),
//       }
//    }
//}


pub struct LLParser {
    s: Scanner,
}

impl LLParser {
    pub fn new(s: Scanner) -> LLParser {
        LLParser {
            s
        }
    }

    pub fn parse(&mut self) {
        self.program()
    }

    fn program(&mut self) {
        self.program_header();

        self.program_body();

        let tok = self.s.scan();
        if tok != Token::Period { // .
            // Probably want to warn here instead
            panic!("Expected \".\"");
        }
    }

    fn program_header(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Keyword { // program keyword
            panic!("Expected \"program\"");
        }

        let tok = self.s.scan();
        if tok != Token::Identifier { // identifier
            panic!("Expected \"identifier\"");
        }

        let tok = self.s.scan();
        if tok != Token::Keyword { // is keyword
            panic!("Expected \"is\"");
        }
    }

    // first: "global", "procedure", "variable", "begin"
    fn program_body(&mut self) {
        // TODO: peek next token
        let tok = self.s.scan();
        while tok == Token::Keyword || tok == Token::Keyword || tok == Token::Keyword { // declaration first: global, procedure, or variable
            self.declaration();

            let tok = self.s.scan();
            if tok != Token::Semicolon { // ;
                panic!("Expected \";\"");
            }
        }

        let tok = self.s.scan();
        if tok != Token::Keyword { // begin keyword
            panic!("Expected \"begin\"");
        }

        // TODO: peek instead
        let tok = self.s.scan();
        while tok == Token::Identifier || tok == Token::Keyword || tok == Token::Keyword || tok == Token::Keyword { // statement first: "identifier", "if", "for", "return"
            self.statement();

            let tok = self.s.scan();
            if tok != Token::Semicolon { // ;
                panic!("Expected \";\"");
            }
        }

        let tok = self.s.scan();
        if tok != Token::Keyword { // end program keyword
            panic!("Expected \"end program\"");
        }
    }

    fn declaration(&mut self) {
        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::Keyword { // global
            // consume global token
            let tok = self.s.scan();
        }

        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::Keyword { // procedure keyword
            self.procedure_declaration();
        } else if tok == Token::Keyword { // variable keyword
            self.variable_declaration();
        } else {
            panic!("Expected \"Procedure declaration or Variable declaration\"");
        }
    }

    fn procedure_declaration(&mut self) {
        self.procedure_header();
        self.procedure_body();
    }

    fn procedure_header(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Keyword { // procedure keyword
            panic!("Expected \"procedure\"");
        }
        let tok = self.s.scan();
        if tok != Token::Identifier { // identifier keyword
            panic!("Expected \"identifier\"");
        }
        let tok = self.s.scan();
        if tok != Token::Colon { // :
            panic!("Expected \":\"");
        }

        self.type_mark();

        let tok = self.s.scan();
        if tok != Token::LParen { // :
            panic!("Expected \"(\"");
        }

        // parameter_list first: "variable"
        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::Keyword { // variable keyword
            self.parameter_list();
        }

        let tok = self.s.scan();
        if tok != Token::RParen { // :
            panic!("Expected \")\"");
        }
    }

    fn type_mark(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Type {
            panic!("Expected \"type\"");
        }
    }

    fn parameter_list(&mut self) {
        self.parameter();

        // TODO: peek token
        let mut peek = self.s.scan();
        while peek == Token::Comma {
            //consume comma
            self.s.scan();

            self.parameter();

            // TODO: peek instead
            peek = self.s.scan();
        }
    }

    fn parameter(&mut self) {
        self.variable_declaration();
    }

    fn procedure_body(&mut self) {
        // TODO: peek next token
        let tok = self.s.scan();
        while tok == Token::Keyword || tok == Token::Keyword || tok == Token::Keyword { // declaration first: global, procedure, or variable
            self.declaration();

            let tok = self.s.scan();
            if tok != Token::Semicolon { // ;
                panic!("Expected \";\"");
            }
        }

        let tok = self.s.scan();
        if tok != Token::Keyword { // begin keyword
            panic!("Expected \"begin\"");
        }

        // TODO: peek instead
        let tok = self.s.scan();
        while tok == Token::Identifier || tok == Token::Keyword || tok == Token::Keyword || tok == Token::Keyword { // statement first: "identifier", "if", "for", "return"
            self.statement();

            let tok = self.s.scan();
            if tok != Token::Semicolon { // ;
                panic!("Expected \";\"");
            }
        }

        let tok = self.s.scan();
        if tok != Token::Keyword { // end program keyword
            panic!("Expected \"end procedure\"");
        }
    }

    fn variable_declaration(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Keyword { // variable keyword
            panic!("Expected \"variable\"");
        }

        let tok = self.s.scan();
        if tok != Token::Identifier { // identifier keyword
            panic!("Expected \"identifier\"");
        }

        let tok = self.s.scan();
        if tok != Token::Colon { // :
            panic!("Expected \":\"");
        }

        self.type_mark();

        // TODO: peek token
        let tok = self.s.scan();
        if tok != Token::LSquare { // :
            return;
        }

        // consume LSquare
        let tok = self.s.scan();

        self.bound();

        let tok = self.s.scan();
        if tok != Token::RSquare { // :
            panic!("Expected \"]\"");
        }
    }

    fn bound(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Number {
            panic!("Expected \"number\"");
        }
    }

    fn statement(&mut self) {
        // statement first: "identifier", "if", "for", "return"
        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::Identifier {
            self.assignment_statement();
        } else if tok == Token::Keyword { // if keyword
            self.if_statement();
        } else if tok == Token::Keyword { // for keyword
            self.loop_statement();
        } else if tok == Token::Keyword { // return keyword
            self.return_statement();
        } else {
            panic!("Expected \"statement\"");
        }
    }

    fn assignment_statement(&mut self) {
        self.destination();

        let tok = self.s.scan();
        if tok != Token::Assign {
            panic!("Expected \":=\"");
        }

        self.expr();
    }

    fn destination(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Identifier {
            panic!("Expected \"identifier\"");
        }

        // TODO: peek token
        let tok = self.s.scan();
        if tok != Token::LSquare { // :
            return;
        }

        // consume LSquare
        let tok = self.s.scan();

        self.expr();

        let tok = self.s.scan();
        if tok != Token::RSquare { // :
            panic!("Expected \"]\"");
        }
    }

    fn if_statement(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Keyword { // if
            panic!("Expected \"if\"");
        }

        self.expr();

        let tok = self.s.scan();
        if tok != Token::Keyword { // then
            panic!("Expected \"then\"");
        }

        // TODO: peek
        let tok = self.s.scan();
        while tok == Token::Keyword { // else
            let tok = self.s.scan();
            if tok != Token::Keyword { // else
                panic!("Expected \"else\"");
            }

            self.statement();

            let tok = self.s.scan();
            if tok != Token::Semicolon { // ;
                panic!("Expected \";\"");
            }
        }

        let tok = self.s.scan();
        if tok != Token::Keyword { // end if
            panic!("Expected \"end if\"");
        }
    }

    fn loop_statement(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Keyword { // for
            panic!("Expected \"for\"");
        }

        let tok = self.s.scan();
        if tok != Token::LParen {
            panic!("Expected \"(\"");
        }

        self.assignment_statement();

        let tok = self.s.scan();
        if tok != Token::Semicolon {
            panic!("Expected \";\"");
        }

        self.expr();

        let tok = self.s.scan();
        if tok != Token::RParen {
            panic!("Expected \")\"");
        }

        // TODO: peek instead
        let tok = self.s.scan();
        while tok == Token::Identifier || tok == Token::Keyword || tok == Token::Keyword || tok == Token::Keyword { // statement first: "identifier", "if", "for", "return"
            self.statement();

            let tok = self.s.scan();
            if tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
        }

        let tok = self.s.scan();
        if tok != Token::Keyword { // end for
            panic!("Expected \"end for\"");
        }

    }

    fn return_statement(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Keyword { // return
            panic!("Expected \"return\"");
        }

        self.expr();
    }

    fn expr(&mut self) {
        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::Keyword { // not
            // consume not token
            let tok = self.s.scan();
        }

        self.arith_op();

        self.expr_prime();
    }

    fn expr_prime(&mut self) {
        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::And || tok == Token::Or {
            // consume token
            let tok = self.s.scan();

            self.arith_op();
        } else {
            // null body production
            return;
        }

        self.expr_prime();
    }

    fn arith_op(&mut self) {
        self.relation();

        self.arith_op_prime();
    }

    fn arith_op_prime(&mut self) {
        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::AddOp || tok == Token::AddOp { // plus or minus
            // consume token
            let tok = self.s.scan();

            self.relation();
        } else {
            // null body production
            return;
        }

        self.arith_op_prime();
    }

    fn relation(&mut self) {
        self.term();

        self.relation_prime();
    }

    fn relation_prime(&mut self) {
        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::LT ||
            tok == Token::LTE ||
            tok == Token::GT ||
            tok == Token::GTE ||
            tok == Token::Eq ||
            tok == Token::NotEq {
            // consume token
            let tok = self.s.scan();

            self.term();
        } else {
            // null body production
            return;
        }

        self.relation_prime();
    }

    fn term(&mut self) {
        self.factor();

        self.term_prime();
    }

    fn term_prime(&mut self) {
        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::MulOp || tok == Token::MulOp { // mul or div
            // consume token
            let tok = self.s.scan();

            self.factor();
        } else {
            // null body production
            return;
        }

        self.term_prime();
    }

    fn factor(&mut self) {
        // factor first: "(", procedure_call first, "-", name first, "number", "string", "true", "false"
        // procedure_call first: "identifier"
        // name first: "identifier"

        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::Identifier {
            // TODO: how do we determine if it is a name or procedure call?
            self.procedure_call();
            //self.name();
        } else if tok == Token::AddOp { // minus
            // consume minus
            self.s.scan();

            // TODO: peek token
            let tok = self.s.scan();
            if tok == Token::Identifier {
                self.name();
            } else if tok == Token::Number {
                // consume number
                self.s.scan();
            } else {
                panic!("Expected \"identifier\" or \"number\" following \"-\"");
            }
        } else if tok == Token::Number {
            // consume number
            self.s.scan();
        } else if tok == Token::LParen {
            // consume left paren
            self.s.scan();

            self.expr();

            let tok = self.s.scan();
            if tok != Token::RParen { // :
                panic!("Expected \")\"");
            }
        } else if tok == Token::String {
            // consume number
            self.s.scan();
        } else if tok == Token::Keyword { // true
            // consume true
            self.s.scan();
        } else if tok == Token::Keyword { // false
            // consume true
            self.s.scan();
        } else {
            panic!("Expected \"factor\"");
        }
    }

    fn procedure_call(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Identifier {
            panic!("Expected \"identifier\"");
        }

        let tok = self.s.scan();
        if tok != Token::LParen { // :
            panic!("Expected \"(\"");
        }

        // argument_list first: "expr first"
        // expr first: "not", "arith_op first"
        // arith_op first: "relation first"
        // relation first: "term first"
        // term first: "factor first"
        // factor first: ""
        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::Keyword { // not
        // TODO: factor first
            self.argument_list();
        }

        let tok = self.s.scan();
        if tok != Token::RParen { // :
            panic!("Expected \")\"");
        }
    }

    fn argument_list(&mut self) {
        self.expr();

        // TODO: peek token
        let mut peek = self.s.scan();
        if peek == Token::Comma {
            //consume comma
            self.s.scan();

            self.argument_list();
        }
    }

    fn name(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Identifier {
            panic!("Expected \"identifier\"");
        }

        // TODO: peek token
        let tok = self.s.scan();
        if tok != Token::LSquare { // :
            return;
        }

        // consume LSquare
        let tok = self.s.scan();

        self.expr();

        let tok = self.s.scan();
        if tok != Token::RSquare { // :
            panic!("Expected \"]\"");
        }
    }
}

pub struct Scanner {
    stream: String,
    i: usize,
    table: HashMap<String, Token>,
}

impl Scanner {
    pub fn new(contents: String) -> Scanner {
        let mut table = HashMap::new();

        table.insert(String::from("program"), Token::Keyword);
        table.insert(String::from("is"), Token::Keyword);
        table.insert(String::from("global"), Token::Keyword);
        table.insert(String::from("procedure"), Token::Keyword);
        table.insert(String::from("variable"), Token::Keyword);
        table.insert(String::from("begin"), Token::Keyword);
        table.insert(String::from("if"), Token::Keyword);
        table.insert(String::from("then"), Token::Keyword);
        table.insert(String::from("else"), Token::Keyword);
        table.insert(String::from("for"), Token::Keyword);
        table.insert(String::from("return"), Token::Keyword);
        table.insert(String::from("end program"), Token::Keyword);
        table.insert(String::from("end procedure"), Token::Keyword);
        table.insert(String::from("end if"), Token::Keyword);
        table.insert(String::from("end for"), Token::Keyword);
        table.insert(String::from("true"), Token::Keyword);
        table.insert(String::from("false"), Token::Keyword);
        table.insert(String::from("not"), Token::Keyword);
        table.insert(String::from("integer"), Token::Type);
        table.insert(String::from("float"), Token::Type);
        table.insert(String::from("string"), Token::Type);
        table.insert(String::from("bool"), Token::Type);

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

    fn read_ch(&mut self) -> Option<char> {
        if self.i <= self.stream.len() {
            return None;
        }
        let c = self.stream.chars().nth(self.i);
        self.i += 1;
        return c;
    }

    fn unread_ch(&mut self) {
        self.i -= 1;
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
        if self.i < self.stream.len() && self.is_alpha(self.stream.chars().nth(self.i).unwrap()) {
            self.i += 1;
            while self.i < self.stream.len() && (self.is_alphanumeric(self.stream.chars().nth(self.i).unwrap()) || self.stream.chars().nth(self.i).unwrap() == '_') {
                self.i += 1;
            }

            let slice = &self.stream[start..self.i];
            println!("{}", slice);

            match self.table.get(slice) {
                Some(tok) => return tok.clone(),
                _ => (),
            }

            return Token::Identifier;
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
            return Token::LParen;
        }

        // right parenthesis
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ')' {
            self.i += 1;
            println!(")");
            return Token::RParen;
        }

        // left square bracket
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '[' {
            self.i += 1;
            println!("[");
            return Token::LSquare;
        }

        // right square bracket
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == ']' {
            self.i += 1;
            println!("]");
            return Token::RSquare;
        }

        // plus
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '+' {
            self.i += 1;
            println!("+");
            return Token::AddOp;
        }

        // minus
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '-' {
            self.i += 1;
            println!("-");
            return Token::AddOp;
        }

        // multiply
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '*' {
            self.i += 1;
            println!("*");
            return Token::MulOp;
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
                return Token::Eq;
            }
        }

        // not equal
        else if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '!' {
            self.i += 1;
            if self.i < self.stream.len() && self.stream.chars().nth(self.i).unwrap() == '=' {
                self.i += 1;
                println!("!=");
                return Token::NotEq;
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
        let mut s = Scanner::new(String::from("+"));
        let tok = s.scan();
        assert!(tok == Token::AddOp);
    }

    #[test]
    fn scan_sub() {
        let mut s = Scanner::new(String::from("-"));
        let tok = s.scan();
        assert!(tok == Token::AddOp);
    }

    #[test]
    fn scan_mul() {
        let mut s = Scanner::new(String::from("*"));
        let tok = s.scan();
        assert!(tok == Token::MulOp);
    }

    #[test]
    fn scan_div() {
        let mut s = Scanner::new(String::from("/"));
        let tok = s.scan();
        assert!(tok == Token::MulOp);
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
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_bool_true() {
        let mut s = Scanner::new(String::from("true"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_bool_false() {
        let mut s = Scanner::new(String::from("false"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_type_integer() {
        let mut s = Scanner::new(String::from("integer"));
        let tok = s.scan();
        assert!(tok == Token::Type);
    }

    #[test]
    fn scan_type_float() {
        let mut s = Scanner::new(String::from("float"));
        let tok = s.scan();
        assert!(tok == Token::Type);
    }

    #[test]
    fn scan_type_string() {
        let mut s = Scanner::new(String::from("string"));
        let tok = s.scan();
        assert!(tok == Token::Type);
    }

    #[test]
    fn scan_type_bool() {
        let mut s = Scanner::new(String::from("bool"));
        let tok = s.scan();
        assert!(tok == Token::Type);
    }

    #[test]
    fn scan_program() {
        let mut s = Scanner::new(String::from("program"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_is() {
        let mut s = Scanner::new(String::from("is"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_global() {
        let mut s = Scanner::new(String::from("global"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_procedure() {
        let mut s = Scanner::new(String::from("procedure"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_variable() {
        let mut s = Scanner::new(String::from("variable"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_begin() {
        let mut s = Scanner::new(String::from("begin"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_endprogram() {
        let mut s = Scanner::new(String::from("end program"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_endprocedure() {
        let mut s = Scanner::new(String::from("end procedure"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_endif() {
        let mut s = Scanner::new(String::from("end if"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_endfor() {
        let mut s = Scanner::new(String::from("end for"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_if() {
        let mut s = Scanner::new(String::from("if"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_then() {
        let mut s = Scanner::new(String::from("then"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_else() {
        let mut s = Scanner::new(String::from("else"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_for() {
        let mut s = Scanner::new(String::from("for"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
    }

    #[test]
    fn scan_return() {
        let mut s = Scanner::new(String::from("return"));
        let tok = s.scan();
        assert!(tok == Token::Keyword);
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
        assert!(tok == Token::Number);
    }

    #[test]
    fn scan_number_multidigit() {
        let mut s = Scanner::new(String::from("23459"));
        let tok = s.scan();
        assert!(tok == Token::Number);
    }

    #[test]
    fn scan_number_multidigit_decimalpoint() {
        let mut s = Scanner::new(String::from("15429."));
        let tok = s.scan();
        assert!(tok == Token::Number);
    }

    #[test]
    fn scan_number_multidigit_decimalpoint_singledigit() {
        let mut s = Scanner::new(String::from("92345.1"));
        let tok = s.scan();
        assert!(tok == Token::Number);
    }

    #[test]
    fn scan_number_multidigit_decimalpoint_multidigit() {
        let mut s = Scanner::new(String::from("9345.23456"));
        let tok = s.scan();
        assert!(tok == Token::Number);
    }

    #[test]
    fn scan_string_empty() {
        let mut s = Scanner::new(String::from("\"\""));
        let tok = s.scan();
        assert!(tok == Token::String);
    }

    #[test]
    fn scan_string_singlechar() {
        let mut s = Scanner::new(String::from("\"a\""));
        let tok = s.scan();
        assert!(tok == Token::String);
    }

    #[test]
    fn scan_string_multichar() {
        let mut s = Scanner::new(String::from("\"asdgqerygsh\""));
        let tok = s.scan();
        assert!(tok == Token::String);
    }

    #[test]
    fn scan_identifier_single_lower() {
        let mut s = Scanner::new(String::from("a"));
        let tok = s.scan();
        assert!(tok == Token::Identifier);
    }

    #[test]
    fn scan_identifier_single_upper() {
        let mut s = Scanner::new(String::from("A"));
        let tok = s.scan();
        assert!(tok == Token::Identifier);
    }

    #[test]
    fn scan_identifier_multi_lower() {
        let mut s = Scanner::new(String::from("awerthsdf"));
        let tok = s.scan();
        assert!(tok == Token::Identifier);
    }

    #[test]
    fn scan_identifier_multi_upper() {
        let mut s = Scanner::new(String::from("ASGSDFIWERYHEHA"));
        let tok = s.scan();
        assert!(tok == Token::Identifier);
    }

    #[test]
    fn scan_identifier_multi_alpha() {
        let mut s = Scanner::new(String::from("aaGWErsGBHq"));
        let tok = s.scan();
        assert!(tok == Token::Identifier);
    }

    #[test]
    fn scan_identifier_multi_alphanumeric() {
        let mut s = Scanner::new(String::from("aa4GWErs467GBHq78"));
        let tok = s.scan();
        assert!(tok == Token::Identifier);
    }

    #[test]
    fn scan_identifier_multi_all() {
        let mut s = Scanner::new(String::from("aa4__GWErs467GBHq7_8"));
        let tok = s.scan();
        assert!(tok == Token::Identifier);
    }

    #[test]
    fn scan_identifier_multi_underscores() {
        let mut s = Scanner::new(String::from("a_____________"));
        let tok = s.scan();
        assert!(tok == Token::Identifier);
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
        assert!(tok == Token::Identifier);
    }

    #[test]
    fn scan_whitespace_tabs() {
        let mut s = Scanner::new(String::from("\t\t\t\ta\t\t\t\t"));
        let tok = s.scan();
        assert!(tok == Token::Identifier);
    }

    #[test]
    fn scan_whitespace_linefeed() {
        let mut s = Scanner::new(String::from("\n\n\na\n\n\n"));
        let tok = s.scan();
        assert!(tok == Token::Identifier);
    }

    #[test]
    fn scan_whitespace_carriagereturn() {
        let mut s = Scanner::new(String::from("\r\r\r\ra\r\r\r\r"));
        let tok = s.scan();
        assert!(tok == Token::Identifier);
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
}
