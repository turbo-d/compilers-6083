use crate::scanner::Scanner;
use crate::token::Token;

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
        if tok != Token::Program {
            panic!("Expected \"program\"");
        }

        let tok = self.s.scan();
        if tok != Token::Identifier { // identifier
            panic!("Expected \"identifier\"");
        }

        let tok = self.s.scan();
        if tok != Token::Is {
            panic!("Expected \"is\"");
        }
    }

    // first: "global", "procedure", "variable", "begin"
    fn program_body(&mut self) {
        // TODO: peek next token
        let tok = self.s.scan();
        while tok == Token::Global || tok == Token::Procedure || tok == Token::Variable { // declaration first: global, procedure, or variable
            self.declaration();

            let tok = self.s.scan();
            if tok != Token::Semicolon { // ;
                panic!("Expected \";\"");
            }
        }

        let tok = self.s.scan();
        if tok != Token::Begin {
            panic!("Expected \"begin\"");
        }

        // TODO: peek instead
        let tok = self.s.scan();
        while tok == Token::Identifier || tok == Token::If || tok == Token::For || tok == Token::Return { // statement first: "identifier", "if", "for", "return"
            self.statement();

            let tok = self.s.scan();
            if tok != Token::Semicolon { // ;
                panic!("Expected \";\"");
            }
        }

        let tok = self.s.scan();
        if tok != Token::EndProgram { // end program keyword
            panic!("Expected \"end program\"");
        }
    }

    fn declaration(&mut self) {
        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::Global {
            // consume global token
            let tok = self.s.scan();
        }

        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::Procedure {
            self.procedure_declaration();
        } else if tok == Token::Variable {
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
        if tok != Token::Procedure {
            panic!("Expected \"procedure\"");
        }
        let tok = self.s.scan();
        if tok != Token::Identifier {
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
        if tok == Token::Variable {
            self.parameter_list();
        }

        let tok = self.s.scan();
        if tok != Token::RParen { // :
            panic!("Expected \")\"");
        }
    }

    fn type_mark(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Int || tok != Token::Float || tok != Token::String || tok != Token::Bool {
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
        while tok == Token::Global || tok == Token::Procedure || tok == Token::Variable { // declaration first: global, procedure, or variable
            self.declaration();

            let tok = self.s.scan();
            if tok != Token::Semicolon { // ;
                panic!("Expected \";\"");
            }
        }

        let tok = self.s.scan();
        if tok != Token::Begin {
            panic!("Expected \"begin\"");
        }

        // TODO: peek instead
        let tok = self.s.scan();
        while tok == Token::Identifier || tok == Token::If || tok == Token::For || tok == Token::Return { // statement first: "identifier", "if", "for", "return"
            self.statement();

            let tok = self.s.scan();
            if tok != Token::Semicolon { // ;
                panic!("Expected \";\"");
            }
        }

        let tok = self.s.scan();
        if tok != Token::EndProgram {
            panic!("Expected \"end procedure\"");
        }
    }

    fn variable_declaration(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Variable {
            panic!("Expected \"variable\"");
        }

        let tok = self.s.scan();
        if tok != Token::Identifier {
            panic!("Expected \"identifier\"");
        }

        let tok = self.s.scan();
        if tok != Token::Colon {
            panic!("Expected \":\"");
        }

        self.type_mark();

        // TODO: peek token
        let tok = self.s.scan();
        if tok != Token::LSquare {
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
        } else if tok == Token::If {
            self.if_statement();
        } else if tok == Token::For {
            self.loop_statement();
        } else if tok == Token::Return {
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
        if tok != Token::If {
            panic!("Expected \"if\"");
        }

        self.expr();

        let tok = self.s.scan();
        if tok != Token::Then {
            panic!("Expected \"then\"");
        }

        // TODO: peek
        let tok = self.s.scan();
        while tok == Token::Else {
            let tok = self.s.scan();
            if tok != Token::Else {
                panic!("Expected \"else\"");
            }

            self.statement();

            let tok = self.s.scan();
            if tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
        }

        let tok = self.s.scan();
        if tok != Token::EndIf {
            panic!("Expected \"end if\"");
        }
    }

    fn loop_statement(&mut self) {
        let tok = self.s.scan();
        if tok != Token::For {
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
        while tok == Token::Identifier || tok == Token::If || tok == Token::For || tok == Token::Return { // statement first: "identifier", "if", "for", "return"
            self.statement();

            let tok = self.s.scan();
            if tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
        }

        let tok = self.s.scan();
        if tok != Token::EndFor {
            panic!("Expected \"end for\"");
        }

    }

    fn return_statement(&mut self) {
        let tok = self.s.scan();
        if tok != Token::Return {
            panic!("Expected \"return\"");
        }

        self.expr();
    }

    fn expr(&mut self) {
        // TODO: peek token
        let tok = self.s.scan();
        if tok == Token::Not {
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
        if tok == Token::Add || tok == Token::Sub {
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
        if tok == Token::Mul || tok == Token::Div {
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
        } else if tok == Token::Sub {
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
        } else if tok == Token::True {
            // consume true
            self.s.scan();
        } else if tok == Token::False {
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
        if tok == Token::Not {
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

