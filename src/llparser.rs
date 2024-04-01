use crate::scanner::Scanner;
use crate::symtable::SymTable;
use crate::token::Token;

pub struct LLParser {
    s: Scanner,
    tok: Token,
    st: SymTable,
}

impl LLParser {
    pub fn new(s: Scanner) -> LLParser {
        LLParser {
            s,
            tok: Token::Unknown,
            st: SymTable::new(),
        }
    }

    pub fn parse(&mut self) {
        self.consume_tok();

        self.program();

        if self.tok != Token::EOF {
            eprintln!("Extraneous trailing characters");
        }
    }

    fn consume_tok(&mut self) {
        self.tok = self.s.scan();
    }

    fn program(&mut self) {
        self.program_header();

        self.program_body();

        if self.tok != Token::Period {
            // TODO: Probably want to warn here instead
            panic!("Expected \".\"");
        }
        self.consume_tok();
    }

    fn program_header(&mut self) {
        if self.tok != Token::Program {
            panic!("Expected \"program\"");
        }
        self.consume_tok();

        if !matches!(self.tok, Token::Identifier(_)) {
            panic!("Expected \"identifier\"");
        }
        self.consume_tok();

        if self.tok != Token::Is {
            panic!("Expected \"is\"");
        }
        self.consume_tok();
    }

    // first(program_body): "global", "procedure", "variable", "begin"
    fn program_body(&mut self) {
        // first(declaration)
        while self.tok == Token::Global || self.tok == Token::Procedure || self.tok == Token::Variable {
            self.declaration();

            if self.tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
            self.consume_tok();
        }

        if self.tok != Token::Begin {
            panic!("Expected \"begin\"");
        }
        self.consume_tok();

        // first(statement)
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            self.statement();

            if self.tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
            self.consume_tok();
        }

        if self.tok != Token::EndProgram {
            panic!("Expected \"end program\"");
        }
        self.consume_tok();
    }

    // first(declaration): "global", "procedure", "variable"
    fn declaration(&mut self) {
        let mut is_global = false;
        if self.tok == Token::Global {
            self.consume_tok();
            is_global = true;
        }

        if self.tok == Token::Procedure {
            self.procedure_declaration(is_global);
        } else if self.tok == Token::Variable {
            self.variable_declaration(is_global);
        } else {
            panic!("Expected \"Procedure declaration or Variable declaration\"");
        }
    }

    fn procedure_declaration(&mut self, is_global: bool) {
        self.procedure_header(is_global);
        self.procedure_body();
    }

    fn procedure_header(&mut self, is_global: bool) {
        if self.tok != Token::Procedure {
            panic!("Expected \"procedure\"");
        }
        self.consume_tok();

        let identifier: String;
        match &self.tok {
            Token::Identifier(id) => identifier = id.clone(),
            _ => panic!("Expected \"identifier\""),
        }
        self.consume_tok();

        if self.tok != Token::Colon {
            panic!("Expected \":\"");
        }
        self.consume_tok();

        self.type_mark();

        if self.tok != Token::LParen {
            panic!("Expected \"(\"");
        }
        self.consume_tok();

        // first(parameter_list)
        if self.tok == Token::Variable {
            self.parameter_list();
        }

        if self.tok != Token::RParen {
            panic!("Expected \")\"");
        }
        self.consume_tok();

        let result: Result<(), String>;
        if is_global {
            result = self.st.insert_global(identifier.clone(), Token::Unknown);
        } else {
            result = self.st.insert(identifier.clone(), Token::Unknown);
        }

        match result {
            Ok(_) => (),
            Err(_) => panic!("Duplicate declaration. {identifier} is already declared in this scope"),
        }
    }

    fn type_mark(&mut self) {
        if self.tok != Token::IntType && self.tok != Token::FloatType && self.tok != Token::StringType && self.tok != Token::BoolType {
            panic!("Expected \"type\"");
        }
        self.consume_tok();
    }

    // first(parameter_list): "variable"
    fn parameter_list(&mut self) {
        self.parameter();

        while self.tok == Token::Comma {
            //consume comma
            self.consume_tok();

            self.parameter();
        }
    }

    fn parameter(&mut self) {
        self.variable_declaration(false);
    }

    // TODO: Almost the same as program_body
    // first(procedure_body): "global", "procedure", "variable", "begin"
    fn procedure_body(&mut self) {
        // first(declaration)
        while self.tok == Token::Global || self.tok == Token::Procedure || self.tok == Token::Variable {
            self.declaration();

            if self.tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
            self.consume_tok();
        }

        if self.tok != Token::Begin {
            panic!("Expected \"begin\"");
        }
        self.consume_tok();

        // first(statement)
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            self.statement();

            if self.tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
            self.consume_tok();
        }

        if self.tok != Token::EndProcedure {
            panic!("Expected \"end procedure\"");
        }
        self.consume_tok();
    }

    fn variable_declaration(&mut self, is_global: bool) {
        if self.tok != Token::Variable {
            panic!("Expected \"variable\"");
        }
        self.consume_tok();

        let identifier: String;
        match &self.tok {
            Token::Identifier(id) => identifier = id.clone(),
            _ => panic!("Expected \"identifier\""),
        }
        self.consume_tok();

        if self.tok != Token::Colon {
            panic!("Expected \":\"");
        }
        self.consume_tok();

        self.type_mark();

        if self.tok == Token::LSquare {
            // consume LSquare
            self.consume_tok();

            // bound (inlined production expansion)
            if !matches!(self.tok, Token::Number(_)) {
                panic!("Expected \"number\"");
            }
            self.consume_tok();

            if self.tok != Token::RSquare {
                panic!("Expected \"]\"");
            }
            self.consume_tok();
        }

        let result: Result<(), String>;
        if is_global {
            result = self.st.insert_global(identifier.clone(), Token::Unknown);
        } else {
            result = self.st.insert(identifier.clone(), Token::Unknown);
        }

        match result {
            Ok(_) => (),
            Err(_) => panic!("Duplicate declaration. {identifier} is already declared in this scope"),
        }
    }

    // first(statement): "identifier", "if", "for", "return"
    fn statement(&mut self) {
        if matches!(self.tok, Token::Identifier(_)) {
            self.assignment_statement();
        } else if self.tok == Token::If {
            self.if_statement();
        } else if self.tok == Token::For {
            self.loop_statement();
        } else if self.tok == Token::Return {
            self.return_statement();
        } else {
            panic!("Expected \"statement\"");
        }
    }

    fn assignment_statement(&mut self) {
        self.destination();

        if self.tok != Token::Assign {
            panic!("Expected \":=\"");
        }
        self.consume_tok();

        self.expr();
    }

    fn destination(&mut self) {
        if !matches!(self.tok, Token::Identifier(_)) {
            panic!("Expected \"identifier\"");
        }
        self.consume_tok();

        if self.tok != Token::LSquare {
            return;
        }
        // consume LSquare
        self.consume_tok();

        self.expr();

        if self.tok != Token::RSquare {
            panic!("Expected \"]\"");
        }
        self.consume_tok();
    }

    fn if_statement(&mut self) {
        if self.tok != Token::If {
            panic!("Expected \"if\"");
        }
        self.consume_tok();

        if self.tok != Token::LParen {
            panic!("Expected \"(\"");
        }
        self.consume_tok();

        self.expr();

        if self.tok != Token::RParen {
            panic!("Expected \")\"");
        }
        self.consume_tok();

        if self.tok != Token::Then {
            panic!("Expected \"then\"");
        }
        self.consume_tok();

        // first(statement)
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            self.statement();

            if self.tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
            self.consume_tok();
        }

        if self.tok == Token::Else {
            // consume else
            self.consume_tok();

            // first(statement)
            while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
                self.statement();

                if self.tok != Token::Semicolon {
                    panic!("Expected \";\"");
                }
                self.consume_tok();
            }
        }

        if self.tok != Token::EndIf {
            panic!("Expected \"end if\"");
        }
        self.consume_tok();
    }

    fn loop_statement(&mut self) {
        if self.tok != Token::For {
            panic!("Expected \"for\"");
        }
        self.consume_tok();

        if self.tok != Token::LParen {
            panic!("Expected \"(\"");
        }
        self.consume_tok();

        self.assignment_statement();

        if self.tok != Token::Semicolon {
            panic!("Expected \";\"");
        }
        self.consume_tok();

        self.expr();

        if self.tok != Token::RParen {
            panic!("Expected \")\"");
        }
        self.consume_tok();

        // first(statement)
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            self.statement();

            if self.tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
            self.consume_tok();
        }

        if self.tok != Token::EndFor {
            panic!("Expected \"end for\"");
        }
        self.consume_tok();
    }

    fn return_statement(&mut self) {
        if self.tok != Token::Return {
            panic!("Expected \"return\"");
        }
        self.consume_tok();

        self.expr();
    }

    // first(expr): "not", "(", "identifier", "-", "number", "string", "true", "false"
    fn expr(&mut self) {
        if self.tok == Token::Not {
            // consume not token
            self.consume_tok();
        }

        self.arith_op();

        self.expr_prime();
    }

    fn expr_prime(&mut self) {
        if self.tok == Token::And || self.tok == Token::Or {
            // consume token
            self.consume_tok();

            self.arith_op();
        } else {
            // null body production
            // TODO: check follow() for error checking
            return;
        }

        self.expr_prime();
    }

    // first(arith_op): "(", "identifier", "-", "number", "string", "true", "false"
    fn arith_op(&mut self) {
        self.relation();

        self.arith_op_prime();
    }

    fn arith_op_prime(&mut self) {
        if self.tok == Token::Add || self.tok == Token::Sub {
            // consume token
            self.consume_tok();

            self.relation();
        } else {
            // null body production
            // TODO: check follow() for error checking
            return;
        }

        self.arith_op_prime();
    }

    // first(relation): "(", "identifier", "-", "number", "string", "true", "false"
    fn relation(&mut self) {
        self.term();

        self.relation_prime();
    }

    fn relation_prime(&mut self) {
        if self.tok == Token::LT ||
            self.tok == Token::LTE ||
            self.tok == Token::GT ||
            self.tok == Token::GTE ||
            self.tok == Token::Eq ||
            self.tok == Token::NotEq {
            // consume token
            self.consume_tok();

            self.term();
        } else {
            // null body production
            // TODO: check follow() for error checking
            return;
        }

        self.relation_prime();
    }

    // first(term): "(", "identifier", "-", "number", "string", "true", "false"
    fn term(&mut self) {
        self.factor();

        self.term_prime();
    }

    fn term_prime(&mut self) {
        if self.tok == Token::Mul || self.tok == Token::Div {
            // consume token
            self.consume_tok();

            self.factor();
        } else {
            // null body production
            // TODO: check follow() for error checking
            return;
        }

        self.term_prime();
    }

    // first(factor): "(", "identifier", "-", "number", "string", "true", "false"
    fn factor(&mut self) {
        if matches!(self.tok, Token::Identifier(_)) {
            // consume identifier
            self.consume_tok();

            if self.tok == Token::LParen {
                self.procedure_call_prime();
            } else {
                self.name_prime();
            }
        } else if self.tok == Token::Sub {
            // consume minus
            self.consume_tok();

            if matches!(self.tok, Token::Identifier(_)) {
                self.name();
            } else if matches!(self.tok, Token::Number(_)) {
                // consume number
                self.consume_tok();
            } else {
                panic!("Expected \"identifier\" or \"number\" following \"-\"");
            }
        } else if matches!(self.tok, Token::Number(_)) {
            // consume number
            self.consume_tok();
        } else if self.tok == Token::LParen {
            // consume left paren
            self.consume_tok();

            self.expr();

            if self.tok != Token::RParen {
                panic!("Expected \")\"");
            }
            self.consume_tok();
        } else if matches!(self.tok, Token::String(_)) {
            // consume string
            self.consume_tok();
        } else if self.tok == Token::True {
            // consume true
            self.consume_tok();
        } else if self.tok == Token::False {
            // consume false
            self.consume_tok();
        } else {
            panic!("Expected \"factor\"");
        }
    }

    fn procedure_call_prime(&mut self) {
        // identifier already consumed

        if self.tok != Token::LParen {
            panic!("Expected \"(\"");
        }
        self.consume_tok();

        // first(argument_list)
        if matches!(self.tok, Token::Identifier(_)) ||
            matches!(self.tok, Token::Number(_)) || 
            matches!(self.tok, Token::String(_)) || 
            self.tok == Token::Not ||
            self.tok == Token::LParen ||
            self.tok == Token::True ||
            self.tok == Token::False ||
            self.tok == Token::Sub {
            self.argument_list();
        }

        if self.tok != Token::RParen {
            panic!("Expected \")\"");
        }
        self.consume_tok();
    }

    // first(argument_list): "not", "(", "identifier", "-", "number", "string", "true", "false"
    fn argument_list(&mut self) {
        self.expr();

        while self.tok == Token::Comma {
            //consume comma
            self.consume_tok();

            self.expr();
        }
    }

    fn name(&mut self) {
        if !matches!(self.tok, Token::Identifier(_)) {
            panic!("Expected \"identifier\"");
        }
        self.consume_tok();

        self.name_prime();
    }

    fn name_prime(&mut self) {
        // identifier already consumed

        if self.tok != Token::LSquare {
            return;
        }
        // consume LSquare
        self.consume_tok();

        self.expr();

        if self.tok != Token::RSquare {
            panic!("Expected \"]\"");
        }
        self.consume_tok();
    }
}

// TODO: tests
// TODO: error handling and recovery
