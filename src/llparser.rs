use crate::ast;
use crate::codegen::CodeGen;
use crate::scanner::Scanner;
use crate::symtable::SymTable;
use crate::token::Token;
use crate::types::Types;

use std::vec::Vec;

pub struct LLParser<'a, 'ctx> {
    s: Scanner,
    tok: Token,
    st: SymTable,
    cg: CodeGen<'a, 'ctx>,
}

impl<'a, 'ctx> LLParser<'a, 'ctx> {
    pub fn new(s: Scanner, codegen: CodeGen<'a, 'ctx>) -> LLParser<'a, 'ctx> {
        let mut st = SymTable::new();
        // TODO: Delete this once runtime is finished.
        // This is just for testing
        let _ = st.insert_global(String::from("getbool"), Types::Proc(Box::new(Types::Bool), Vec::new()));
        let _ = st.insert_global(String::from("getinteger"), Types::Proc(Box::new(Types::Int), Vec::new()));
        let _ = st.insert_global(String::from("getfloat"), Types::Proc(Box::new(Types::Float), Vec::new()));
        let _ = st.insert_global(String::from("getstring"), Types::Proc(Box::new(Types::String), Vec::new()));
        let _ = st.insert_global(String::from("putbool"), Types::Proc(Box::new(Types::Bool), vec![Types::Bool]));
        let _ = st.insert_global(String::from("putinteger"), Types::Proc(Box::new(Types::Bool), vec![Types::Int]));
        let _ = st.insert_global(String::from("putfloat"), Types::Proc(Box::new(Types::Bool), vec![Types::Float]));
        let _ = st.insert_global(String::from("putstring"), Types::Proc(Box::new(Types::Bool), vec![Types::String]));
        let _ = st.insert_global(String::from("sqrt"), Types::Proc(Box::new(Types::Float), vec![Types::Int]));

        LLParser {
            s,
            tok: Token::Unknown,
            st: st,
            cg: codegen,
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
        let proc_type = self.procedure_header(is_global);
        self.procedure_body(proc_type);
    }

    fn procedure_header(&mut self, is_global: bool) -> Types {
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

        let return_type = self.type_mark();

        if self.tok != Token::LParen {
            panic!("Expected \"(\"");
        }
        self.consume_tok();

        let mut param_types = Vec::new();
        // first(parameter_list)
        if self.tok == Token::Variable {
            param_types = self.parameter_list();
        }

        if self.tok != Token::RParen {
            panic!("Expected \")\"");
        }
        self.consume_tok();

        let parsed_type = Types::Proc(Box::new(return_type), param_types);
        let result: Result<(), String>;
        if is_global {
            result = self.st.insert_global(identifier.clone(), parsed_type.clone());
        } else {
            result = self.st.insert(identifier.clone(), parsed_type.clone());
        }

        match result {
            Ok(_) => (),
            Err(_) => panic!("Duplicate declaration. {identifier} is already declared in this scope"),
        }

        parsed_type
    }

    fn type_mark(&mut self) -> Types {
        let parsed_type: Types;
        if self.tok == Token::IntType {
            parsed_type = Types::Int;
        } else if self.tok == Token::FloatType {
            parsed_type = Types::Float;
        } else if self.tok == Token::StringType {
            parsed_type = Types::String;
        } else if self.tok == Token::BoolType {
            parsed_type = Types::Bool;
        } else {
            panic!("Expected \"type\"");
        }
        self.consume_tok();

        return parsed_type;
    }

    // first(parameter_list): "variable"
    fn parameter_list(&mut self) -> Vec<Types> {
        let mut param_types = Vec::new();

        param_types.push(self.parameter());

        while self.tok == Token::Comma {
            //consume comma
            self.consume_tok();

            param_types.push(self.parameter());
        }

        param_types
    }

    fn parameter(&mut self) -> Types {
        self.variable_declaration(false)
    }

    // TODO: Almost the same as program_body
    // first(procedure_body): "global", "procedure", "variable", "begin"
    fn procedure_body(&mut self, proc_type: Types) {
        self.st.enter_scope(proc_type.clone());

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

        self.st.exit_scope();
    }

    fn variable_declaration(&mut self, is_global: bool) -> Types {
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

        let mut parsed_type = self.type_mark();

        if self.tok == Token::LSquare {
            // consume LSquare
            self.consume_tok();

            // bound (inlined production expansion)
            match &self.tok {
                Token::IntLiteral(num) => {
                    let num = num.clone();
                    if (num as u32) as i32 == num {
                        let bound = num as u32;
                        parsed_type = Types::Array(bound, Box::new(parsed_type));
                    } else {
                        panic!("Array size must be non-negative")
                    }
                }
                Token::FloatLiteral(_) => panic!("Array size must be a non-negative integer value"),
                _ => panic!("Expected \"number\""),
            }
            self.consume_tok();

            if self.tok != Token::RSquare {
                panic!("Expected \"]\"");
            }
            self.consume_tok();
        }

        let result: Result<(), String>;
        if is_global {
            result = self.st.insert_global(identifier.clone(), parsed_type.clone());
        } else {
            result = self.st.insert(identifier.clone(), parsed_type.clone());
        }

        match result {
            Ok(_) => (),
            Err(_) => panic!("Duplicate declaration. {identifier} is already declared in this scope"),
        }

        parsed_type
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
        let dest_type = self.destination();

        if self.tok != Token::Assign {
            panic!("Expected \":=\"");
        }
        self.consume_tok();

        let (expr_type, expr_node) = self.expr();

        match dest_type {
            Types::Bool => {
                if expr_type != Types::Bool && expr_type != Types::Int {
                    panic!("Type mismatch. Expression must be of bool or integer type");
                }
            }
            Types::Int => {
                if expr_type != Types::Int && expr_type != Types::Bool {
                    panic!("Type mismatch. Expression must be of integer, float, or bool type");
                }
            }
            Types::Float => {
                if expr_type != Types::Float && expr_type != Types::Int {
                    panic!("Type mismatch. Expression must be of float or integer type");
                }
            }
            Types::String => {
                if expr_type != Types::String {
                    panic!("Type mismatch. Expression must be of string type");
                }
            }
            _ => panic!("Assignment not supported for this operand type"),
        }
    }

    fn destination(&mut self) -> Types {
        let parsed_type: Types;
        match &self.tok {
            Token::Identifier(id) => {
                match self.st.get(id) {
                    Some(types) => parsed_type = types.clone(),
                    None => panic!("Missing declaration for {id}"),
                }
            }
            _ => panic!("Expected \"identifier\""),
        }
        self.consume_tok();

        if self.tok != Token::LSquare {
            return parsed_type;
        }
        // consume LSquare
        self.consume_tok();

        let elem_type: Types;
        match parsed_type {
            Types::Array(_, elem) => elem_type = *elem,
            _ => panic!("Indexing can only be performed on array types"),
        }

        let (expr_type, expr_node) = self.expr();
        match expr_type {
            Types::Int => (),
            _ => panic!("Array index must be of integer type"),
        }

        if self.tok != Token::RSquare {
            panic!("Expected \"]\"");
        }
        self.consume_tok();

        elem_type
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

        let (expr_type, expr_node) = self.expr();
        if expr_type != Types::Bool && expr_type != Types::Int {
            panic!("The conditional expression must be of bool or integer type");
        }

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

        let (expr_type, expr_node) = self.expr();
        if expr_type != Types::Bool && expr_type != Types::Int {
            panic!("The conditional expression must be of bool or integer type");
        }

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

        let (expr_type, expr_node) = self.expr();
        let owning_proc_type = self.st.get_owning_proc_type();

        // Same compatibility rules as assignment
        if let Types::Proc(return_type, _) = owning_proc_type {
            match *return_type {
                Types::Bool => {
                    if expr_type != Types::Bool && expr_type != Types::Int {
                        panic!("Expression type does not match the return type of the owning procedure");
                    }
                }
                Types::Int => {
                    if expr_type != Types::Int && expr_type != Types::Bool {
                        panic!("Expression type does not match the return type of the owning procedure");
                    }
                }
                Types::Float => {
                    if expr_type != Types::Float && expr_type != Types::Int {
                        panic!("Expression type does not match the return type of the owning procedure");
                    }
                }
                Types::String => {
                    if expr_type != Types::String {
                        panic!("Expression type does not match the return type of the owning procedure");
                    }
                }
                _ => panic!("Returns not supported for this operand type"),
            }
        }
    }

    // first(expr): "not", "(", "identifier", "-", "number", "string", "true", "false"
    fn expr(&mut self) -> (Types, Box<dyn ast::ASTNode>) {
        if self.tok == Token::Not {
            // consume not token
            self.consume_tok();
        }

        let l_op_type = self.arith_op();

        self.expr_prime(l_op_type.clone())
    }

    fn expr_prime(&mut self, l_op_type: Types) -> (Types, Box<dyn ast::ASTNode>) {
        let null_node = Box::new(ast::Var {
            id: String::from(""),
        });

        if self.tok != Token::And && self.tok != Token::Or {
            // null body production
            // TODO: check follow() for error checking
            return (l_op_type, null_node);
        }

        // consume token
        self.consume_tok();

        let r_op_type = self.arith_op();

        if l_op_type != Types::Int {
            panic!("Bitwise operations can only be performed on operands of integer type");
        }

        if r_op_type != Types::Int {
            panic!("Bitwise operations can only be performed on operands of integer type");
        }

        let op_type = Types::Int;
        self.expr_prime(op_type)
    }

    // first(arith_op): "(", "identifier", "-", "number", "string", "true", "false"
    fn arith_op(&mut self) -> Types {
        let l_op_type = self.relation();

        self.arith_op_prime(l_op_type.clone())
    }

    fn arith_op_prime(&mut self, l_op_type: Types) -> Types {
        if self.tok != Token::Add && self.tok != Token::Sub {
            // null body production
            // TODO: check follow() for error checking
            return l_op_type;
        }

        //let op = self.tok.clone();
        // consume token
        self.consume_tok();

        let r_op_type = self.relation();

        if l_op_type != Types::Int && l_op_type != Types::Float {
            panic!("Arithmetic operations can only be performed on operands of integer and float type");
        }

        if r_op_type != Types::Int && r_op_type != Types::Float {
            panic!("Arithmetic operations can only be performed on operands of integer and float type");
        }

        let mut op_type = Types::Float;
        if l_op_type == Types::Int && r_op_type == Types::Int {
            op_type = Types::Int;
        }

        self.arith_op_prime(op_type)
    }

    // first(relation): "(", "identifier", "-", "number", "string", "true", "false"
    fn relation(&mut self) -> Types {
        let (l_op_type, _) = self.term();

        self.relation_prime(l_op_type.clone())
    }

    fn relation_prime(&mut self, l_op_type: Types) -> Types {
        if self.tok != Token::LT && 
            self.tok != Token::LTE &&
            self.tok != Token::GT &&
            self.tok != Token::GTE &&
            self.tok != Token::Eq &&
            self.tok != Token::NotEq {
            // null body production
            // TODO: check follow() for error checking
            return l_op_type;
        }

        let op = self.tok.clone();
        // consume token
        self.consume_tok();

        let (r_op_type, _) = self.term();

        match l_op_type {
            Types::Bool => {
                if r_op_type != Types::Bool && r_op_type != Types::Int {
                    panic!("Type mismatch. Right operand must be of bool or integer type");
                }
            }
            Types::Int => {
                if r_op_type != Types::Int && r_op_type != Types::Bool {
                    panic!("Type mismatch. Right operand must be of integer or bool type");
                }
            }
            Types::Float => {
                if r_op_type != Types::Float {
                    panic!("Type mismatch. Right operand must be of float type");
                }
            }
            Types::String => {
                if op != Token::Eq && op != Token::NotEq {
                    panic!("Operator not supported for operands of string type. Only == and != are supported for operands of string type");
                }
                if r_op_type != Types::String {
                    panic!("Type mismatch. Right operand must be of string type");
                }
            }
            _ => panic!("Relational operators not supported for this operand type"),
        }

        let op_type = Types::Bool;
        self.relation_prime(op_type)
    }

    // first(term): "(", "identifier", "-", "number", "string", "true", "false"
    fn term(&mut self) -> (Types, Box<dyn ast::ASTNode>) {
        let (lhs_type, lhs_node) = self.factor();

        self.term_prime(lhs_type.clone(), lhs_node)
    }

    fn term_prime(&mut self, lhs_type: Types, lhs_node: Box<dyn ast::ASTNode>) -> (Types, Box<dyn ast::ASTNode>) {
        if self.tok != Token::Mul && self.tok != Token::Div {
            // null body production
            // TODO: check follow() for error checking
            return (lhs_type, lhs_node);
        }

        let op = self.tok.clone();
        // consume token
        self.consume_tok();

        let (rhs_type, rhs_node) = self.factor();

        if lhs_type != Types::Int && lhs_type != Types::Float {
            panic!("Arithmetic operations can only be performed on operands of integer and float type");
        }

        if rhs_type != Types::Int && rhs_type != Types::Float {
            panic!("Arithmetic operations can only be performed on operands of integer and float type");
        }

        let op_type = match op {
            Token::Mul => {
                if lhs_type == Types::Int && rhs_type == Types::Int {
                    Types::Int
                } else {
                    Types::Float
                }
            }
            Token::Div => Types::Float,
            _ => Types::Unknown,
        };

        let term_node: Box<dyn ast::ASTNode> = match op {
            Token::Mul => {
                Box::new(ast::MulOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            Token::Div => {
                Box::new(ast::DivOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            _ => panic!("Cannot create ast node"),
        };

        self.term_prime(op_type, term_node)
    }

    // first(factor): "(", "identifier", "-", "number", "string", "true", "false"
    fn factor(&mut self) -> (Types, Box<dyn ast::ASTNode>) {
        let mut parsed_type: Types;
        let mut factor_node: Box<dyn ast::ASTNode>;

        let tok = self.tok.clone();
        match tok {
            Token::Identifier(id) => {
                let mut var_id = String::from("");
                match self.st.get(&id) {
                    Some(types) => {
                        parsed_type = types.clone();
                        var_id = id.clone();
                    }
                    None => panic!("Missing declaration for {id}"),
                }
                // consume identifier
                self.consume_tok();

                let var = Box::new(ast::Var {
                    id: var_id,
                });

                if self.tok == Token::LParen {
                    (parsed_type, factor_node) = self.procedure_call_prime(parsed_type.clone(), var);
                } else {
                    (parsed_type, factor_node) = self.name_prime(parsed_type.clone(), var);
                }
            }
            Token::Sub => {
                // consume minus
                self.consume_tok();

                let mut negate_operand_node: Box<dyn ast::ASTNode>;
                let tok = self.tok.clone();
                match tok {
                    Token::Identifier(_) => {
                        (parsed_type, negate_operand_node) = self.name();
                    }
                    Token::IntLiteral(val) => {
                        // consume number
                        self.consume_tok();
                        parsed_type = Types::Int;
                        negate_operand_node = Box::new(ast::IntLiteral {
                            value: val,
                        });
                    }
                    Token::FloatLiteral(val) => {
                        // consume number
                        self.consume_tok();
                        parsed_type = Types::Float;
                        negate_operand_node = Box::new(ast::FloatLiteral {
                            value: val,
                        });
                    }
                    _ => panic!("Expected \"identifier\" or \"number\" following \"-\"")
                }

                factor_node = Box::new(ast::NegateOp {
                    operand: negate_operand_node,
                });
            }
            Token::IntLiteral(val) => {
                // consume number
                self.consume_tok();
                parsed_type = Types::Int;
                factor_node = Box::new(ast::IntLiteral {
                    value: val,
                });
            }
            Token::FloatLiteral(val) => {
                // consume number
                self.consume_tok();
                parsed_type = Types::Float;
                factor_node = Box::new(ast::FloatLiteral {
                    value: val,
                });
            }
            Token::LParen => {
                // consume left paren
                self.consume_tok();

                let expr_node: Box<dyn ast::ASTNode>;
                (parsed_type, factor_node) = self.expr();

                if self.tok != Token::RParen {
                    panic!("Expected \")\"");
                }
                self.consume_tok();
            }
            Token::String(val) => {
                // consume string
                self.consume_tok();
                parsed_type = Types::String;
                factor_node = Box::new(ast::StringLiteral {
                    value: val.clone(),
                });
            }
            Token::True => {
                // consume true
                self.consume_tok();
                parsed_type = Types::Bool;
                factor_node = Box::new(ast::BoolLiteral {
                    value: true,
                });
            }
            Token::False => {
                // consume false
                self.consume_tok();
                parsed_type = Types::Bool;
                factor_node = Box::new(ast::BoolLiteral {
                    value: false,
                });
            }
            _ => panic!("Expected \"factor\""),
        }

        (parsed_type, factor_node)
    }

    fn procedure_call_prime(&mut self, proc_type: Types, var_node: Box<ast::Var>) -> (Types, Box<dyn ast::ASTNode>) {
        // identifier already consumed

        if self.tok != Token::LParen {
            panic!("Expected \"(\"");
        }
        self.consume_tok();

        let mut args = Vec::new();
        // first(argument_list)
        if matches!(self.tok, Token::Identifier(_)) ||
            matches!(self.tok, Token::IntLiteral(_)) || 
            matches!(self.tok, Token::FloatLiteral(_)) || 
            matches!(self.tok, Token::String(_)) || 
            self.tok == Token::Not ||
            self.tok == Token::LParen ||
            self.tok == Token::True ||
            self.tok == Token::False ||
            self.tok == Token::Sub {
            args = self.argument_list();
        }

        if self.tok != Token::RParen {
            panic!("Expected \")\"");
        }
        self.consume_tok();

        let return_type: Types;
        match proc_type {
            Types::Proc(out_type, param_types) => {
                let n_args = args.len();
                let n_params = param_types.len();
                if n_args != n_params {
                    panic!("Incorrect number of arguments");
                }

                for (i, ((arg_type, arg_node), param_type)) in args.iter().zip(param_types.iter()).enumerate() {
                    if arg_type != param_type {
                        panic!("Type mismatch in argument {i}. (0-indexed)");
                    }
                }

                return_type = *out_type;
            }
            _ => panic!("Expected procedure type"),
        }

        let (arg_types, arg_nodes): (Vec<_>, Vec<_>) = args.into_iter().unzip();
        let proc_call_node = Box::new(ast::ProcCall {
            proc: var_node,
            args: arg_nodes,
        });

        (return_type, proc_call_node)
    }

    // first(argument_list): "not", "(", "identifier", "-", "number", "string", "true", "false"
    fn argument_list(&mut self) -> Vec<(Types, Box<dyn ast::ASTNode>)> {
        let mut arg_types = Vec::new();

        //let (mut expr_type, mut expr_node) = self.expr();
        //arg_types.push(expr_type);
        arg_types.push(self.expr());

        while self.tok == Token::Comma {
            //consume comma
            self.consume_tok();

            //(expr_type, expr_node) = self.expr();
            //arg_types.push(expr_type);
            arg_types.push(self.expr());
        }

        arg_types
    }

    fn name(&mut self) -> (Types, Box<dyn ast::ASTNode>) {
        let mut parsed_type: Types;
        let mut var_id = String::from("");
        match &self.tok {
            Token::Identifier(id) => {
                match self.st.get(id) {
                    Some(types) => {
                        parsed_type = types.clone();
                        var_id = id.clone();
                    }
                    None => panic!("Missing declaration for {id}"),
                }
            }
            _ => panic!("Expected \"identifier\""),
        }
        self.consume_tok();

        let var = Box::new(ast::Var {
            id: var_id,
        });

        let expr_node: Box<dyn ast::ASTNode>;
        (parsed_type, expr_node) = self.name_prime(parsed_type.clone(), var);

        (parsed_type, expr_node)
    }

    fn name_prime(&mut self, array_type: Types, var_node: Box<ast::Var>) -> (Types, Box<dyn ast::ASTNode>) {
        // identifier already consumed

        if self.tok != Token::LSquare {
            return (array_type, var_node);
        }
        // consume LSquare
        self.consume_tok();

        let elem_type: Types;
        match array_type {
            Types::Array(_, elem) => elem_type = *elem,
            _ => panic!("Indexing can only be performed on array types"),
        }

        let (expr_type, expr_node) = self.expr();
        match expr_type {
            Types::Int => (),
            _ => panic!("Array index must be of integer type"),
        }

        if self.tok != Token::RSquare {
            panic!("Expected \"]\"");
        }
        self.consume_tok();

        let sub_node = Box::new(ast::SubscriptOp {
            array: var_node,
            index: expr_node,
        });

        (elem_type, sub_node)
    }
}

// TODO: tests
// TODO: error handling and recovery
