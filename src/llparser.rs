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

    pub fn parse(&mut self) -> Box<dyn ast::ASTNode> {
        self.consume_tok();

        let prgm_node = self.program();

        if self.tok != Token::EOF {
            eprintln!("Extraneous trailing characters");
        }

        prgm_node
    }

    fn consume_tok(&mut self) {
        self.tok = self.s.scan();
    }

    fn program(&mut self) -> Box<dyn ast::ASTNode> {
        let name = self.program_header();

        let (decls, body) = self.program_body();

        if self.tok != Token::Period {
            // TODO: Probably want to warn here instead
            panic!("Expected \".\"");
        }
        self.consume_tok();

        Box::new(ast::Program {
            name: name,
            decls: decls,
            body: body,
        })
    }

    fn program_header(&mut self) -> String {
        if self.tok != Token::Program {
            panic!("Expected \"program\"");
        }
        self.consume_tok();

        let identifier: String;
        match &self.tok {
            Token::Identifier(id) => identifier = id.clone(),
            _ => panic!("Expected \"identifier\""),
        }
        self.consume_tok();

        if self.tok != Token::Is {
            panic!("Expected \"is\"");
        }
        self.consume_tok();

        identifier
    }

    // first(program_body): "global", "procedure", "variable", "begin"
    fn program_body(&mut self) -> (Vec<Box<dyn ast::ASTNode>>, Vec<Box<dyn ast::ASTNode>>) {
        let mut decls = Vec::new();
        // first(declaration)
        while self.tok == Token::Global || self.tok == Token::Procedure || self.tok == Token::Variable {
            decls.push(self.declaration());

            if self.tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
            self.consume_tok();
        }

        if self.tok != Token::Begin {
            panic!("Expected \"begin\"");
        }
        self.consume_tok();

        let mut stmts = Vec::new();
        // first(statement)
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            stmts.push(self.statement());

            if self.tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
            self.consume_tok();
        }

        if self.tok != Token::EndProgram {
            panic!("Expected \"end program\"");
        }
        self.consume_tok();

        (decls, stmts)
    }

    // first(declaration): "global", "procedure", "variable"
    fn declaration(&mut self) -> Box<dyn ast::ASTNode> {
        let mut is_global = false;
        if self.tok == Token::Global {
            is_global = true;
            self.consume_tok();
        }

        let decl_node: Box<dyn ast::ASTNode>;
        if self.tok == Token::Procedure {
            decl_node = self.procedure_declaration(is_global);
        } else if self.tok == Token::Variable {
            (_, decl_node) = self.variable_declaration(is_global);
        } else {
            panic!("Expected \"Procedure declaration or Variable declaration\"");
        }

        decl_node
    }

    fn procedure_declaration(&mut self, is_global: bool) -> Box<dyn ast::ASTNode> {
        let (name, proc_type, params) = self.procedure_header(is_global);
        let (decls, body) = self.procedure_body(proc_type.clone());

        Box::new(ast::ProcDecl {
            is_global: is_global,
            name: name,
            ty: proc_type,
            params: params,
            decls: decls,
            body: body,
        })
    }

    fn procedure_header(&mut self, is_global: bool) -> (String, Types, Vec<Box<dyn ast::ASTNode>>) {
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

        let mut params = Vec::new();
        // first(parameter_list)
        if self.tok == Token::Variable {
            params = self.parameter_list();
        }

        if self.tok != Token::RParen {
            panic!("Expected \")\"");
        }
        self.consume_tok();

        let (param_types, param_nodes): (Vec<_>, Vec<_>) = params.into_iter().unzip();
        let parsed_type = Types::Proc(Box::new(return_type), param_types);

        (identifier, parsed_type, param_nodes)
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
    fn parameter_list(&mut self) -> Vec<(Types, Box<dyn ast::ASTNode>)> {
        let mut params = Vec::new();

        params.push(self.parameter());

        while self.tok == Token::Comma {
            //consume comma
            self.consume_tok();

            params.push(self.parameter());
        }

        params
    }

    fn parameter(&mut self) -> (Types, Box<dyn ast::ASTNode>) {
        self.variable_declaration(false)
    }

    // TODO: Almost the same as program_body
    // first(procedure_body): "global", "procedure", "variable", "begin"
    fn procedure_body(&mut self, proc_type: Types) -> (Vec<Box<dyn ast::ASTNode>>, Vec<Box<dyn ast::ASTNode>>) {
        let mut decls = Vec::new();
        // first(declaration)
        while self.tok == Token::Global || self.tok == Token::Procedure || self.tok == Token::Variable {
            decls.push(self.declaration());

            if self.tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
            self.consume_tok();
        }

        if self.tok != Token::Begin {
            panic!("Expected \"begin\"");
        }
        self.consume_tok();

        let mut stmts = Vec::new();
        // first(statement)
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            stmts.push(self.statement());

            if self.tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
            self.consume_tok();
        }

        if self.tok != Token::EndProcedure {
            panic!("Expected \"end procedure\"");
        }
        self.consume_tok();

        (decls, stmts)
    }

    fn variable_declaration(&mut self, is_global: bool) -> (Types, Box<dyn ast::ASTNode>) {
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

            // TODO: Move to type checking (semantic checking)?
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

        let var_decl_node = Box::new(ast::VarDecl {
            is_global: is_global,
            name: identifier,
            ty: parsed_type.clone(),
        });

        (parsed_type, var_decl_node)
    }

    // first(statement): "identifier", "if", "for", "return"
    fn statement(&mut self) -> Box<dyn ast::ASTNode> {
        let stmt_node: Box<dyn ast::ASTNode>;
        if matches!(self.tok, Token::Identifier(_)) {
            stmt_node = self.assignment_statement();
        } else if self.tok == Token::If {
            stmt_node = self.if_statement();
        } else if self.tok == Token::For {
            stmt_node = self.loop_statement();
        } else if self.tok == Token::Return {
            stmt_node = self.return_statement();
        } else {
            panic!("Expected \"statement\"");
        }
        stmt_node
    }

    fn assignment_statement(&mut self) -> Box<dyn ast::ASTNode> {
        let dest_node = self.destination();

        if self.tok != Token::Assign {
            panic!("Expected \":=\"");
        }
        self.consume_tok();

        let expr_node = self.expr();

        Box::new(ast::AssignStmt {
            dest: dest_node,
            expr: expr_node,
        })
    }

    fn destination(&mut self) -> Box<dyn ast::ASTNode> {
        let var_id = match &self.tok {
            Token::Identifier(id) => id.clone(),
            _ => panic!("Expected \"identifier\""),
        };
        self.consume_tok();

        let var_node = Box::new(ast::Var {
            id: var_id,
        });

        if self.tok != Token::LSquare {
            return var_node;
        }
        // consume LSquare
        self.consume_tok();

        let expr_node = self.expr();

        if self.tok != Token::RSquare {
            panic!("Expected \"]\"");
        }
        self.consume_tok();

        Box::new(ast::SubscriptOp {
            array: var_node,
            index: expr_node,
        })
    }

    fn if_statement(&mut self) -> Box<dyn ast::ASTNode> {
        if self.tok != Token::If {
            panic!("Expected \"if\"");
        }
        self.consume_tok();

        if self.tok != Token::LParen {
            panic!("Expected \"(\"");
        }
        self.consume_tok();

        let expr_node = self.expr();

        if self.tok != Token::RParen {
            panic!("Expected \")\"");
        }
        self.consume_tok();

        if self.tok != Token::Then {
            panic!("Expected \"then\"");
        }
        self.consume_tok();

        let mut then_body = Vec::new();
        // first(statement)
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            then_body.push(self.statement());

            if self.tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
            self.consume_tok();
        }

        let mut else_body = Vec::new();
        if self.tok == Token::Else {
            // consume else
            self.consume_tok();

            // first(statement)
            while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
                else_body.push(self.statement());

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

        Box::new(ast::IfStmt {
            cond: expr_node,
            then_body: then_body,
            else_body: else_body,
        })
    }

    fn loop_statement(&mut self) -> Box<dyn ast::ASTNode> {
        if self.tok != Token::For {
            panic!("Expected \"for\"");
        }
        self.consume_tok();

        if self.tok != Token::LParen {
            panic!("Expected \"(\"");
        }
        self.consume_tok();

        let assign_stmt_node = self.assignment_statement();

        if self.tok != Token::Semicolon {
            panic!("Expected \";\"");
        }
        self.consume_tok();

        let expr_node = self.expr();

        if self.tok != Token::RParen {
            panic!("Expected \")\"");
        }
        self.consume_tok();

        let mut body = Vec::new();
        // first(statement)
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            body.push(self.statement());

            if self.tok != Token::Semicolon {
                panic!("Expected \";\"");
            }
            self.consume_tok();
        }

        if self.tok != Token::EndFor {
            panic!("Expected \"end for\"");
        }
        self.consume_tok();

        Box::new(ast::LoopStmt {
            init: assign_stmt_node,
            cond: expr_node,
            body: body,
        })
    }

    fn return_statement(&mut self) -> Box<dyn ast::ASTNode> {
        if self.tok != Token::Return {
            panic!("Expected \"return\"");
        }
        self.consume_tok();

        let expr_node = self.expr();

        Box::new(ast::ReturnStmt {
            expr: expr_node,
        })
    }

    // first(expr): "not", "(", "identifier", "-", "number", "string", "true", "false"
    fn expr(&mut self) -> Box<dyn ast::ASTNode> {
        let mut do_complement = false;
        if self.tok == Token::Not {
            do_complement = true;
            // consume not token
            self.consume_tok();
        }

        let lhs_node = self.arith_op();

        let expr_node = self.expr_prime(lhs_node);

        if do_complement {
            return Box::new(ast::NotOp {
                operand: expr_node,
            });
        }

        expr_node
    }

    fn expr_prime(&mut self, lhs_node: Box<dyn ast::ASTNode>) -> Box<dyn ast::ASTNode> {
        if self.tok != Token::And && self.tok != Token::Or {
            // null body production
            // TODO: check follow() for error checking
            return lhs_node;
        }

        let op = self.tok.clone();
        // consume token
        self.consume_tok();

        let rhs_node = self.arith_op();

        let expr_node: Box<dyn ast::ASTNode> = match op {
            Token::And => {
                Box::new(ast::AndOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            Token::Or => {
                Box::new(ast::OrOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            _ => panic!("Cannot create ast node"),
        };

        self.expr_prime(expr_node)
    }

    // first(arith_op): "(", "identifier", "-", "number", "string", "true", "false"
    fn arith_op(&mut self) -> Box<dyn ast::ASTNode> {
        let lhs_node = self.relation();

        self.arith_op_prime(lhs_node)
    }

    fn arith_op_prime(&mut self, lhs_node: Box<dyn ast::ASTNode>) -> Box<dyn ast::ASTNode> {
        if self.tok != Token::Add && self.tok != Token::Sub {
            // null body production
            // TODO: check follow() for error checking
            return lhs_node;
        }

        let op = self.tok.clone();
        // consume token
        self.consume_tok();

        let rhs_node = self.relation();

        let arith_op_node: Box<dyn ast::ASTNode> = match op {
            Token::Add => {
                Box::new(ast::AddOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            Token::Sub => {
                Box::new(ast::SubOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            _ => panic!("Cannot create ast node"),
        };

        self.arith_op_prime(arith_op_node)
    }

    // first(relation): "(", "identifier", "-", "number", "string", "true", "false"
    fn relation(&mut self) -> Box<dyn ast::ASTNode> {
        let lhs_node = self.term();

        self.relation_prime(lhs_node)
    }

    fn relation_prime(&mut self, lhs_node: Box<dyn ast::ASTNode>) -> Box<dyn ast::ASTNode> {
        if self.tok != Token::LT && 
            self.tok != Token::LTE &&
            self.tok != Token::GT &&
            self.tok != Token::GTE &&
            self.tok != Token::Eq &&
            self.tok != Token::NotEq {
            // null body production
            // TODO: check follow() for error checking
            return lhs_node;
        }

        let op = self.tok.clone();
        // consume token
        self.consume_tok();

        let rhs_node = self.term();

        let rel_op = match op {
            Token::LT => ast::RelationOp::LT,
            Token::LTE => ast::RelationOp::LTE,
            Token::GT => ast::RelationOp::GT,
            Token::GTE => ast::RelationOp::GTE,
            Token::Eq => ast::RelationOp::Eq,
            Token::NotEq => ast::RelationOp::NotEq,
            _ => panic!("Invalid relation token"),
        };

        let rel_node = Box::new(ast::Relation {
            op: rel_op,
            lhs: lhs_node,
            rhs: rhs_node,
        });

        self.relation_prime(rel_node)
    }

    // first(term): "(", "identifier", "-", "number", "string", "true", "false"
    fn term(&mut self) -> Box<dyn ast::ASTNode> {
        let lhs_node = self.factor();

        self.term_prime(lhs_node)
    }

    fn term_prime(&mut self, lhs_node: Box<dyn ast::ASTNode>) -> Box<dyn ast::ASTNode> {
        if self.tok != Token::Mul && self.tok != Token::Div {
            // null body production
            // TODO: check follow() for error checking
            return lhs_node;
        }

        let op = self.tok.clone();
        // consume token
        self.consume_tok();

        let rhs_node = self.factor();

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

        self.term_prime(term_node)
    }

    // first(factor): "(", "identifier", "-", "number", "string", "true", "false"
    fn factor(&mut self) -> Box<dyn ast::ASTNode> {
        let mut factor_node: Box<dyn ast::ASTNode>;

        let tok = self.tok.clone();
        match tok {
            Token::Identifier(id) => {
                // consume identifier
                self.consume_tok();

                let var = Box::new(ast::Var {
                    id: id,
                });

                if self.tok == Token::LParen {
                    factor_node = self.procedure_call_prime(var);
                } else {
                    factor_node = self.name_prime(var);
                }
            }
            Token::Sub => {
                // consume minus
                self.consume_tok();

                let mut negate_operand_node: Box<dyn ast::ASTNode>;
                let tok = self.tok.clone();
                match tok {
                    Token::Identifier(_) => {
                        negate_operand_node = self.name();
                    }
                    Token::IntLiteral(val) => {
                        // consume number
                        self.consume_tok();
                        negate_operand_node = Box::new(ast::IntLiteral {
                            value: val,
                        });
                    }
                    Token::FloatLiteral(val) => {
                        // consume number
                        self.consume_tok();
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
                factor_node = Box::new(ast::IntLiteral {
                    value: val,
                });
            }
            Token::FloatLiteral(val) => {
                // consume number
                self.consume_tok();
                factor_node = Box::new(ast::FloatLiteral {
                    value: val,
                });
            }
            Token::LParen => {
                // consume left paren
                self.consume_tok();

                factor_node = self.expr();

                if self.tok != Token::RParen {
                    panic!("Expected \")\"");
                }
                self.consume_tok();
            }
            Token::String(val) => {
                // consume string
                self.consume_tok();
                factor_node = Box::new(ast::StringLiteral {
                    value: val.clone(),
                });
            }
            Token::True => {
                // consume true
                self.consume_tok();
                factor_node = Box::new(ast::BoolLiteral {
                    value: true,
                });
            }
            Token::False => {
                // consume false
                self.consume_tok();
                factor_node = Box::new(ast::BoolLiteral {
                    value: false,
                });
            }
            _ => panic!("Expected \"factor\""),
        }

        factor_node
    }

    fn procedure_call_prime(&mut self, var_node: Box<ast::Var>) -> Box<dyn ast::ASTNode> {
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

        Box::new(ast::ProcCall {
            proc: var_node,
            args: args,
        })
    }

    // first(argument_list): "not", "(", "identifier", "-", "number", "string", "true", "false"
    fn argument_list(&mut self) -> Vec<Box<dyn ast::ASTNode>> {
        let mut args = Vec::new();

        //let (mut expr_type, mut expr_node) = self.expr();
        //arg_types.push(expr_type);
        args.push(self.expr());

        while self.tok == Token::Comma {
            //consume comma
            self.consume_tok();

            //(expr_type, expr_node) = self.expr();
            //arg_types.push(expr_type);
            args.push(self.expr());
        }

        args
    }

    fn name(&mut self) -> Box<dyn ast::ASTNode> {
        let var_id;
        match &self.tok {
            Token::Identifier(id) => var_id = id.clone(),
            _ => panic!("Expected \"identifier\""),
        }
        self.consume_tok();

        let var = Box::new(ast::Var {
            id: var_id,
        });

        self.name_prime(var)
    }

    fn name_prime(&mut self, var_node: Box<ast::Var>) -> Box<dyn ast::ASTNode> {
        // identifier already consumed

        if self.tok != Token::LSquare {
            return var_node;
        }
        // consume LSquare
        self.consume_tok();

        let expr_node = self.expr();

        if self.tok != Token::RSquare {
            panic!("Expected \"]\"");
        }
        self.consume_tok();

        let sub_node = Box::new(ast::SubscriptOp {
            array: var_node,
            index: expr_node,
        });

        sub_node
    }
}

// TODO: tests
// TODO: error handling and recovery
