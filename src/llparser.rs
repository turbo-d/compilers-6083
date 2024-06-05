use crate::ast::{Ast, RelationOp};
use crate::error::CompilerError;
use crate::scanner::Scan;
use crate::token::Token;
use crate::types::Types;

use std::vec::Vec;

#[derive(Debug)]
pub struct TerminalError;

pub struct LLParser {
    s: Box<dyn Scan>,
    tok: Token,
    errs: Vec<CompilerError>,
}

impl LLParser {
    pub fn new(s: Box<dyn Scan>) -> LLParser {
        let mut ll = LLParser {
            s,
            tok: Token::Unknown,
            errs: Vec::<CompilerError>::new(),
        };
        ll.consume_tok();
        ll
    }

    pub fn get_errors(&self) -> &Vec<CompilerError> {
        &self.errs
    }

    pub fn parse(&mut self) -> Result<Box<Ast>, TerminalError> {
        let prgm_node = self.program()?;

        if self.tok != Token::EOF {
            self.errs.push(CompilerError::Warning { line: self.s.line(), msg: String::from("Extraneous trailing characters") });
        }

        Ok(prgm_node)
    }

    fn consume_tok(&mut self) {
        self.tok = self.s.scan();
    }

    fn program(&mut self) -> Result<Box<Ast>, TerminalError> {
        let name = self.program_header()?;

        let (decls, body) = self.program_body()?;

        if self.tok != Token::Period {
            self.errs.push(CompilerError::Warning { line: self.s.line(), msg: String::from("Expected \".\"") });
        }
        self.consume_tok();

        Ok(Box::new(Ast::Program {
            name: name,
            decls: decls,
            body: body,
        }))
    }

    fn program_header(&mut self) -> Result<String, TerminalError> {
        if self.tok != Token::Program {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"program\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let identifier: String;
        match &self.tok {
            Token::Identifier(id) => identifier = id.clone(),
            _ => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"identifier\"") });
                return Err(TerminalError);
            },
        }
        self.consume_tok();

        if self.tok != Token::Is {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"is\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        Ok(identifier)
    }

    // first(program_body): "global", "procedure", "variable", "begin"
    fn program_body(&mut self) -> Result<(Vec<Box<Ast>>, Vec<Box<Ast>>), TerminalError> {
        let mut decls = Vec::new();
        // first(declaration)
        while self.tok == Token::Global || self.tok == Token::Procedure || self.tok == Token::Variable {
            decls.push(self.declaration()?);

            if self.tok != Token::Semicolon {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \";\"") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        if self.tok != Token::Begin {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"begin\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut stmts = Vec::new();
        // first(statement)
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            stmts.push(self.statement()?);

            if self.tok != Token::Semicolon {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \";\"") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        if self.tok != Token::EndProgram {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"end program\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        Ok((decls, stmts))
    }

    // first(declaration): "global", "procedure", "variable"
    fn declaration(&mut self) -> Result<Box<Ast>, TerminalError> {
        let mut is_global = false;
        if self.tok == Token::Global {
            is_global = true;
            self.consume_tok();
        }

        let decl_node: Box<Ast>;
        if self.tok == Token::Procedure {
            decl_node = self.procedure_declaration(is_global)?;
        } else if self.tok == Token::Variable {
            let (_, var_decl_node) = self.variable_declaration(is_global)?;
            decl_node = Box::new(var_decl_node);
        } else {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"Procedure declaration or Variable declaration\"") });
            return Err(TerminalError);
        }

        Ok(decl_node)
    }

    fn procedure_declaration(&mut self, is_global: bool) -> Result<Box<Ast>, TerminalError> {
        let (name, proc_type, params) = self.procedure_header()?;
        let (decls, body) = self.procedure_body()?;

        Ok(Box::new(Ast::ProcDecl {
            is_global: is_global,
            name: name,
            ty: proc_type,
            params: params,
            decls: decls,
            body: body,
        }))
    }

    fn procedure_header(&mut self) -> Result<(String, Types, Vec<Ast>), TerminalError> {
        if self.tok != Token::Procedure {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"procedure\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let identifier: String;
        match &self.tok {
            Token::Identifier(id) => identifier = id.clone(),
            _ => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"identifier\"") });
                return Err(TerminalError);
            },
        }
        self.consume_tok();

        if self.tok != Token::Colon {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \":\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let return_type = self.type_mark()?;

        if self.tok != Token::LParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"(\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut params = Vec::new();
        // first(parameter_list)
        if self.tok == Token::Variable {
            params = self.parameter_list()?;
        }

        if self.tok != Token::RParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \")\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let (param_types, param_nodes): (Vec<_>, Vec<_>) = params.into_iter().unzip();
        let parsed_type = Types::Proc(Box::new(return_type), param_types);

        Ok((identifier, parsed_type, param_nodes))
    }

    fn type_mark(&mut self) -> Result<Types, TerminalError> {
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
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"type\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        Ok(parsed_type)
    }

    // first(parameter_list): "variable"
    fn parameter_list(&mut self) -> Result<Vec<(Types, Ast)>, TerminalError> {
        let mut params = Vec::new();

        params.push(self.parameter()?);

        while self.tok == Token::Comma {
            //consume comma
            self.consume_tok();

            params.push(self.parameter()?);
        }

        Ok(params)
    }

    fn parameter(&mut self) -> Result<(Types, Ast), TerminalError> {
        self.variable_declaration(false)
    }

    // TODO: Almost the same as program_body
    // first(procedure_body): "global", "procedure", "variable", "begin"
    fn procedure_body(&mut self) -> Result<(Vec<Box<Ast>>, Vec<Box<Ast>>), TerminalError> {
        let mut decls = Vec::new();
        // first(declaration)
        while self.tok == Token::Global || self.tok == Token::Procedure || self.tok == Token::Variable {
            decls.push(self.declaration()?);

            if self.tok != Token::Semicolon {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \";\"") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        if self.tok != Token::Begin {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"begin\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut stmts = Vec::new();
        // first(statement)
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            stmts.push(self.statement()?);

            if self.tok != Token::Semicolon {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \";\"") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        if self.tok != Token::EndProcedure {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"end procedure\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        Ok((decls, stmts))
    }

    fn variable_declaration(&mut self, is_global: bool) -> Result<(Types, Ast), TerminalError> {
        if self.tok != Token::Variable {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"variable\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let identifier: String;
        match &self.tok {
            Token::Identifier(id) => identifier = id.clone(),
            _ => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"identifier\"") });
                return Err(TerminalError);
            },
        }
        self.consume_tok();

        if self.tok != Token::Colon {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \":\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut parsed_type = self.type_mark()?;

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
                        self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Array size must be non-negative") });
                        return Err(TerminalError);
                    }
                }
                Token::FloatLiteral(_) => {
                    self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Array size must be a non-negative integer value") });
                    return Err(TerminalError);
                },
                _ => {
                    self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"number\"") });
                    return Err(TerminalError);
                },
            }
            self.consume_tok();

            if self.tok != Token::RSquare {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"]\"") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        let var_decl_node = Ast::VarDecl {
            is_global: is_global,
            name: identifier,
            ty: parsed_type.clone(),
        };

        Ok((parsed_type, var_decl_node))
    }

    // first(statement): "identifier", "if", "for", "return"
    fn statement(&mut self) -> Result<Box<Ast>, TerminalError> {
        let stmt_node: Box<Ast>;
        if matches!(self.tok, Token::Identifier(_)) {
            stmt_node = self.assignment_statement()?;
        } else if self.tok == Token::If {
            stmt_node = self.if_statement()?;
        } else if self.tok == Token::For {
            stmt_node = self.loop_statement()?;
        } else if self.tok == Token::Return {
            stmt_node = self.return_statement()?;
        } else {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"statement\"") });
            return Err(TerminalError);
        }
        Ok(stmt_node)
    }

    fn assignment_statement(&mut self) -> Result<Box<Ast>, TerminalError> {
        let dest_node = self.destination()?;

        if self.tok != Token::Assign {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \":=\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let expr_node = self.expr()?;

        Ok(Box::new(Ast::AssignStmt {
            dest: dest_node,
            expr: expr_node,
        }))
    }

    fn destination(&mut self) -> Result<Box<Ast>, TerminalError> {
        let var_id = match &self.tok {
            Token::Identifier(id) => id.clone(),
            _ => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"identifier\"") });
                return Err(TerminalError);
            },
        };
        self.consume_tok();

        let var_node = Box::new(Ast::Var {
            id: var_id,
        });

        if self.tok != Token::LSquare {
            return Ok(var_node);
        }
        // consume LSquare
        self.consume_tok();

        let expr_node = self.expr()?;

        if self.tok != Token::RSquare {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"]\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        Ok(Box::new(Ast::SubscriptOp {
            array: var_node,
            index: expr_node,
        }))
    }

    fn if_statement(&mut self) -> Result<Box<Ast>, TerminalError> {
        if self.tok != Token::If {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"if\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        if self.tok != Token::LParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"(\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let expr_node = self.expr()?;

        if self.tok != Token::RParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \")\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        if self.tok != Token::Then {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"then\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut then_body = Vec::new();
        // first(statement)
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            then_body.push(self.statement()?);

            if self.tok != Token::Semicolon {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \";\"") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        let mut else_body = Vec::new();
        if self.tok == Token::Else {
            // consume else
            self.consume_tok();

            // first(statement)
            while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
                else_body.push(self.statement()?);

                if self.tok != Token::Semicolon {
                    self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \";\"") });
                    return Err(TerminalError);
                }
                self.consume_tok();
            }
        }

        if self.tok != Token::EndIf {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"end if\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        Ok(Box::new(Ast::IfStmt {
            cond: expr_node,
            then_body: then_body,
            else_body: else_body,
        }))
    }

    fn loop_statement(&mut self) -> Result<Box<Ast>, TerminalError> {
        if self.tok != Token::For {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"for\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        if self.tok != Token::LParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"(\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let assign_stmt_node = self.assignment_statement()?;

        if self.tok != Token::Semicolon {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \";\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let expr_node = self.expr()?;

        if self.tok != Token::RParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \")\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut body = Vec::new();
        // first(statement)
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            body.push(self.statement()?);

            if self.tok != Token::Semicolon {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \";\"") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        if self.tok != Token::EndFor {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"end for\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        Ok(Box::new(Ast::LoopStmt {
            init: assign_stmt_node,
            cond: expr_node,
            body: body,
        }))
    }

    fn return_statement(&mut self) -> Result<Box<Ast>, TerminalError> {
        if self.tok != Token::Return {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"return\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let expr_node = self.expr()?;

        Ok(Box::new(Ast::ReturnStmt {
            expr: expr_node,
        }))
    }

    // first(expr): "not", "(", "identifier", "-", "number", "string", "true", "false"
    fn expr(&mut self) -> Result<Box<Ast>, TerminalError> {
        let mut do_complement = false;
        if self.tok == Token::Not {
            do_complement = true;
            // consume not token
            self.consume_tok();
        }

        let lhs_node = self.arith_op()?;

        let expr_node = self.expr_prime(lhs_node)?;

        if do_complement {
            return Ok(Box::new(Ast::NotOp {
                operand: expr_node,
            }));
        }

        Ok(expr_node)
    }

    fn expr_prime(&mut self, lhs_node: Box<Ast>) -> Result<Box<Ast>, TerminalError> {
        if self.tok != Token::And && self.tok != Token::Or {
            // null body production
            // TODO: check follow() for error checking
            return Ok(lhs_node);
        }

        let op = self.tok.clone();
        // consume token
        self.consume_tok();

        let rhs_node = self.arith_op()?;

        let expr_node: Box<Ast> = match op {
            Token::And => {
                Box::new(Ast::AndOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            Token::Or => {
                Box::new(Ast::OrOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            _ => panic!("Cannot create ast node"),
        };

        self.expr_prime(expr_node)
    }

    // first(arith_op): "(", "identifier", "-", "number", "string", "true", "false"
    fn arith_op(&mut self) -> Result<Box<Ast>, TerminalError> {
        let lhs_node = self.relation()?;

        self.arith_op_prime(lhs_node)
    }

    fn arith_op_prime(&mut self, lhs_node: Box<Ast>) -> Result<Box<Ast>, TerminalError> {
        if self.tok != Token::Add && self.tok != Token::Sub {
            // null body production
            // TODO: check follow() for error checking
            return Ok(lhs_node);
        }

        let op = self.tok.clone();
        // consume token
        self.consume_tok();

        let rhs_node = self.relation()?;

        let arith_op_node: Box<Ast> = match op {
            Token::Add => {
                Box::new(Ast::AddOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            Token::Sub => {
                Box::new(Ast::SubOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            _ => panic!("Cannot create ast node"),
        };

        self.arith_op_prime(arith_op_node)
    }

    // first(relation): "(", "identifier", "-", "number", "string", "true", "false"
    fn relation(&mut self) -> Result<Box<Ast>, TerminalError> {
        let lhs_node = self.term()?;

        self.relation_prime(lhs_node)
    }

    fn relation_prime(&mut self, lhs_node: Box<Ast>) -> Result<Box<Ast>, TerminalError> {
        if self.tok != Token::LT && 
            self.tok != Token::LTE &&
            self.tok != Token::GT &&
            self.tok != Token::GTE &&
            self.tok != Token::Eq &&
            self.tok != Token::NotEq {
            // null body production
            // TODO: check follow() for error checking
            return Ok(lhs_node);
        }

        let op = self.tok.clone();
        // consume token
        self.consume_tok();

        let rhs_node = self.term()?;

        let rel_op = match op {
            Token::LT => RelationOp::LT,
            Token::LTE => RelationOp::LTE,
            Token::GT => RelationOp::GT,
            Token::GTE => RelationOp::GTE,
            Token::Eq => RelationOp::Eq,
            Token::NotEq => RelationOp::NotEq,
            _ => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected a relational operator") });
                return Err(TerminalError);
            },
        };

        let rel_node = Box::new(Ast::Relation {
            op: rel_op,
            lhs: lhs_node,
            rhs: rhs_node,
        });

        self.relation_prime(rel_node)
    }

    // first(term): "(", "identifier", "-", "number", "string", "true", "false"
    fn term(&mut self) -> Result<Box<Ast>, TerminalError> {
        let lhs_node = self.factor()?;

        self.term_prime(lhs_node)
    }

    fn term_prime(&mut self, lhs_node: Box<Ast>) -> Result<Box<Ast>, TerminalError> {
        if self.tok != Token::Mul && self.tok != Token::Div {
            // null body production
            // TODO: check follow() for error checking
            return Ok(lhs_node);
        }

        let op = self.tok.clone();
        // consume token
        self.consume_tok();

        let rhs_node = self.factor()?;

        let term_node: Box<Ast> = match op {
            Token::Mul => {
                Box::new(Ast::MulOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            Token::Div => {
                Box::new(Ast::DivOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            _ => panic!("Cannot create ast node"),
        };

        self.term_prime(term_node)
    }

    // first(factor): "(", "identifier", "-", "number", "string", "true", "false"
    fn factor(&mut self) -> Result<Box<Ast>, TerminalError> {
        let factor_node: Box<Ast>;

        let tok = self.tok.clone();
        match tok {
            Token::Identifier(id) => {
                // consume identifier
                self.consume_tok();

                let var = Box::new(Ast::Var {
                    id: id,
                });

                if self.tok == Token::LParen {
                    factor_node = self.procedure_call_prime(var)?;
                } else {
                    factor_node = self.name_prime(var)?;
                }
            }
            Token::Sub => {
                // consume minus
                self.consume_tok();

                let negate_operand_node: Box<Ast>;
                let tok = self.tok.clone();
                match tok {
                    Token::Identifier(_) => {
                        negate_operand_node = self.name()?;
                    }
                    Token::IntLiteral(val) => {
                        // consume number
                        self.consume_tok();
                        negate_operand_node = Box::new(Ast::IntLiteral {
                            value: val,
                        });
                    }
                    Token::FloatLiteral(val) => {
                        // consume number
                        self.consume_tok();
                        negate_operand_node = Box::new(Ast::FloatLiteral {
                            value: val,
                        });
                    }
                    _ => {
                        self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"identifier\" or \"number\" following \"-\"") });
                        return Err(TerminalError);
                    },
                }

                factor_node = Box::new(Ast::NegateOp {
                    operand: negate_operand_node,
                });
            }
            Token::IntLiteral(val) => {
                // consume number
                self.consume_tok();
                factor_node = Box::new(Ast::IntLiteral {
                    value: val,
                });
            }
            Token::FloatLiteral(val) => {
                // consume number
                self.consume_tok();
                factor_node = Box::new(Ast::FloatLiteral {
                    value: val,
                });
            }
            Token::LParen => {
                // consume left paren
                self.consume_tok();

                factor_node = self.expr()?;

                if self.tok != Token::RParen {
                    self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \")\"") });
                    return Err(TerminalError);
                }
                self.consume_tok();
            }
            Token::String(val) => {
                // consume string
                self.consume_tok();
                factor_node = Box::new(Ast::StringLiteral {
                    value: val.clone(),
                });
            }
            Token::True => {
                // consume true
                self.consume_tok();
                factor_node = Box::new(Ast::BoolLiteral {
                    value: true,
                });
            }
            Token::False => {
                // consume false
                self.consume_tok();
                factor_node = Box::new(Ast::BoolLiteral {
                    value: false,
                });
            }
            _ => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"factor\"") });
                return Err(TerminalError);
            },
        }

        Ok(factor_node)
    }

    fn procedure_call_prime(&mut self, var_node: Box<Ast>) -> Result<Box<Ast>, TerminalError> {
        // identifier already consumed

        if self.tok != Token::LParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \"(\"") });
            return Err(TerminalError);
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
            args = self.argument_list()?;
        }

        if self.tok != Token::RParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected \")\"") });
            return Err(TerminalError);
        }
        self.consume_tok();

        Ok(Box::new(Ast::ProcCall {
            proc: var_node,
            args: args,
        }))
    }

    // first(argument_list): "not", "(", "identifier", "-", "number", "string", "true", "false"
    fn argument_list(&mut self) -> Result<Vec<Box<Ast>>, TerminalError> {
        let mut args = Vec::new();

        //let (mut expr_type, mut expr_node) = self.expr();
        //arg_types.push(expr_type);
        args.push(self.expr()?);

        while self.tok == Token::Comma {
            //consume comma
            self.consume_tok();

            //(expr_type, expr_node) = self.expr();
            //arg_types.push(expr_type);
            args.push(self.expr()?);
        }

        Ok(args)
    }

    fn name(&mut self) -> Result<Box<Ast>, TerminalError> {
        let var_id = match &self.tok {
            Token::Identifier(id) => id.clone(),
            tok => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected identifier, found {tok}") });
                return Err(TerminalError);
            },
        };
        self.consume_tok();

        let var = Box::new(Ast::Var {
            id: var_id,
        });

        self.name_prime(var)
    }

    fn name_prime(&mut self, var_node: Box<Ast>) -> Result<Box<Ast>, TerminalError> {
        if self.tok != Token::LSquare {
            return Ok(var_node);
        }
        self.consume_tok();

        let expr_node = self.expr()?;

        if self.tok != Token::RSquare {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected ], found {}", self.tok) });
            return Err(TerminalError);
        }
        self.consume_tok();

        let sub_node = Box::new(Ast::SubscriptOp {
            array: var_node,
            index: expr_node,
        });

        Ok(sub_node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestScanner {
        toks: Vec<Token>,
        i: usize,
        line: u32,
    }

    impl TestScanner {
        fn new(toks: Vec<Token>) -> TestScanner {
            TestScanner {
                toks,
                i: 0,
                line: 1,
            }
        }
    }

    impl Scan for TestScanner {
        fn scan(&mut self) -> Token {
            if self.i < self.toks.len() {
                self.i += 1;
                self.toks[self.i-1].clone()
            } else {
                Token::Unknown
            }
        }

        fn line(&self) -> u32 {
            self.line
        }
    }

    #[test]
    fn llparse_test() {
        let toks = vec![
            Token::Identifier(String::from("a")),
            Token::Add,
            Token::Identifier(String::from("b"))
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = match p.expr() {
            Ok(ast) => ast,
            Err(_) => panic!(""),
        };

        let exp_ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::Var { id: String::from("a") }),
            rhs: Box::new(Ast::Var { id: String::from("b") }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_name() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.name().expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_name_err_missingidentifier() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.name().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected identifier, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_name_prime_null() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.name_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_name_prime_subscript() {
        let toks = vec![
            Token::LSquare,
            Token::IntLiteral(1),
            Token::RSquare,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.name_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::SubscriptOp { 
            array: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            index: Box::new(Ast::IntLiteral {
                value: 1,
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_name_prime_err_missingrsquare() {
        let toks = vec![
            Token::LSquare,
            Token::IntLiteral(1),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        p.name_prime(in_ast).expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected ], found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }
}
