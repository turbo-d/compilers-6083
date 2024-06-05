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
        // prime the parser with the first token
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
        match &self.tok {
            Token::Identifier(_) => Ok(self.assignment_statement()?),
            Token::If => Ok(self.if_statement()?),
            Token::For => Ok(self.loop_statement()?),
            Token::Return => Ok(self.return_statement()?),
            tok => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected statement (assignment, if, for, or return), found {tok}") });
                return Err(TerminalError);
            },
        }
    }

    fn assignment_statement(&mut self) -> Result<Box<Ast>, TerminalError> {
        let dest_node = self.destination()?;

        if self.tok != Token::Assign {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected :=, found {}", self.tok) });
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
            tok => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected identifier, found {tok}") });
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
        self.consume_tok();

        let expr_node = self.expr()?;

        if self.tok != Token::RSquare {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected ], found {}", self.tok) });
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
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing if keyword") });
            return Err(TerminalError);
        }
        self.consume_tok();

        if self.tok != Token::LParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected (, found {}", self.tok) });
            return Err(TerminalError);
        }
        self.consume_tok();

        let expr_node = self.expr()?;

        if self.tok != Token::RParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected ), found {}", self.tok) });
            return Err(TerminalError);
        }
        self.consume_tok();

        if self.tok != Token::Then {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing then keyword") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut then_body = Vec::new();
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            then_body.push(self.statement()?);

            if self.tok != Token::Semicolon {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected ;") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        let mut else_body = Vec::new();
        if self.tok == Token::Else {
            self.consume_tok();

            while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
                else_body.push(self.statement()?);

                if self.tok != Token::Semicolon {
                    self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected ;") });
                    return Err(TerminalError);
                }
                self.consume_tok();
            }
        }

        if self.tok != Token::EndIf {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing end if keyword") });
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
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing for keyword") });
            return Err(TerminalError);
        }
        self.consume_tok();

        if self.tok != Token::LParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected (, found {}", self.tok) });
            return Err(TerminalError);
        }
        self.consume_tok();

        let assign_stmt_node = self.assignment_statement()?;

        if self.tok != Token::Semicolon {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected ; after loop initialization statement, found {}", self.tok) });
            return Err(TerminalError);
        }
        self.consume_tok();

        let expr_node = self.expr()?;

        if self.tok != Token::RParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected ), found {}", self.tok) });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut body = Vec::new();
        while matches!(self.tok, Token::Identifier(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            body.push(self.statement()?);

            if self.tok != Token::Semicolon {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected ;") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        if self.tok != Token::EndFor {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing end for keyword") });
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
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing return keyword") });
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
        let do_complement =
            if self.tok == Token::Not {
                self.consume_tok();
                true
            } else {
                false
            };

        let lhs_node = self.arith_op()?;

        let expr_node = self.expr_prime(lhs_node)?;

        if do_complement {
            Ok(Box::new(Ast::NotOp {
                operand: expr_node,
            }))
        } else {
            Ok(expr_node)
        }
    }

    fn expr_prime(&mut self, lhs_node: Box<Ast>) -> Result<Box<Ast>, TerminalError> {
        if self.tok != Token::And && self.tok != Token::Or {
            return Ok(lhs_node);
        }

        let op = self.tok.clone();
        self.consume_tok();

        let rhs_node = self.arith_op()?;

        let expr_node: Box<Ast> = match op {
            Token::And => {
                Box::new(Ast::AndOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            _ => {
                Box::new(Ast::OrOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
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
            return Ok(lhs_node);
        }

        let op = self.tok.clone();
        self.consume_tok();

        let rhs_node = self.relation()?;

        let arith_op_node: Box<Ast> = match op {
            Token::Add => {
                Box::new(Ast::AddOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            _ => {
                Box::new(Ast::SubOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
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
            return Ok(lhs_node);
        }

        let op = self.tok.clone();
        self.consume_tok();

        let rhs_node = self.term()?;

        let rel_op = match op {
            Token::LT => RelationOp::LT,
            Token::LTE => RelationOp::LTE,
            Token::GT => RelationOp::GT,
            Token::GTE => RelationOp::GTE,
            Token::Eq => RelationOp::Eq,
            _ => RelationOp::NotEq,
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
            return Ok(lhs_node);
        }

        let op = self.tok.clone();
        self.consume_tok();

        let rhs_node = self.factor()?;

        let term_node: Box<Ast> = match op {
            Token::Mul => {
                Box::new(Ast::MulOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
            _ => {
                Box::new(Ast::DivOp {
                    lhs: lhs_node,
                    rhs: rhs_node,
                })
            }
        };

        self.term_prime(term_node)
    }

    // first(factor): "(", "identifier", "-", "number", "string", "true", "false"
    fn factor(&mut self) -> Result<Box<Ast>, TerminalError> {
        let tok = self.tok.clone();
        match tok {
            Token::Identifier(id) => {
                self.consume_tok();

                let var = Box::new(Ast::Var {
                    id: id,
                });

                if self.tok == Token::LParen {
                    Ok(self.procedure_call_prime(var)?)
                } else {
                    Ok(self.name_prime(var)?)
                }
            }
            Token::Sub => {
                self.consume_tok();

                let tok = self.tok.clone();
                let negate_operand_node = match tok {
                    Token::Identifier(_) => self.name()?,
                    Token::IntLiteral(val) => {
                        self.consume_tok();
                        Box::new(Ast::IntLiteral {
                            value: val,
                        })
                    }
                    Token::FloatLiteral(val) => {
                        self.consume_tok();
                        Box::new(Ast::FloatLiteral {
                            value: val,
                        })
                    }
                    tok => {
                        self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected identifier or numeric literal following -, found {tok}") });
                        return Err(TerminalError);
                    },
                };

                Ok(Box::new(Ast::NegateOp {
                    operand: negate_operand_node,
                }))
            }
            Token::IntLiteral(val) => {
                self.consume_tok();
                Ok(Box::new(Ast::IntLiteral {
                    value: val,
                }))
            }
            Token::FloatLiteral(val) => {
                self.consume_tok();
                Ok(Box::new(Ast::FloatLiteral {
                    value: val,
                }))
            }
            Token::LParen => {
                self.consume_tok();

                let factor_node = self.expr()?;

                if self.tok != Token::RParen {
                    self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected ), found {}", self.tok) });
                    return Err(TerminalError);
                }
                self.consume_tok();

                Ok(factor_node)
            }
            Token::String(val) => {
                self.consume_tok();
                Ok(Box::new(Ast::StringLiteral {
                    value: val.clone(),
                }))
            }
            Token::True => {
                self.consume_tok();
                Ok(Box::new(Ast::BoolLiteral {
                    value: true,
                }))
            }
            Token::False => {
                self.consume_tok();
                Ok(Box::new(Ast::BoolLiteral {
                    value: false,
                }))
            }
            tok => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected one of: (, -, numeric literal, string literal, boolean literal, identifier, found {tok}") });
                return Err(TerminalError);
            },
        }
    }

    fn procedure_call_prime(&mut self, var_node: Box<Ast>) -> Result<Box<Ast>, TerminalError> {
        if self.tok != Token::LParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected (, found {}", self.tok) });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut args = Vec::new();
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
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected ), found {}", self.tok) });
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

        args.push(self.expr()?);

        while self.tok == Token::Comma {
            self.consume_tok();

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
    fn llparse_statement_assign() {
        let toks = vec![
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::AssignStmt { 
            dest: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            expr: Box::new(Ast::IntLiteral { 
                value: 1,
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_statement_if() {
        let toks = vec![
            Token::If,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::Then,
            Token::EndIf,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: Vec::new(),
            else_body: Vec::new(),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_statement_for() {
        let toks = vec![
            Token::For,
            Token::LParen,
            Token::Identifier(String::from("i")),
            Token::Assign,
            Token::IntLiteral(5),
            Token::Semicolon,
            Token::True,
            Token::RParen,
            Token::EndFor,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::LoopStmt { 
            init: Box::new(Ast::AssignStmt { 
                dest: Box::new(Ast::Var { 
                    id: String::from("i") 
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 5,
                }),
            }),
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            body: Vec::new(),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_statement_return() {
        let toks = vec![
            Token::Return,
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::ReturnStmt { 
            expr: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_statement_err_invalidstatement() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected statement (assignment, if, for, or return), found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_assignment_statement() {
        let toks = vec![
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.assignment_statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::AssignStmt { 
            dest: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            expr: Box::new(Ast::IntLiteral { 
                value: 1,
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_assignment_statement_err_missingassignmentoperator() {
        let toks = vec![
            Token::Identifier(String::from("a")),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.assignment_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected :=, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_destination_nosubscript() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.destination().expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_destination_withsubscript() {
        let toks = vec![
            Token::Identifier(String::from("a")),
            Token::LSquare,
            Token::IntLiteral(1),
            Token::RSquare,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.destination().expect("Parse failed");

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
    fn llparse_destination_err_missingidentifier() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.destination().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected identifier, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_destination_err_missingrsquare() {
        let toks = vec![
            Token::Identifier(String::from("a")),
            Token::LSquare,
            Token::IntLiteral(1),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.destination().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected ], found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_if_statement_emptythen_noelse() {
        let toks = vec![
            Token::If,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::Then,
            Token::EndIf,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.if_statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: Vec::new(),
            else_body: Vec::new(),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_if_statement_singlestmtthen_noelse() {
        let toks = vec![
            Token::If,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::Then,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Semicolon,
            Token::EndIf,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.if_statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: vec![
                Box::new(Ast::AssignStmt { 
                    dest: Box::new(Ast::Var { 
                        id: String::from("a") 
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 1,
                    }),
                }),
            ],
            else_body: Vec::new(),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_if_statement_multistmtthen_noelse() {
        let toks = vec![
            Token::If,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::Then,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Semicolon,
            Token::Identifier(String::from("b")),
            Token::Assign,
            Token::IntLiteral(2),
            Token::Semicolon,
            Token::EndIf,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.if_statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: vec![
                Box::new(Ast::AssignStmt { 
                    dest: Box::new(Ast::Var { 
                        id: String::from("a") 
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 1,
                    }),
                }),
                Box::new(Ast::AssignStmt { 
                    dest: Box::new(Ast::Var { 
                        id: String::from("b") 
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 2,
                    }),
                }),
            ],
            else_body: Vec::new(),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_if_statement_emptythen_emptyelse() {
        let toks = vec![
            Token::If,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::Then,
            Token::Else,
            Token::EndIf,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.if_statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: Vec::new(),
            else_body: Vec::new(),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_if_statement_emptythen_singlestmtelse() {
        let toks = vec![
            Token::If,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::Then,
            Token::Else,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Semicolon,
            Token::EndIf,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.if_statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: Vec::new(),
            else_body: vec![
                Box::new(Ast::AssignStmt { 
                    dest: Box::new(Ast::Var { 
                        id: String::from("a") 
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 1,
                    }),
                }),
            ],
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_if_statement_emptythen_multistmtelse() {
        let toks = vec![
            Token::If,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::Then,
            Token::Else,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Semicolon,
            Token::Identifier(String::from("b")),
            Token::Assign,
            Token::IntLiteral(2),
            Token::Semicolon,
            Token::EndIf,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.if_statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: Vec::new(),
            else_body: vec![
                Box::new(Ast::AssignStmt { 
                    dest: Box::new(Ast::Var { 
                        id: String::from("a") 
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 1,
                    }),
                }),
                Box::new(Ast::AssignStmt { 
                    dest: Box::new(Ast::Var { 
                        id: String::from("b") 
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 2,
                    }),
                }),
            ],
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_if_statement_err_missingif() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.if_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing if keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_if_statement_err_missinglparen() {
        let toks = vec![
            Token::If,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.if_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected (, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_if_statement_err_missingrparen() {
        let toks = vec![
            Token::If,
            Token::LParen,
            Token::True,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.if_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected ), found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_if_statement_err_missingthen() {
        let toks = vec![
            Token::If,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.if_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing then keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_if_statement_err_missingthenbodystmtsemicolon() {
        let toks = vec![
            Token::If,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::Then,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.if_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Expected ;") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_if_statement_err_missingelsebodystmtsemicolon() {
        let toks = vec![
            Token::If,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::Then,
            Token::Else,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.if_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Expected ;") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_if_statement_err_missingendif() {
        let toks = vec![
            Token::If,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::Then,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.if_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing end if keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_loop_statement_emptybody() {
        let toks = vec![
            Token::For,
            Token::LParen,
            Token::Identifier(String::from("i")),
            Token::Assign,
            Token::IntLiteral(5),
            Token::Semicolon,
            Token::True,
            Token::RParen,
            Token::EndFor,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.loop_statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::LoopStmt { 
            init: Box::new(Ast::AssignStmt { 
                dest: Box::new(Ast::Var { 
                    id: String::from("i") 
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 5,
                }),
            }),
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            body: Vec::new(),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_loop_statement_singlestmtbody() {
        let toks = vec![
            Token::For,
            Token::LParen,
            Token::Identifier(String::from("i")),
            Token::Assign,
            Token::IntLiteral(5),
            Token::Semicolon,
            Token::True,
            Token::RParen,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Semicolon,
            Token::EndFor,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.loop_statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::LoopStmt { 
            init: Box::new(Ast::AssignStmt { 
                dest: Box::new(Ast::Var { 
                    id: String::from("i") 
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 5,
                }),
            }),
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            body: vec![
                Box::new(Ast::AssignStmt { 
                    dest: Box::new(Ast::Var { 
                        id: String::from("a") 
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 1,
                    }),
                }),
            ],
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_loop_statement_multistmtbody() {
        let toks = vec![
            Token::For,
            Token::LParen,
            Token::Identifier(String::from("i")),
            Token::Assign,
            Token::IntLiteral(5),
            Token::Semicolon,
            Token::True,
            Token::RParen,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Semicolon,
            Token::Identifier(String::from("b")),
            Token::Assign,
            Token::IntLiteral(2),
            Token::Semicolon,
            Token::EndFor,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.loop_statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::LoopStmt { 
            init: Box::new(Ast::AssignStmt { 
                dest: Box::new(Ast::Var { 
                    id: String::from("i") 
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 5,
                }),
            }),
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            body: vec![
                Box::new(Ast::AssignStmt { 
                    dest: Box::new(Ast::Var { 
                        id: String::from("a") 
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 1,
                    }),
                }),
                Box::new(Ast::AssignStmt { 
                    dest: Box::new(Ast::Var { 
                        id: String::from("b") 
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 2,
                    }),
                }),
            ],
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_loop_statement_err_missingfor() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.loop_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing for keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_loop_statement_err_missinglparen() {
        let toks = vec![
            Token::For,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.loop_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected (, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_loop_statement_err_missinginitsemicolon() {
        let toks = vec![
            Token::For,
            Token::LParen,
            Token::Identifier(String::from("i")),
            Token::Assign,
            Token::IntLiteral(5),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.loop_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected ; after loop initialization statement, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_loop_statement_err_missingrparen() {
        let toks = vec![
            Token::For,
            Token::LParen,
            Token::Identifier(String::from("i")),
            Token::Assign,
            Token::IntLiteral(5),
            Token::Semicolon,
            Token::True,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.loop_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected ), found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_loop_statement_err_missingbodystmtsemicolon() {
        let toks = vec![
            Token::For,
            Token::LParen,
            Token::Identifier(String::from("i")),
            Token::Assign,
            Token::IntLiteral(5),
            Token::Semicolon,
            Token::True,
            Token::RParen,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.loop_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Expected ;") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_loop_statement_err_missingendfor() {
        let toks = vec![
            Token::For,
            Token::LParen,
            Token::Identifier(String::from("i")),
            Token::Assign,
            Token::IntLiteral(5),
            Token::Semicolon,
            Token::True,
            Token::RParen,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.loop_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing end for keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_return_statement() {
        let toks = vec![
            Token::Return,
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.return_statement().expect("Parse failed");

        let exp_ast = Box::new(Ast::ReturnStmt { 
            expr: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_return_statement_err_noreturnkeyword() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.return_statement().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing return keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_expr_withoutnot() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.expr().expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_expr_withnot() {
        let toks = vec![
            Token::Not,
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.expr().expect("Parse failed");

        let exp_ast = Box::new(Ast::NotOp { 
            operand: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_expr_prime_null() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.expr_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_expr_prime_singleand() {
        let toks = vec![
            Token::And,
            Token::Identifier(String::from("b")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.expr_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::AndOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_expr_prime_multiand() {
        let toks = vec![
            Token::And,
            Token::Identifier(String::from("b")),
            Token::And,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.expr_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::AndOp {
            lhs: Box::new(Ast::AndOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_expr_prime_singleor() {
        let toks = vec![
            Token::Or,
            Token::Identifier(String::from("b")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.expr_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::OrOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_expr_prime_multior() {
        let toks = vec![
            Token::Or,
            Token::Identifier(String::from("b")),
            Token::Or,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.expr_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::OrOp {
            lhs: Box::new(Ast::OrOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_arith_op() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.arith_op().expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_arith_op_prime_null() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.arith_op_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_arith_op_prime_singleadd() {
        let toks = vec![
            Token::Add,
            Token::Identifier(String::from("b")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.arith_op_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_arith_op_prime_multiadd() {
        let toks = vec![
            Token::Add,
            Token::Identifier(String::from("b")),
            Token::Add,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.arith_op_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::AddOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_arith_op_prime_singlesub() {
        let toks = vec![
            Token::Sub,
            Token::Identifier(String::from("b")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.arith_op_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_arith_op_prime_multisub() {
        let toks = vec![
            Token::Sub,
            Token::Identifier(String::from("b")),
            Token::Sub,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.arith_op_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::SubOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.relation().expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_null() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_singlelt() {
        let toks = vec![
            Token::LT,
            Token::Identifier(String::from("b")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::LT,
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_multilt() {
        let toks = vec![
            Token::LT,
            Token::Identifier(String::from("b")),
            Token::LT,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::LT,
            lhs: Box::new(Ast::Relation {
                op: RelationOp::LT,
                lhs: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_singlelte() {
        let toks = vec![
            Token::LTE,
            Token::Identifier(String::from("b")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::LTE,
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_multilte() {
        let toks = vec![
            Token::LTE,
            Token::Identifier(String::from("b")),
            Token::LTE,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::LTE,
            lhs: Box::new(Ast::Relation {
                op: RelationOp::LTE,
                lhs: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_singlegt() {
        let toks = vec![
            Token::GT,
            Token::Identifier(String::from("b")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::GT,
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_multigt() {
        let toks = vec![
            Token::GT,
            Token::Identifier(String::from("b")),
            Token::GT,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::GT,
            lhs: Box::new(Ast::Relation {
                op: RelationOp::GT,
                lhs: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_singlegte() {
        let toks = vec![
            Token::GTE,
            Token::Identifier(String::from("b")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::GTE,
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_multigte() {
        let toks = vec![
            Token::GTE,
            Token::Identifier(String::from("b")),
            Token::GTE,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::GTE,
            lhs: Box::new(Ast::Relation {
                op: RelationOp::GTE,
                lhs: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_singleeq() {
        let toks = vec![
            Token::Eq,
            Token::Identifier(String::from("b")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::Eq,
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_multieq() {
        let toks = vec![
            Token::Eq,
            Token::Identifier(String::from("b")),
            Token::Eq,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::Eq,
            lhs: Box::new(Ast::Relation {
                op: RelationOp::Eq,
                lhs: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_singlenoteq() {
        let toks = vec![
            Token::NotEq,
            Token::Identifier(String::from("b")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::NotEq,
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_relation_prime_multinoteq() {
        let toks = vec![
            Token::NotEq,
            Token::Identifier(String::from("b")),
            Token::NotEq,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::NotEq,
            lhs: Box::new(Ast::Relation {
                op: RelationOp::NotEq,
                lhs: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_term() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.term().expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_term_prime_null() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.term_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_term_prime_singlemul() {
        let toks = vec![
            Token::Mul,
            Token::Identifier(String::from("b")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.term_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_term_prime_multimul() {
        let toks = vec![
            Token::Mul,
            Token::Identifier(String::from("b")),
            Token::Mul,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.term_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::MulOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_term_prime_singlediv() {
        let toks = vec![
            Token::Div,
            Token::Identifier(String::from("b")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.term_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_term_prime_multidiv() {
        let toks = vec![
            Token::Div,
            Token::Identifier(String::from("b")),
            Token::Div,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        let act_ast = p.term_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::DivOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_factor_name() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_factor_proc_call() {
        let toks = vec![
            Token::Identifier(String::from("foo")),
            Token::LParen,
            Token::RParen,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");

        let exp_ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo") 
            }),
            args: Vec::new(),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_factor_neg_name() {
        let toks = vec![
            Token::Sub,
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");

        let exp_ast = Box::new(Ast::NegateOp {
            operand: Box::new(Ast::Var { 
                id: String::from("a")
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_factor_neg_intliteral() {
        let toks = vec![
            Token::Sub,
            Token::IntLiteral(5),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");

        let exp_ast = Box::new(Ast::NegateOp {
            operand: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_factor_neg_floatliteral() {
        let toks = vec![
            Token::Sub,
            Token::FloatLiteral(5.0),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");

        let exp_ast = Box::new(Ast::NegateOp {
            operand: Box::new(Ast::FloatLiteral { 
                value: 5.0,
            }),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_factor_intliteral() {
        let toks = vec![
            Token::IntLiteral(5),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");

        let exp_ast = Box::new(Ast::IntLiteral { 
            value: 5,
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_factor_floatliteral() {
        let toks = vec![
            Token::FloatLiteral(5.0),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");

        let exp_ast = Box::new(Ast::FloatLiteral { 
            value: 5.0,
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_factor_stringliteral() {
        let toks = vec![
            Token::String(String::from("test")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");

        let exp_ast = Box::new(Ast::StringLiteral { 
            value: String::from("test"),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_factor_true() {
        let toks = vec![
            Token::True,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");

        let exp_ast = Box::new(Ast::BoolLiteral { 
            value: true,
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_factor_false() {
        let toks = vec![
            Token::False,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");

        let exp_ast = Box::new(Ast::BoolLiteral { 
            value: false,
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_factor_parenthesizedexpr() {
        let toks = vec![
            Token::LParen,
            Token::Identifier(String::from("a")),
            Token::RParen,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a") 
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_factor_err_invalidnegation() {
        let toks = vec![
            Token::Sub,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.factor().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected identifier or numeric literal following -, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_factor_err_missingrparen() {
        let toks = vec![
            Token::LParen,
            Token::Identifier(String::from("a")),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.factor().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected ), found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_factor_err_invalidfirstterminal() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.factor().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected one of: (, -, numeric literal, string literal, boolean literal, identifier, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_call_prime_noargs() {
        let toks = vec![
            Token::LParen,
            Token::RParen,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("foo") 
        });

        let act_ast = p.procedure_call_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo") 
            }),
            args: Vec::new(),
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_procedure_call_prime_withargs() {
        let toks = vec![
            Token::LParen,
            Token::Identifier(String::from("a")),
            Token::Comma,
            Token::Identifier(String::from("b")),
            Token::RParen,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("foo") 
        });

        let act_ast = p.procedure_call_prime(in_ast).expect("Parse failed");

        let exp_ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo") 
            }),
            args: vec![
                Box::new(Ast::Var { 
                    id: String::from("a") 
                }),
                Box::new(Ast::Var { 
                    id: String::from("b") 
                }),
            ],
        });

        assert_eq!(act_ast, exp_ast);
    }

    #[test]
    fn llparse_procedure_call_err_missinglparen() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("foo") 
        });

        p.procedure_call_prime(in_ast).expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected (, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_call_err_missingrparen() {
        let toks = vec![
            Token::LParen,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("foo") 
        });

        p.procedure_call_prime(in_ast).expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected ), found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_argument_list_singleexpr() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_vec_ast = p.argument_list().expect("Parse failed");

        let exp_vec_ast = vec![
            Box::new(Ast::Var { 
                id: String::from("a") 
            })
        ];

        assert_eq!(act_vec_ast, exp_vec_ast);
    }

    #[test]
    fn llparse_argument_list_multiexpr() {
        let toks = vec![
            Token::Identifier(String::from("a")),
            Token::Comma,
            Token::Identifier(String::from("b")),
            Token::Comma,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_vec_ast = p.argument_list().expect("Parse failed");

        let exp_vec_ast = vec![
            Box::new(Ast::Var { 
                id: String::from("a") 
            }),
            Box::new(Ast::Var { 
                id: String::from("b") 
            }),
            Box::new(Ast::Var { 
                id: String::from("c") 
            }),
        ];

        assert_eq!(act_vec_ast, exp_vec_ast);
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

    //TODO: maybe, mixed operator associativity tests
    //TODO: expr tests for operator precedence
}
