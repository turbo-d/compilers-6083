use crate::ast::{Ast, RelationOp};
use crate::error::{CompilerError, TerminalError};
use crate::scanner::Scan;
use crate::token::Token;
use crate::types::Types;

use std::vec::Vec;

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
            self.errs.push(CompilerError::Warning { line: self.s.line(), msg: String::from("Missing . after end program keyword") });
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
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing program keyword") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let (identifier, _) = self.identifier()?;

        if self.tok != Token::Is {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing is keyword") });
            return Err(TerminalError);
        }
        self.consume_tok();

        Ok(identifier)
    }

    // first(program_body): "global", "procedure", "variable", "begin"
    fn program_body(&mut self) -> Result<(Vec<Box<Ast>>, Vec<Box<Ast>>), TerminalError> {
        let mut decls = Vec::new();
        while self.tok == Token::Global || self.tok == Token::Procedure || self.tok == Token::Variable {
            decls.push(self.declaration()?);

            if self.tok != Token::Semicolon {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected ; after declaration") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        if self.tok != Token::Begin {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing begin keyword") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut stmts = Vec::new();
        while matches!(self.tok, Token::Identifier(_)) || matches!(self.tok, Token::Invalid(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            stmts.push(self.statement()?);

            if self.tok != Token::Semicolon {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected ;") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        if self.tok != Token::EndProgram {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing end program keyword") });
            return Err(TerminalError);
        }
        self.consume_tok();

        Ok((decls, stmts))
    }

    // first(declaration): "global", "procedure", "variable"
    fn declaration(&mut self) -> Result<Box<Ast>, TerminalError> {
        let is_global =
            if self.tok == Token::Global {
                self.consume_tok();
                true
            } else {
                false
            };

        match &self.tok {
            Token::Procedure => self.procedure_declaration(is_global),
            Token::Variable => {
                let (_, var_decl_node) = self.variable_declaration(is_global)?;
                Ok(var_decl_node)
            },
            _ => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected declaration") });
                Err(TerminalError)
            },
        }
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

    fn procedure_header(&mut self) -> Result<(String, Types, Vec<Box<Ast>>), TerminalError> {
        if self.tok != Token::Procedure {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing procedure keyword") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let (identifier, _) = self.identifier()?;

        if self.tok != Token::Colon {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected :, found {}", self.tok) });
            return Err(TerminalError);
        }
        self.consume_tok();

        let return_type = self.type_mark()?;

        if self.tok != Token::LParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected (, found {}", self.tok) });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut params = Vec::new();
        if self.tok == Token::Variable {
            params = self.parameter_list()?;
        }
        let (param_types, param_nodes): (Vec<_>, Vec<_>) = params.into_iter().unzip();

        if self.tok != Token::RParen {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected ), found {}", self.tok) });
            return Err(TerminalError);
        }
        self.consume_tok();

        let parsed_type = Types::Proc(Box::new(return_type), param_types);

        Ok((identifier, parsed_type, param_nodes))
    }

    fn type_mark(&mut self) -> Result<Types, TerminalError> {
        match &self.tok {
            Token::IntType => {
                self.consume_tok();
                Ok(Types::Int)
            },
            Token::FloatType => {
                self.consume_tok();
                Ok(Types::Float)
            },
            Token::StringType => {
                self.consume_tok();
                Ok(Types::String)
            },
            Token::BoolType => {
                self.consume_tok();
                Ok(Types::Bool)
            },
            tok => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected type (integer, float, string, bool), found {tok}") });
                Err(TerminalError)
            },
        }
    }

    // first(parameter_list): "variable"
    fn parameter_list(&mut self) -> Result<Vec<(Types, Box<Ast>)>, TerminalError> {
        let mut params = Vec::new();

        params.push(self.parameter()?);

        while self.tok == Token::Comma {
            self.consume_tok();

            params.push(self.parameter()?);
        }

        Ok(params)
    }

    fn parameter(&mut self) -> Result<(Types, Box<Ast>), TerminalError> {
        self.variable_declaration(false)
    }

    // first(procedure_body): "global", "procedure", "variable", "begin"
    fn procedure_body(&mut self) -> Result<(Vec<Box<Ast>>, Vec<Box<Ast>>), TerminalError> {
        let mut decls = Vec::new();
        while self.tok == Token::Global || self.tok == Token::Procedure || self.tok == Token::Variable {
            decls.push(self.declaration()?);

            if self.tok != Token::Semicolon {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected ; after declaration") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        if self.tok != Token::Begin {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing begin keyword") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut stmts = Vec::new();
        while matches!(self.tok, Token::Identifier(_)) || matches!(self.tok, Token::Invalid(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
            stmts.push(self.statement()?);

            if self.tok != Token::Semicolon {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected ;") });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        if self.tok != Token::EndProcedure {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing end procedure keyword") });
            return Err(TerminalError);
        }
        self.consume_tok();

        Ok((decls, stmts))
    }

    fn variable_declaration(&mut self, is_global: bool) -> Result<(Types, Box<Ast>), TerminalError> {
        if self.tok != Token::Variable {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Missing variable keyword") });
            return Err(TerminalError);
        }
        self.consume_tok();

        let (identifier, _) = self.identifier()?;

        if self.tok != Token::Colon {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected :, found {}", self.tok) });
            return Err(TerminalError);
        }
        self.consume_tok();

        let mut parsed_type = self.type_mark()?;

        if self.tok == Token::LSquare {
            // consume LSquare
            self.consume_tok();

            parsed_type = match &self.tok {
                Token::IntLiteral(bound) => Types::Array(*bound, Box::new(parsed_type)),
                tok => {
                    self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected integer literal, found {tok}") });
                    return Err(TerminalError);
                },
            };
            self.consume_tok();

            if self.tok != Token::RSquare {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected ], found {}", self.tok) });
                return Err(TerminalError);
            }
            self.consume_tok();
        }

        let var_decl_node = Box::new(Ast::VarDecl {
            is_global: is_global,
            name: identifier,
            ty: parsed_type.clone(),
        });

        Ok((parsed_type, var_decl_node))
    }

    // first(statement): "identifier", "if", "for", "return"
    fn statement(&mut self) -> Result<Box<Ast>, TerminalError> {
        match &self.tok {
            Token::Identifier(_) | Token::Invalid(_) => Ok(self.assignment_statement()?),
            Token::If => Ok(self.if_statement()?),
            Token::For => Ok(self.loop_statement()?),
            Token::Return => Ok(self.return_statement()?),
            tok => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected statement (assignment, if, for, or return), found {tok}") });
                Err(TerminalError)
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
        let (var_id, var_line) = self.identifier()?;

        let var_node = Box::new(Ast::Var {
            id: var_id,
            line: var_line,
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
        while matches!(self.tok, Token::Identifier(_)) || matches!(self.tok, Token::Invalid(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
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

            while matches!(self.tok, Token::Identifier(_)) || matches!(self.tok, Token::Invalid(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
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
        while matches!(self.tok, Token::Identifier(_)) || matches!(self.tok, Token::Invalid(_)) || self.tok == Token::If || self.tok == Token::For || self.tok == Token::Return {
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
            Token::Identifier(_) | Token::Invalid(_) => {
                let (id, id_line) = self.identifier()?;

                let var = Box::new(Ast::Var {
                    id: id,
                    line: id_line,
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
                    Token::Identifier(_) | Token::Invalid(_) => self.name()?,
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
                Err(TerminalError)
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
            matches!(self.tok, Token::Invalid(_)) || 
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
        let (var_id, var_line) = self.identifier()?;

        let var = Box::new(Ast::Var {
            id: var_id,
            line: var_line,
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

    fn identifier(&mut self) -> Result<(String, u32), TerminalError> {
        let mut ident = String::new();
        let mut has_invalid_chars = false;
        let mut num_idents = 0;
        match &self.tok {
            Token::Identifier(id) => {
                num_idents += 1;
                ident.push_str(id);
            },
            Token::Invalid(val) => {
                has_invalid_chars = true;
                if val == "_" {
                    self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Identifiers cannot begin with _") });
                }
                ident.push_str(val);
            },
            tok => {
                self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected identifier, found {tok}") });
                return Err(TerminalError);
            },
        }
        let line = self.s.line();
        self.consume_tok();

        while matches!(self.tok, Token::Identifier(_)) || matches!(self.tok, Token::Invalid(_)) {
            if let Token::Identifier(id) = &self.tok {
                num_idents += 1;
                ident.push_str(id);
            } else if let Token::Invalid(val) = &self.tok {
                has_invalid_chars = true;
                ident.push_str(val);
            }
            self.consume_tok();
        }

        if num_idents == 0 {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: String::from("Expected identifier, found invalid chars") });
            return Err(TerminalError);
        }

        if has_invalid_chars {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Identifier {ident} contains invalid chars") });
        } else if num_idents > 1 {
            self.errs.push(CompilerError::Error { line: self.s.line(), msg: format!("Expected identifier, found {num_idents} identifiers") });
        }

        Ok((ident, line))
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
    fn llparse_parse() {
        let toks = vec![
            Token::Program,
            Token::Identifier(String::from("test_prgm")),
            Token::Is,
            Token::Begin,
            Token::EndProgram,
            Token::Period,
            Token::EOF,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.parse().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Program {
            name: String::from("test_prgm"),
            decls: Vec::new(),
            body: Vec::new(),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_parse_warn_extraneoustrailingchars() {
        let toks = vec![
            Token::Program,
            Token::Identifier(String::from("test_prgm")),
            Token::Is,
            Token::Begin,
            Token::EndProgram,
            Token::Period,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.parse().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Program {
            name: String::from("test_prgm"),
            decls: Vec::new(),
            body: Vec::new(),
        });
        let exp_errs =  &vec![
            CompilerError::Warning { line: 1, msg: String::from("Extraneous trailing characters") }
        ];

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program() {
        let toks = vec![
            Token::Program,
            Token::Identifier(String::from("test_prgm")),
            Token::Is,
            Token::Begin,
            Token::EndProgram,
            Token::Period,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.program().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Program {
            name: String::from("test_prgm"),
            decls: Vec::new(),
            body: Vec::new(),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_warn_noperiod() {
        let toks = vec![
            Token::Program,
            Token::Identifier(String::from("test_prgm")),
            Token::Is,
            Token::Begin,
            Token::EndProgram,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.program().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Program {
            name: String::from("test_prgm"),
            decls: Vec::new(),
            body: Vec::new(),
        });
        let exp_errs =  &vec![
            CompilerError::Warning { line: 1, msg: String::from("Missing . after end program keyword") }
        ];

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_header() {
        let toks = vec![
            Token::Program,
            Token::Identifier(String::from("test_prgm")),
            Token::Is,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_name = p.program_header().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_errs = &Vec::new();

        assert_eq!(act_name, String::from("test_prgm"));
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_header_terminalerr_missingprogram() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.program_header().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing program keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_header_terminalerr_missingis() {
        let toks = vec![
            Token::Program,
            Token::Identifier(String::from("test_prgm")),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.program_header().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing is keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_body_nodecl_nostmt() {
        let toks = vec![
            Token::Begin,
            Token::EndProgram,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_decls, act_stmts) = p.program_body().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_decls = Vec::new();
        let exp_stmts = Vec::new();
        let exp_errs = &Vec::new();

        assert_eq!(act_decls, exp_decls);
        assert_eq!(act_stmts, exp_stmts);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_body_singledecl_nostmts() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::Semicolon,
            Token::Begin,
            Token::EndProgram,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_decls, act_stmts) = p.program_body().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_decls = vec![
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("a"),
                ty: Types::Int,
            }),
        ];
        let exp_stmts = Vec::new();
        let exp_errs = &Vec::new();

        assert_eq!(act_decls, exp_decls);
        assert_eq!(act_stmts, exp_stmts);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_body_multidecl_nostmts() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::Semicolon,
            Token::Variable,
            Token::Identifier(String::from("b")),
            Token::Colon,
            Token::IntType,
            Token::Semicolon,
            Token::Begin,
            Token::EndProgram,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_decls, act_stmts) = p.program_body().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_decls = vec![
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("a"),
                ty: Types::Int,
            }),
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("b"),
                ty: Types::Int,
            }),
        ];
        let exp_stmts = Vec::new();
        let exp_errs = &Vec::new();

        assert_eq!(act_decls, exp_decls);
        assert_eq!(act_stmts, exp_stmts);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_body_nodecl_singlestmt() {
        let toks = vec![
            Token::Begin,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Semicolon,
            Token::EndProgram,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_decls, act_stmts) = p.program_body().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_decls = Vec::new();
        let exp_stmts = vec![
            Box::new(Ast::AssignStmt { 
                dest: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 1,
                }),
            }),
        ];
        let exp_errs = &Vec::new();

        assert_eq!(act_decls, exp_decls);
        assert_eq!(act_stmts, exp_stmts);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_body_nodecl_multistmt() {
        let toks = vec![
            Token::Begin,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Semicolon,
            Token::Identifier(String::from("b")),
            Token::Assign,
            Token::IntLiteral(2),
            Token::Semicolon,
            Token::EndProgram,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_decls, act_stmts) = p.program_body().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_decls = Vec::new();
        let exp_stmts = vec![
            Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("a"),
                    line: 1,
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 1,
                }),
            }),
            Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("b"),
                    line: 1,
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 2,
                }),
            }),
        ];
        let exp_errs = &Vec::new();

        assert_eq!(act_decls, exp_decls);
        assert_eq!(act_stmts, exp_stmts);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_body_multidecl_multistmt() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::Semicolon,
            Token::Variable,
            Token::Identifier(String::from("b")),
            Token::Colon,
            Token::IntType,
            Token::Semicolon,
            Token::Begin,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Semicolon,
            Token::Identifier(String::from("b")),
            Token::Assign,
            Token::IntLiteral(2),
            Token::Semicolon,
            Token::EndProgram,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_decls, act_stmts) = p.program_body().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_decls = vec![
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("a"),
                ty: Types::Int,
            }),
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("b"),
                ty: Types::Int,
            }),
        ];
        let exp_stmts = vec![
            Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("a"),
                    line: 1,
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 1,
                }),
            }),
            Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("b"),
                    line: 1,
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 2,
                }),
            }),
        ];
        let exp_errs = &Vec::new();

        assert_eq!(act_decls, exp_decls);
        assert_eq!(act_stmts, exp_stmts);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_body_terminalerr_missingdeclsemicolon() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.program_body().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Expected ; after declaration") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_body_terminalerr_missingbegin() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.program_body().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing begin keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_body_terminalerr_missingstmtsemicolon() {
        let toks = vec![
            Token::Begin,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.program_body().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Expected ;") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_program_body_terminalerr_missingendprogram() {
        let toks = vec![
            Token::Begin,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.program_body().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing end program keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_declaration_local_procedure() {
        let toks = vec![
            Token::Procedure,
            Token::Identifier(String::from("foo")),
            Token::Colon,
            Token::IntType,
            Token::LParen,
            Token::RParen,
            Token::Begin,
            Token::EndProcedure,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.declaration().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::ProcDecl {
            is_global: false,
            name: String::from("foo"),
            ty: Types::Proc(Box::new(Types::Int), Vec::new()),
            params: Vec::new(),
            decls: Vec::new(),
            body: Vec::new(),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_declaration_global_procedure() {
        let toks = vec![
            Token::Global,
            Token::Procedure,
            Token::Identifier(String::from("foo")),
            Token::Colon,
            Token::IntType,
            Token::LParen,
            Token::RParen,
            Token::Begin,
            Token::EndProcedure,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.declaration().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::ProcDecl {
            is_global: true,
            name: String::from("foo"),
            ty: Types::Proc(Box::new(Types::Int), Vec::new()),
            params: Vec::new(),
            decls: Vec::new(),
            body: Vec::new(),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_declaration_local_variable() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.declaration().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::VarDecl { 
            is_global: false,
            name: String::from("a"),
            ty: Types::Int,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_declaration_global_variable() {
        let toks = vec![
            Token::Global,
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.declaration().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::VarDecl { 
            is_global: true,
            name: String::from("a"),
            ty: Types::Int,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_declaration_terminalerr_nodecl() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.declaration().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Expected declaration") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_declaration() {
        let toks = vec![
            Token::Procedure,
            Token::Identifier(String::from("foo")),
            Token::Colon,
            Token::IntType,
            Token::LParen,
            Token::RParen,
            Token::Begin,
            Token::EndProcedure,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.procedure_declaration(false).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::ProcDecl {
            is_global: false,
            name: String::from("foo"),
            ty: Types::Proc(Box::new(Types::Int), Vec::new()),
            params: Vec::new(),
            decls: Vec::new(),
            body: Vec::new(),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_header_noparams() {
        let toks = vec![
            Token::Procedure,
            Token::Identifier(String::from("foo")),
            Token::Colon,
            Token::IntType,
            Token::LParen,
            Token::RParen,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_name, act_ty, act_params) = p.procedure_header().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_name = String::from("foo");
        let exp_ty = Types::Proc(Box::new(Types::Int), vec![]);
        let exp_params = vec![];
        let exp_errs = &Vec::new();

        assert_eq!(act_name, exp_name);
        assert_eq!(act_ty, exp_ty);
        assert_eq!(act_params, exp_params);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_header_withparams() {
        let toks = vec![
            Token::Procedure,
            Token::Identifier(String::from("foo")),
            Token::Colon,
            Token::IntType,
            Token::LParen,
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::Comma,
            Token::Variable,
            Token::Identifier(String::from("b")),
            Token::Colon,
            Token::IntType,
            Token::RParen,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_name, act_ty, act_params) = p.procedure_header().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_name = String::from("foo");
        let exp_ty = Types::Proc(
            Box::new(Types::Int),
            vec![
                Types::Int,
                Types::Int,
            ],
        );
        let exp_params = vec![
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("a"),
                ty: Types::Int,
            }),
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("b"),
                ty: Types::Int,
            }),
        ];
        let exp_errs = &Vec::new();

        assert_eq!(act_name, exp_name);
        assert_eq!(act_ty, exp_ty);
        assert_eq!(act_params, exp_params);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_header_terminalerr_missingprocedure() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.procedure_header().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing procedure keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_header_terminalerr_missingcolon() {
        let toks = vec![
            Token::Procedure,
            Token::Identifier(String::from("foo")),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.procedure_header().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected :, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_header_terminalerr_missinglparen() {
        let toks = vec![
            Token::Procedure,
            Token::Identifier(String::from("foo")),
            Token::Colon,
            Token::IntType,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.procedure_header().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected (, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_header_terminalerr_missingrparen() {
        let toks = vec![
            Token::Procedure,
            Token::Identifier(String::from("foo")),
            Token::Colon,
            Token::IntType,
            Token::LParen,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.procedure_header().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected ), found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_type_mark_int() {
        let toks = vec![
            Token::IntType,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_type = p.type_mark().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_errs = &Vec::new();

        assert_eq!(act_type, Types::Int);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_type_mark_float() {
        let toks = vec![
            Token::FloatType,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_type = p.type_mark().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_errs = &Vec::new();

        assert_eq!(act_type, Types::Float);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_type_mark_string() {
        let toks = vec![
            Token::StringType,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_type = p.type_mark().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_errs = &Vec::new();

        assert_eq!(act_type, Types::String);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_type_mark_bool() {
        let toks = vec![
            Token::BoolType,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_type = p.type_mark().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_errs = &Vec::new();

        assert_eq!(act_type, Types::Bool);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_type_mark_terminalerr_invalidtype() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.type_mark().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected type (integer, float, string, bool), found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_parameter_list_singleparam() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_params = p.parameter_list().expect("Parse failed");
        let (act_vec_type, act_vec_ast): (Vec<_>, Vec<_>) = act_params.into_iter().unzip();
        let act_errs = p.get_errors();

        let exp_vec_type = vec![
            Types::Int,
        ];
        let exp_vec_ast = vec![
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("a"),
                ty: Types::Int,
            }),
        ];
        let exp_errs = &Vec::new();

        assert_eq!(act_vec_type, exp_vec_type);
        assert_eq!(act_vec_ast, exp_vec_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_parameter_list_multiparam() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::Comma,
            Token::Variable,
            Token::Identifier(String::from("b")),
            Token::Colon,
            Token::IntType,
            Token::Comma,
            Token::Variable,
            Token::Identifier(String::from("c")),
            Token::Colon,
            Token::IntType,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_params = p.parameter_list().expect("Parse failed");
        let (act_vec_type, act_vec_ast): (Vec<_>, Vec<_>) = act_params.into_iter().unzip();
        let act_errs = p.get_errors();

        let exp_vec_type = vec![
            Types::Int,
            Types::Int,
            Types::Int,
        ];
        let exp_vec_ast = vec![
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("a"),
                ty: Types::Int,
            }),
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("b"),
                ty: Types::Int,
            }),
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("c"),
                ty: Types::Int,
            }),
        ];
        let exp_errs = &Vec::new();

        assert_eq!(act_vec_type, exp_vec_type);
        assert_eq!(act_vec_ast, exp_vec_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_body_nodecl_nostmt() {
        let toks = vec![
            Token::Begin,
            Token::EndProcedure,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_decls, act_stmts) = p.procedure_body().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_decls = Vec::new();
        let exp_stmts = Vec::new();
        let exp_errs = &Vec::new();

        assert_eq!(act_decls, exp_decls);
        assert_eq!(act_stmts, exp_stmts);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_body_singledecl_nostmts() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::Semicolon,
            Token::Begin,
            Token::EndProcedure,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_decls, act_stmts) = p.procedure_body().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_decls = vec![
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("a"),
                ty: Types::Int,
            }),
        ];
        let exp_stmts = Vec::new();
        let exp_errs = &Vec::new();

        assert_eq!(act_decls, exp_decls);
        assert_eq!(act_stmts, exp_stmts);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_body_multidecl_nostmts() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::Semicolon,
            Token::Variable,
            Token::Identifier(String::from("b")),
            Token::Colon,
            Token::IntType,
            Token::Semicolon,
            Token::Begin,
            Token::EndProcedure,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_decls, act_stmts) = p.procedure_body().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_decls = vec![
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("a"),
                ty: Types::Int,
            }),
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("b"),
                ty: Types::Int,
            }),
        ];
        let exp_stmts = Vec::new();
        let exp_errs = &Vec::new();

        assert_eq!(act_decls, exp_decls);
        assert_eq!(act_stmts, exp_stmts);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_body_nodecl_singlestmt() {
        let toks = vec![
            Token::Begin,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Semicolon,
            Token::EndProcedure,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_decls, act_stmts) = p.procedure_body().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_decls = Vec::new();
        let exp_stmts = vec![
            Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("a"),
                    line: 1,
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 1,
                }),
            }),
        ];
        let exp_errs = &Vec::new();

        assert_eq!(act_decls, exp_decls);
        assert_eq!(act_stmts, exp_stmts);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_body_nodecl_multistmt() {
        let toks = vec![
            Token::Begin,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Semicolon,
            Token::Identifier(String::from("b")),
            Token::Assign,
            Token::IntLiteral(2),
            Token::Semicolon,
            Token::EndProcedure,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_decls, act_stmts) = p.procedure_body().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_decls = Vec::new();
        let exp_stmts = vec![
            Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("a"),
                    line: 1,
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 1,
                }),
            }),
            Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("b"),
                    line: 1,
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 2,
                }),
            }),
        ];
        let exp_errs = &Vec::new();

        assert_eq!(act_decls, exp_decls);
        assert_eq!(act_stmts, exp_stmts);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_body_multidecl_multistmt() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::Semicolon,
            Token::Variable,
            Token::Identifier(String::from("b")),
            Token::Colon,
            Token::IntType,
            Token::Semicolon,
            Token::Begin,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Semicolon,
            Token::Identifier(String::from("b")),
            Token::Assign,
            Token::IntLiteral(2),
            Token::Semicolon,
            Token::EndProcedure,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_decls, act_stmts) = p.procedure_body().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_decls = vec![
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("a"),
                ty: Types::Int,
            }),
            Box::new(Ast::VarDecl { 
                is_global: false,
                name: String::from("b"),
                ty: Types::Int,
            }),
        ];
        let exp_stmts = vec![
            Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("a"),
                    line: 1,
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 1,
                }),
            }),
            Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("b"),
                    line: 1,
                }),
                expr: Box::new(Ast::IntLiteral { 
                    value: 2,
                }),
            }),
        ];
        let exp_errs = &Vec::new();

        assert_eq!(act_decls, exp_decls);
        assert_eq!(act_stmts, exp_stmts);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_body_terminalerr_missingdeclsemicolon() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.procedure_body().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Expected ; after declaration") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_body_terminalerr_missingbegin() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.procedure_body().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing begin keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_body_terminalerr_missingstmtsemicolon() {
        let toks = vec![
            Token::Begin,
            Token::Identifier(String::from("a")),
            Token::Assign,
            Token::IntLiteral(1),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.procedure_body().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Expected ;") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_body_terminalerr_missingendprocedure() {
        let toks = vec![
            Token::Begin,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.procedure_body().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing end procedure keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_variable_declaration_scalar() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_ty, act_ast) = p.variable_declaration(false).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ty = Types::Int;
        let exp_ast = Box::new(Ast::VarDecl { 
            is_global: false,
            name: String::from("a"),
            ty: Types::Int,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ty, exp_ty);
        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_variable_declaration_array() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::LSquare,
            Token::IntLiteral(5),
            Token::RSquare,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_ty, act_ast) = p.variable_declaration(false).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ty = Types::Array(5, Box::new(Types::Int));
        let exp_ast = Box::new(Ast::VarDecl { 
            is_global: false,
            name: String::from("a"),
            ty: Types::Array(5, Box::new(Types::Int)),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ty, exp_ty);
        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_variable_declaration_terminalerr_missingvariable() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.variable_declaration(false).expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Missing variable keyword") }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_variable_declaration_terminalerr_missingcolon() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.variable_declaration(false).expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected :, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_variable_declaration_terminalerr_invalidarraybound() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::LSquare,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.variable_declaration(false).expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected integer literal, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_variable_declaration_terminalerr_missingrsquare() {
        let toks = vec![
            Token::Variable,
            Token::Identifier(String::from("a")),
            Token::Colon,
            Token::IntType,
            Token::LSquare,
            Token::IntLiteral(5),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.variable_declaration(false).expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected ], found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::AssignStmt { 
            dest: Box::new(Ast::Var {
                id: String::from("a"),
                line: 1,
            }),
            expr: Box::new(Ast::IntLiteral { 
                value: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: Vec::new(),
            else_body: Vec::new(),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::LoopStmt {
            init: Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("i"),
                    line: 1,
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
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::ReturnStmt {
            expr: Box::new(Ast::Var {
                id: String::from("a"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_statement_terminalerr_invalidstatement() {
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::AssignStmt {
            dest: Box::new(Ast::Var {
                id: String::from("a"),
                line: 1,
            }),
            expr: Box::new(Ast::IntLiteral {
                value: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_assignment_statement_terminalerr_missingassignmentoperator() {
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var {
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::SubscriptOp {
            array: Box::new(Ast::Var {
                id: String::from("a"),
                line: 1,
            }),
            index: Box::new(Ast::IntLiteral {
                value: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_destination_terminalerr_missingrsquare() {
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: Vec::new(),
            else_body: Vec::new(),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: vec![
                Box::new(Ast::AssignStmt {
                    dest: Box::new(Ast::Var {
                        id: String::from("a"),
                        line: 1,
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 1,
                    }),
                }),
            ],
            else_body: Vec::new(),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: vec![
                Box::new(Ast::AssignStmt {
                    dest: Box::new(Ast::Var {
                        id: String::from("a"),
                        line: 1,
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 1,
                    }),
                }),
                Box::new(Ast::AssignStmt {
                    dest: Box::new(Ast::Var {
                        id: String::from("b"),
                        line: 1,
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 2,
                    }),
                }),
            ],
            else_body: Vec::new(),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: Vec::new(),
            else_body: Vec::new(),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: Vec::new(),
            else_body: vec![
                Box::new(Ast::AssignStmt {
                    dest: Box::new(Ast::Var {
                        id: String::from("a"),
                        line: 1,
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 1,
                    }),
                }),
            ],
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::IfStmt { 
            cond: Box::new(Ast::BoolLiteral { 
                value: true,
            }),
            then_body: Vec::new(),
            else_body: vec![
                Box::new(Ast::AssignStmt {
                    dest: Box::new(Ast::Var {
                        id: String::from("a"),
                        line: 1,
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 1,
                    }),
                }),
                Box::new(Ast::AssignStmt {
                    dest: Box::new(Ast::Var {
                        id: String::from("b"),
                        line: 1,
                    }),
                    expr: Box::new(Ast::IntLiteral { 
                        value: 2,
                    }),
                }),
            ],
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_if_statement_terminalerr_missingif() {
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
    fn llparse_if_statement_terminalerr_missinglparen() {
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
    fn llparse_if_statement_terminalerr_missingrparen() {
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
    fn llparse_if_statement_terminalerr_missingthen() {
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
    fn llparse_if_statement_terminalerr_missingthenbodystmtsemicolon() {
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
    fn llparse_if_statement_terminalerr_missingelsebodystmtsemicolon() {
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
    fn llparse_if_statement_terminalerr_missingendif() {
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::LoopStmt {
            init: Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("i"),
                    line: 1,
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
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::LoopStmt {
            init: Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("i"),
                    line: 1,
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
                        id: String::from("a"),
                        line: 1,
                    }),
                    expr: Box::new(Ast::IntLiteral {
                        value: 1,
                    }),
                }),
            ],
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::LoopStmt {
            init: Box::new(Ast::AssignStmt {
                dest: Box::new(Ast::Var {
                    id: String::from("i"),
                    line: 1,
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
                        id: String::from("a"),
                        line: 1,
                    }),
                    expr: Box::new(Ast::IntLiteral {
                        value: 1,
                    }),
                }),
                Box::new(Ast::AssignStmt {
                    dest: Box::new(Ast::Var {
                        id: String::from("b"),
                        line: 1,
                    }),
                    expr: Box::new(Ast::IntLiteral {
                        value: 2,
                    }),
                }),
            ],
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_loop_statement_terminalerr_missingfor() {
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
    fn llparse_loop_statement_terminalerr_missinglparen() {
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
    fn llparse_loop_statement_terminalerr_missinginitsemicolon() {
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
    fn llparse_loop_statement_terminalerr_missingrparen() {
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
    fn llparse_loop_statement_terminalerr_missingbodystmtsemicolon() {
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
    fn llparse_loop_statement_terminalerr_missingendfor() {
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::ReturnStmt {
            expr: Box::new(Ast::Var {
                id: String::from("a"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_return_statement_terminalerr_noreturnkeyword() {
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var {
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::NotOp {
            operand: Box::new(Ast::Var {
                id: String::from("a"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_expr_prime_null() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var {
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.expr_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var {
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.expr_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::AndOp {
            lhs: Box::new(Ast::Var {
                id: String::from("a"),
                line: 1,
            }),
            rhs: Box::new(Ast::Var {
                id: String::from("b"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.expr_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::AndOp {
            lhs: Box::new(Ast::AndOp {
                lhs: Box::new(Ast::Var {
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var {
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var {
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.expr_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::OrOp {
            lhs: Box::new(Ast::Var {
                id: String::from("a"),
                line: 1,
            }),
            rhs: Box::new(Ast::Var {
                id: String::from("b"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.expr_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::OrOp {
            lhs: Box::new(Ast::OrOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_expr_prime_andor() {
        let toks = vec![
            Token::And,
            Token::Identifier(String::from("b")),
            Token::Or,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.expr_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::OrOp {
            lhs: Box::new(Ast::AndOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_expr_prime_orand() {
        let toks = vec![
            Token::Or,
            Token::Identifier(String::from("b")),
            Token::And,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.expr_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::AndOp {
            lhs: Box::new(Ast::OrOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_arith_op() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.arith_op().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_arith_op_prime_null() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.arith_op_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.arith_op_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.arith_op_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::AddOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.arith_op_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.arith_op_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::SubOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_arith_op_prime_addsub() {
        let toks = vec![
            Token::Add,
            Token::Identifier(String::from("b")),
            Token::Sub,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.arith_op_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::SubOp {
            lhs: Box::new(Ast::AddOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_arith_op_prime_subadd() {
        let toks = vec![
            Token::Sub,
            Token::Identifier(String::from("b")),
            Token::Add,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.arith_op_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::AddOp {
            lhs: Box::new(Ast::SubOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_relation() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.relation().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_relation_prime_null() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::LT,
            lhs: Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::LT,
            lhs: Box::new(Ast::Relation {
                op: RelationOp::LT,
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::LTE,
            lhs: Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::LTE,
            lhs: Box::new(Ast::Relation {
                op: RelationOp::LTE,
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::GT,
            lhs: Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::GT,
            lhs: Box::new(Ast::Relation {
                op: RelationOp::GT,
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::GTE,
            lhs: Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::GTE,
            lhs: Box::new(Ast::Relation {
                op: RelationOp::GTE,
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::Eq,
            lhs: Box::new(Ast::Var {
                id: String::from("a"),
                line: 1,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::Eq,
            lhs: Box::new(Ast::Relation {
                op: RelationOp::Eq,
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::NotEq,
            lhs: Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.relation_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Relation {
            op: RelationOp::NotEq,
            lhs: Box::new(Ast::Relation {
                op: RelationOp::NotEq,
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_term() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.term().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_term_prime_null() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.term_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.term_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.term_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::MulOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.term_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("b"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.term_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::DivOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_term_prime_muldiv() {
        let toks = vec![
            Token::Mul,
            Token::Identifier(String::from("b")),
            Token::Div,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.term_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::DivOp {
            lhs: Box::new(Ast::MulOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_term_prime_divmul() {
        let toks = vec![
            Token::Div,
            Token::Identifier(String::from("b")),
            Token::Mul,
            Token::Identifier(String::from("c")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.term_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::MulOp {
            lhs: Box::new(Ast::DivOp {
                lhs: Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                rhs: Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            }),
            rhs: Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_factor_name() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo"),
                line: 1,
            }),
            args: Vec::new(),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::NegateOp {
            operand: Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::NegateOp {
            operand: Box::new(Ast::IntLiteral { 
                value: 5,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::NegateOp {
            operand: Box::new(Ast::FloatLiteral { 
                value: 5.0,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_factor_intliteral() {
        let toks = vec![
            Token::IntLiteral(5),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::IntLiteral { 
            value: 5,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_factor_floatliteral() {
        let toks = vec![
            Token::FloatLiteral(5.0),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::FloatLiteral { 
            value: 5.0,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_factor_stringliteral() {
        let toks = vec![
            Token::String(String::from("test")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::StringLiteral { 
            value: String::from("test"),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_factor_true() {
        let toks = vec![
            Token::True,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::BoolLiteral { 
            value: true,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_factor_false() {
        let toks = vec![
            Token::False,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.factor().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::BoolLiteral { 
            value: false,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_factor_terminalerr_invalidnegation() {
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
    fn llparse_factor_terminalerr_missingrparen() {
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
    fn llparse_factor_terminalerr_invalidfirstterminal() {
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
            id: String::from("foo"),
            line: 1,
        });

        let act_ast = p.procedure_call_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo"),
                line: 1,
            }),
            args: Vec::new(),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("foo"),
            line: 1,
        });

        let act_ast = p.procedure_call_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::ProcCall {
            proc: Box::new(Ast::Var { 
                id: String::from("foo"),
                line: 1,
            }),
            args: vec![
                Box::new(Ast::Var { 
                    id: String::from("a"),
                    line: 1,
                }),
                Box::new(Ast::Var { 
                    id: String::from("b"),
                    line: 1,
                }),
            ],
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_call_terminalerr_missinglparen() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("foo"),
            line: 1,
        });

        p.procedure_call_prime(in_ast).expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected (, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_procedure_call_terminalerr_missingrparen() {
        let toks = vec![
            Token::LParen,
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("foo"),
            line: 1,
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
        let act_errs = p.get_errors();

        let exp_vec_ast = vec![
            Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            })
        ];
        let exp_errs = &Vec::new();

        assert_eq!(act_vec_ast, exp_vec_ast);
        assert_eq!(act_errs, exp_errs);
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
        let act_errs = p.get_errors();

        let exp_vec_ast = vec![
            Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            }),
            Box::new(Ast::Var { 
                id: String::from("b"),
                line: 1,
            }),
            Box::new(Ast::Var { 
                id: String::from("c"),
                line: 1,
            }),
        ];
        let exp_errs = &Vec::new();

        assert_eq!(act_vec_ast, exp_vec_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_name() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let act_ast = p.name().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.name_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
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
            id: String::from("a"),
            line: 1,
        });

        let act_ast = p.name_prime(in_ast).expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_ast = Box::new(Ast::SubscriptOp { 
            array: Box::new(Ast::Var { 
                id: String::from("a"),
                line: 1,
            }),
            index: Box::new(Ast::IntLiteral {
                value: 1,
            }),
        });
        let exp_errs = &Vec::new();

        assert_eq!(act_ast, exp_ast);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_name_prime_terminalerr_missingrsquare() {
        let toks = vec![
            Token::LSquare,
            Token::IntLiteral(1),
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let in_ast = Box::new(Ast::Var { 
            id: String::from("a"),
            line: 1,
        });

        p.name_prime(in_ast).expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected ], found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_identifier() {
        let toks = vec![
            Token::Identifier(String::from("a")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_id, _) = p.identifier().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_id = String::from("a");
        let exp_errs = &Vec::new();

        assert_eq!(act_id, exp_id);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_identifier_terminalerr_missingidentifier() {
        let toks = vec![
            Token::Unknown,
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        p.identifier().expect_err(format!("Parse successful. Expected {:?}, found", TerminalError).as_str());
        let act_errs = p.get_errors();

        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Expected identifier, found {}", Token::Unknown) }
        ];

        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_identifier_resyncerr_whitespaceseparatedidentifiers() {
        let toks = vec![
            Token::Identifier(String::from("spl")),
            Token::Identifier(String::from("it")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_id, _) = p.identifier().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_id = String::from("split");
        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Expected identifier, found 2 identifiers") }
        ];

        assert_eq!(act_id, exp_id);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_identifier_resyncerr_invalidchars() {
        let toks = vec![
            Token::Identifier(String::from("spl")),
            Token::Invalid(String::from("@")),
            Token::Identifier(String::from("it")),
            Token::Invalid(String::from("#")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_id, _) = p.identifier().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_id = String::from("spl@it#");
        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: format!("Identifier {} contains invalid chars", exp_id) },
        ];

        assert_eq!(act_id, exp_id);
        assert_eq!(act_errs, exp_errs);
    }

    #[test]
    fn llparse_identifier_resyncerr_leadingunderscore() {
        let toks = vec![
            Token::Invalid(String::from("_")),
            Token::Identifier(String::from("private_var")),
        ];
        let s = TestScanner::new(toks);
        let mut p = LLParser::new(Box::new(s));

        let (act_id, _) = p.identifier().expect("Parse failed");
        let act_errs = p.get_errors();

        let exp_id = String::from("_private_var");
        let exp_errs =  &vec![
            CompilerError::Error { line: 1, msg: String::from("Identifiers cannot begin with _") },
            CompilerError::Error { line: 1, msg: format!("Identifier {} contains invalid chars", exp_id) },
        ];

        assert_eq!(act_id, exp_id);
        assert_eq!(act_errs, exp_errs);
    }
}
