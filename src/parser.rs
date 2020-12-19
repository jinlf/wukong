use crate::ast;
use crate::lexer;
use crate::token;
use core::fmt::Display;

pub struct Parser {
    l: lexer::Lexer,
    cur_tk: token::Token,
    peek_tk: token::Token,
}

pub fn new(l: lexer::Lexer) -> Parser {
    let mut p = Parser {
        l: l,
        cur_tk: lexer::new_token(token::Type::Illegal, 0),
        peek_tk: lexer::new_token(token::Type::Illegal, 0),
    };
    p.next_token();
    p.next_token();
    p
}

impl Parser {
    pub fn next_token(&mut self) -> Result<(), ParserError> {
        self.cur_tk = std::mem::replace(&mut self.peek_tk, self.l.next_token()?);
        Ok(())
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, ParserError> {
        let mut stmts = vec![];
        while !self.cur_tk_is(&token::Type::Eof) {
            stmts.push(self.parse_stmt()?);
        }

        Ok(ast::Program { stmts: stmts })
    }

    fn cur_tk_is(&self, tt: &token::Type) -> bool {
        self.cur_tk.ty == *tt
    }

    fn parse_stmt(&mut self) -> Result<ast::Stmt, ParserError> {
        match self.cur_tk.ty {
            token::Type::Let => self.parse_let_stmt(),
            token::Type::Return => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<ast::Stmt, ParserError> {
        self.expect_cur(&token::Type::Let)?;
        let id = String::from_utf8(self.cur_tk.literal.clone())?;
        self.expect_cur(&token::Type::Id)?;
        self.expect_cur(&token::Type::Assign)?;
        let expr = self.parse_expr(Precedence::Lowest)?;
        if self.cur_tk_is(&token::Type::Semicolon) {
            self.next_token()?;
        }
        Ok(ast::Stmt::LetStmt {
            name: id,
            value: expr,
        })
    }
    fn parse_return_stmt(&mut self) -> Result<ast::Stmt, ParserError> {
        self.expect_cur(&token::Type::Return)?;
        let expr = self.parse_expr(Precedence::Lowest)?;
        if self.cur_tk_is(&token::Type::Semicolon) {
            self.next_token()?;
        }
        Ok(ast::Stmt::ReturnStmt(expr))
    }
    fn parse_expr_stmt(&mut self) -> Result<ast::Stmt, ParserError> {
        let expr = self.parse_expr(Precedence::Lowest)?;
        if self.cur_tk_is(&token::Type::Semicolon) {
            self.next_token()?;
        }
        Ok(ast::Stmt::ExprStmt(expr))
    }

    fn expect_cur(&mut self, tt: &token::Type) -> Result<(), ParserError> {
        if self.cur_tk.ty == *tt {
            self.next_token()?;
            Ok(())
        } else {
            Err(ParserError::UnexpectedToken {
                expected: format!("{:?}", tt),
                actual: format!("{:?}", self.cur_tk.ty),
            })
        }
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Result<ast::Expr, ParserError> {
        match lookup_prefix_parse_fn(&self.cur_tk.ty) {
            Some(prefix) => {
                let mut left = prefix(self)?;
                while !self.cur_tk_is(&token::Type::Semicolon) && precedence < self.cur_precedence()
                {
                    match lookup_infix_prase_fn(&self.cur_tk.ty) {
                        Some(infix) => {
                            left = infix(self, left)?;
                        }
                        _ => return Ok(left),
                    }
                }
                Ok(left)
            }
            _ => Err(ParserError::NoPrefixParseFn(format!(
                "{:?}",
                self.cur_tk.ty
            ))),
        }
    }

    fn parse_id(&mut self) -> Result<ast::Expr, ParserError> {
        let id = String::from_utf8(self.cur_tk.literal.clone())?;
        self.expect_cur(&token::Type::Id)?;
        Ok(ast::Expr::Id(id))
    }
    fn parse_int_literal(&mut self) -> Result<ast::Expr, ParserError> {
        let s = String::from_utf8(self.cur_tk.literal.clone())?;
        self.expect_cur(&token::Type::Int)?;
        let i = i32::from_str_radix(&s[..], 10)?;
        Ok(ast::Expr::IntLiteral(i))
    }
    fn parse_prefix_expr(&mut self) -> Result<ast::Expr, ParserError> {
        let operator = String::from_utf8(self.cur_tk.literal.clone())?;
        self.next_token()?;
        Ok(ast::Expr::PrefixExpr {
            operator: operator,
            right: Box::new(self.parse_expr(Precedence::Prefix)?),
        })
    }

    fn cur_precedence(&self) -> Precedence {
        lookup_precedence(&self.cur_tk.ty)
    }

    fn parse_infix_expr(&mut self, left: ast::Expr) -> Result<ast::Expr, ParserError> {
        let operator = String::from_utf8(self.cur_tk.literal.clone())?;
        let precedence = self.cur_precedence();
        self.next_token()?;
        Ok(ast::Expr::InfixExpr {
            left: Box::new(left),
            operator: operator,
            right: Box::new(self.parse_expr(precedence)?),
        })
    }

    fn parse_bool_literal(&mut self) -> Result<ast::Expr, ParserError> {
        let value = match self.cur_tk.ty {
            token::Type::True => true,
            token::Type::False => false,
            _ => {
                return Err(ParserError::UnexpectedToken {
                    expected: "true for false".to_string(),
                    actual: format!("{:?}", self.cur_tk.ty),
                })
            }
        };
        self.next_token()?;
        Ok(ast::Expr::BoolLiteral(value))
    }
    fn parse_grouped_expr(&mut self) -> Result<ast::Expr, ParserError> {
        self.expect_cur(&token::Type::LParen)?;
        let exp = self.parse_expr(Precedence::Lowest)?;
        self.expect_cur(&token::Type::RParen)?;
        Ok(exp)
    }
    fn parse_if_expr(&mut self) -> Result<ast::Expr, ParserError> {
        self.expect_cur(&token::Type::If)?;
        self.expect_cur(&token::Type::LParen)?;
        let condition = self.parse_expr(Precedence::Lowest)?;
        self.expect_cur(&token::Type::RParen)?;
        let consequence = self.parse_block_stmt()?;
        let alternative = if self.cur_tk_is(&token::Type::Else) {
            self.next_token()?;
            Some(self.parse_block_stmt()?)
        } else {
            None
        };
        Ok(ast::Expr::IfExpr {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative: alternative,
        })
    }
    fn parse_block_stmt(&mut self) -> Result<ast::BlockStmt, ParserError> {
        self.expect_cur(&token::Type::LBrace)?;
        let mut stmts = vec![];
        if self.cur_tk_is(&token::Type::RBrace) {
            self.next_token()?;
            return Ok(ast::BlockStmt { stmts: stmts });
        }

        while !self.cur_tk_is(&token::Type::RBrace) {
            stmts.push(self.parse_stmt()?);
        }
        self.next_token()?;
        Ok(ast::BlockStmt { stmts: stmts })
    }
    fn parse_func_literal(&mut self) -> Result<ast::Expr, ParserError> {
        self.expect_cur(&token::Type::Func)?;
        self.expect_cur(&token::Type::LParen)?;
        let params = self.parse_func_params()?;
        self.expect_cur(&token::Type::RParen)?;
        Ok(ast::Expr::FuncLiteral {
            params: params,
            body: self.parse_block_stmt()?,
        })
    }
    fn parse_func_params(&mut self) -> Result<Vec<ast::Id>, ParserError> {
        let mut ids = vec![];
        if self.cur_tk_is(&token::Type::RParen) {
            return Ok(ids);
        }
        let id = String::from_utf8(self.cur_tk.literal.clone())?;
        self.expect_cur(&token::Type::Id)?;
        ids.push(id);
        while self.cur_tk_is(&token::Type::Comma) {
            self.next_token()?;
            let id = String::from_utf8(self.cur_tk.literal.clone())?;
            self.expect_cur(&token::Type::Id)?;
            ids.push(id);
        }
        Ok(ids)
    }
    fn parse_call_expr(&mut self, func: ast::Expr) -> Result<ast::Expr, ParserError> {
        self.expect_cur(&token::Type::LParen)?;
        let args = self.parse_expr_list(&token::Type::RParen)?;
        Ok(ast::Expr::CallExpr {
            func: Box::new(func),
            args: args,
        })
    }
    fn parse_str_literal(&mut self) -> Result<ast::Expr, ParserError> {
        let s = String::from_utf8(self.cur_tk.literal.clone())?;
        self.expect_cur(&token::Type::Str)?;
        Ok(ast::Expr::StrLiteral(s))
    }
    fn parse_array_literal(&mut self) -> Result<ast::Expr, ParserError> {
        self.expect_cur(&token::Type::LBracket)?;
        let elems = self.parse_expr_list(&token::Type::RBracket)?;
        Ok(ast::Expr::ArrayLiteral(elems))
    }

    fn parse_expr_list(&mut self, end: &token::Type) -> Result<Vec<ast::Expr>, ParserError> {
        let mut list = vec![];
        if self.cur_tk_is(end) {
            self.next_token()?;
            return Ok(list);
        }
        list.push(self.parse_expr(Precedence::Lowest)?);
        while self.cur_tk_is(&token::Type::Comma) {
            self.next_token()?;
            list.push(self.parse_expr(Precedence::Lowest)?);
        }
        self.expect_cur(end)?;
        Ok(list)
    }
    fn parse_index_expr(&mut self, left: ast::Expr) -> Result<ast::Expr, ParserError> {
        self.expect_cur(&token::Type::LBracket)?;
        let index = self.parse_expr(Precedence::Lowest)?;
        self.expect_cur(&token::Type::RBracket)?;
        Ok(ast::Expr::IndexExpr {
            left: Box::new(left),
            index: Box::new(index),
        })
    }
    fn parse_hash_literal(&mut self) -> Result<ast::Expr, ParserError> {
        self.expect_cur(&token::Type::LBrace)?;
        let mut list = vec![];
        if self.cur_tk_is(&token::Type::RBrace) {
            self.next_token()?;
            return Ok(ast::Expr::HashLiteral(list));
        }
        let key = self.parse_expr(Precedence::Lowest)?;
        self.expect_cur(&token::Type::Colon)?;
        let value = self.parse_expr(Precedence::Lowest)?;
        list.push((key, value));
        while self.cur_tk_is(&token::Type::Comma) {
            self.next_token()?;
            let key = self.parse_expr(Precedence::Lowest)?;
            self.expect_cur(&token::Type::Colon)?;
            let value = self.parse_expr(Precedence::Lowest)?;
            list.push((key, value));
        }
        self.expect_cur(&token::Type::RBrace)?;
        Ok(ast::Expr::HashLiteral(list))
    }
}

#[derive(Debug)]
pub enum ParserError {
    LexerError(lexer::LexerError),
    UnexpectedToken { expected: String, actual: String },
    FromUtf8Error(std::string::FromUtf8Error),
    NoPrefixParseFn(String),
    ParseIntError(std::num::ParseIntError),
}
impl From<lexer::LexerError> for ParserError {
    fn from(e: lexer::LexerError) -> Self {
        Self::LexerError(e)
    }
}
impl From<std::string::FromUtf8Error> for ParserError {
    fn from(e: std::string::FromUtf8Error) -> Self {
        Self::FromUtf8Error(e)
    }
}
impl From<std::num::ParseIntError> for ParserError {
    fn from(e: std::num::ParseIntError) -> Self {
        Self::ParseIntError(e)
    }
}

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}
fn lookup_precedence(tt: &token::Type) -> Precedence {
    match tt {
        token::Type::Eq | token::Type::NotEq => Precedence::Equals,
        token::Type::Lt | token::Type::Gt => Precedence::LessGreater,
        token::Type::Plus | token::Type::Minus => Precedence::Sum,
        token::Type::Slash | token::Type::Asterisk => Precedence::Product,
        token::Type::LParen => Precedence::Call,
        token::Type::LBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}

type PrefixParseFn = fn(&mut Parser) -> Result<ast::Expr, ParserError>;
fn lookup_prefix_parse_fn(tt: &token::Type) -> Option<PrefixParseFn> {
    Some(match tt {
        token::Type::Id => |p: &mut Parser| p.parse_id(),
        token::Type::Int => |p: &mut Parser| p.parse_int_literal(),
        token::Type::Bang | token::Type::Minus => |p: &mut Parser| p.parse_prefix_expr(),
        token::Type::True | token::Type::False => |p: &mut Parser| p.parse_bool_literal(),
        token::Type::LParen => |p: &mut Parser| p.parse_grouped_expr(),
        token::Type::If => |p: &mut Parser| p.parse_if_expr(),
        token::Type::Func => |p: &mut Parser| p.parse_func_literal(),
        token::Type::Str => |p: &mut Parser| p.parse_str_literal(),
        token::Type::LBracket => |p: &mut Parser| p.parse_array_literal(),
        token::Type::LBrace => |p: &mut Parser| p.parse_hash_literal(),
        _ => return None,
    })
}
type InfixParseFn = fn(&mut Parser, left: ast::Expr) -> Result<ast::Expr, ParserError>;
fn lookup_infix_prase_fn(tt: &token::Type) -> Option<InfixParseFn> {
    Some(match tt {
        token::Type::Plus
        | token::Type::Minus
        | token::Type::Slash
        | token::Type::Asterisk
        | token::Type::Eq
        | token::Type::NotEq
        | token::Type::Lt
        | token::Type::Gt => |p: &mut Parser, left: ast::Expr| p.parse_infix_expr(left),
        token::Type::LParen => |p: &mut Parser, func: ast::Expr| p.parse_call_expr(func),
        token::Type::LBracket => |p: &mut Parser, func: ast::Expr| p.parse_index_expr(func),
        _ => return None,
    })
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::LexerError(e) => write!(f, "{}", e),
            Self::UnexpectedToken { expected, actual } => {
                write!(f, "expect token {}, actual {}", expected, actual)
            }
            Self::FromUtf8Error(e) => write!(f, "{}", e),
            Self::NoPrefixParseFn(token) => write!(f, "no prefix parse function for {}", token),
            Self::ParseIntError(e) => write!(f, "{}", e),
        }
    }
}
