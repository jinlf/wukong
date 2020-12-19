#[derive(Debug, PartialEq)]
pub enum Type {
    Illegal,
    Eof,
    Id,
    Int,
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen, // (
    RParen,
    LBrace, // {
    RBrace,
    Func,
    Let,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    True,
    False,
    If,
    Else,
    Return,
    Eq,
    NotEq,
    Str,
    LBracket, // [
    RBracket,
    Colon,
}

#[derive(Debug)]
pub struct Token {
    pub ty: Type,
    pub literal: Vec<u8>,
}

pub fn lookup_id<S: AsRef<str>>(id: S) -> Type {
    match id.as_ref() {
        "fn" => Type::Func,
        "let" => Type::Let,
        "true" => Type::True,
        "false" => Type::False,
        "if" => Type::If,
        "else" => Type::Else,
        "return" => Type::Return,
        _ => Type::Id,
    }
}
