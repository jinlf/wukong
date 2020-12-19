use crate::token;
use core::fmt::Display;

pub struct Lexer {
    pub input: String,
    pub pos: usize,
    pub read_pos: usize,
    pub ch: u8,
}

pub fn new<S: AsRef<str>>(input: S) -> Lexer {
    let mut l = Lexer {
        input: String::from(input.as_ref()),
        pos: 0,
        read_pos: 0,
        ch: 0,
    };
    l.read_char();
    l
}

impl Lexer {
    fn read_char(&mut self) {
        if self.read_pos >= self.input.as_bytes().len() {
            self.ch = 0
        } else {
            self.ch = self.input.as_bytes()[self.read_pos]
        }
        self.pos = self.read_pos;
        self.read_pos += 1
    }

    pub fn next_token(&mut self) -> Result<token::Token, LexerError> {
        self.skip_whitespace();
        let tok = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    token::Token {
                        ty: token::Type::Eq,
                        literal: vec![b'=', b'='],
                    }
                } else {
                    new_token(token::Type::Assign, self.ch)
                }
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    token::Token {
                        ty: token::Type::NotEq,
                        literal: vec![b'!', b'='],
                    }
                } else {
                    new_token(token::Type::Bang, self.ch)
                }
            }
            b';' => new_token(token::Type::Semicolon, self.ch),
            b'(' => new_token(token::Type::LParen, self.ch),
            b')' => new_token(token::Type::RParen, self.ch),
            b',' => new_token(token::Type::Comma, self.ch),
            b'+' => new_token(token::Type::Plus, self.ch),
            b'-' => new_token(token::Type::Minus, self.ch),
            b'*' => new_token(token::Type::Asterisk, self.ch),
            b'/' => new_token(token::Type::Slash, self.ch),
            b'<' => new_token(token::Type::Lt, self.ch),
            b'>' => new_token(token::Type::Gt, self.ch),
            b'{' => new_token(token::Type::LBrace, self.ch),
            b'}' => new_token(token::Type::RBrace, self.ch),
            b'[' => new_token(token::Type::LBracket, self.ch),
            b']' => new_token(token::Type::RBracket, self.ch),
            b':' => new_token(token::Type::Colon, self.ch),
            b'"' => token::Token {
                ty: token::Type::Str,
                literal: self.read_str(),
            },
            0 => token::Token {
                ty: token::Type::Eof,
                literal: vec![0],
            },
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_id();
                    return Ok(token::Token {
                        ty: token::lookup_id(std::str::from_utf8(literal.as_slice())?),
                        literal: literal,
                    });
                } else if is_digit(self.ch) {
                    return Ok(token::Token {
                        ty: token::Type::Int,
                        literal: self.read_num(),
                    });
                } else {
                    new_token(token::Type::Illegal, self.ch)
                }
            }
        };
        self.read_char();
        Ok(tok)
    }

    fn read_id(&mut self) -> Vec<u8> {
        let pos = self.pos;
        while is_letter(self.ch) {
            self.read_char()
        }
        self.input.as_bytes()[pos..self.pos].to_vec()
    }

    fn skip_whitespace(&mut self) {
        while match self.ch {
            b' ' | b'\t' | b'\n' | b'\r' => true,
            _ => false,
        } {
            self.read_char()
        }
    }

    fn read_num(&mut self) -> Vec<u8> {
        let pos = self.pos;
        while is_digit(self.ch) {
            self.read_char()
        }
        self.input.as_bytes()[pos..self.pos].to_vec()
    }

    fn peek_char(&self) -> u8 {
        if self.read_pos >= self.input.as_bytes().len() {
            0
        } else {
            self.input.as_bytes()[self.read_pos]
        }
    }

    fn read_str(&mut self) -> Vec<u8> {
        let pos = self.pos + 1;
        loop {
            self.read_char();
            if self.ch == b'"' || self.ch == 0 {
                break;
            }
        }
        self.input.as_bytes()[pos..self.pos].to_vec()
    }
}

#[derive(Debug)]
pub enum LexerError {
    Utf8Error(std::str::Utf8Error),
}
impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Utf8Error(e) => write!(f, "{:?}", e),
        }
    }
}

impl From<std::str::Utf8Error> for LexerError {
    fn from(e: std::str::Utf8Error) -> Self {
        Self::Utf8Error(e)
    }
}

pub fn new_token(ty: token::Type, ch: u8) -> token::Token {
    token::Token {
        ty: ty,
        literal: vec![ch],
    }
}

fn is_letter(ch: u8) -> bool {
    match ch {
        b'a'..=b'z' | b'A'..=b'Z' | b'_' => true,
        _ => false,
    }
}

fn is_digit(ch: u8) -> bool {
    match ch {
        b'0'..=b'9' => true,
        _ => false,
    }
}
