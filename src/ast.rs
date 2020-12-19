use core::fmt::Display;

pub enum Stmt {
    LetStmt { name: Id, value: Expr },
    ReturnStmt(Expr),
    ExprStmt(Expr),
    BlockStmt(BlockStmt),
}

pub enum Expr {
    Id(Id),
    IntLiteral(i32),
    PrefixExpr {
        operator: String,
        right: Box<Expr>,
    },
    InfixExpr {
        left: Box<Expr>,
        operator: String,
        right: Box<Expr>,
    },
    BoolLiteral(bool),
    IfExpr {
        condition: Box<Expr>,
        consequence: Box<BlockStmt>,
        alternative: Option<BlockStmt>,
    },
    FuncLiteral {
        params: Vec<Id>,
        body: BlockStmt,
    },
    CallExpr {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    StrLiteral(String),
    ArrayLiteral(Vec<Expr>),
    IndexExpr {
        left: Box<Expr>,
        index: Box<Expr>,
    },
    HashLiteral(Vec<(Expr, Expr)>),
    EntryPoint(BlockStmt),
}

pub struct Program {
    pub stmts: Vec<Stmt>,
}

pub type Id = String;

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::LetStmt { name, value } => write!(f, "let {} = {}", name, value),
            Self::ReturnStmt(expr) => write!(f, "return {}", expr),
            Self::ExprStmt(expr) => write!(f, "{}", expr),
            Self::BlockStmt(bs) => write!(f, "{}", bs),
        }
    }
}
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Id(id) => write!(f, "{}", id),
            Self::IntLiteral(i) => write!(f, "{}", i),
            Self::PrefixExpr { operator, right } => write!(f, "({}{})", operator, right),
            Self::InfixExpr {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator, right),
            Self::BoolLiteral(b) => write!(f, "{}", b),
            Self::IfExpr {
                condition,
                consequence,
                alternative,
            } => match alternative {
                Some(alt) => write!(f, "if{}{}else {}", condition, consequence, alt),
                _ => write!(f, "if{}{}", condition, consequence),
            },
            Self::FuncLiteral { params, body } => {
                let mut ps = vec![];
                for p in params.iter() {
                    ps.push(p.clone());
                }
                write!(f, "fn({}){}", ps.join(", "), body)
            }
            Self::CallExpr { func, args } => {
                let mut ps = vec![];
                for a in args.iter() {
                    ps.push(format!("{}", a));
                }
                write!(f, "{}({})", func, ps.join(", "))
            }
            Self::StrLiteral(s) => write!(f, "{}", s),
            Self::ArrayLiteral(elems) => {
                let mut es = vec![];
                for e in elems.iter() {
                    es.push(format!("{}", e));
                }
                write!(f, "[{}]", es.join(", "))
            }
            Self::IndexExpr { left, index } => write!(f, "{}[{}]", left, index),
            Self::HashLiteral(list) => {
                let mut pairs = vec![];
                for item in list.iter() {
                    pairs.push(format!("{}:{}", item.0, item.1));
                }
                write!(f, "{{{}}}", pairs.join(", "))
            }
            Self::EntryPoint(body) => write!(f, "fn(){}", body),
        }
    }
}

pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}
impl Display for BlockStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        let mut stmts = vec![];
        for s in self.stmts.iter() {
            stmts.push(format!("{}", s));
        }
        write!(f, "{{\n{}\n}}", stmts.join("\n"))
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        let mut stmts = vec![];
        for s in self.stmts.iter() {
            stmts.push(format!("{}", s));
        }
        write!(f, "{}", stmts.join("\n"))
    }
}
