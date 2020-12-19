use crate::ast;
use crate::wasm;

pub static ENTRY_POINT: &str = "main";

#[derive(Clone)]
struct VarInfo {
    localidx: u32,
    closure_typeidx: wasm::TypeIdx,
}

pub struct Compiler {
    pub module: wasm::Module,
    mem_pos: u32,
    elem_offset: u32,
}
impl Compiler {
    pub fn compile_program(&mut self, program: ast::Program) -> Result<(), CompilerError> {
        let func = ast::Expr::FuncLiteral {
            params: vec![],
            body: ast::BlockStmt {
                stmts: program.stmts,
            },
        };
        self.module = wasm::Module::new();
        let (instrs, locals) = self.code_v_expr(&func, vec![])?;
        match instrs[0] {
            wasm::Instr::MacroFuncIdx(funcidx) => self.module.exports.push(wasm::Export {
                name: String::from(ENTRY_POINT),
                desc: wasm::ExportDesc::Func(funcidx),
            }),
            _ => return Err(CompilerError::Unexpected),
        }
        self.module.mems.push(wasm::Mem { min: 1, max: None });
        self.module.tables.push(wasm::Table {
            limits: wasm::Limits {
                min: 100,
                max: None,
            },
            elem_type: wasm::ElemType::FuncRef,
        });
        Ok(())
    }

    fn code_stmt(
        &mut self,
        is_v: bool,
        stmt: &ast::Stmt,
        locals: Vec<(String, VarInfo)>,
    ) -> Result<(Vec<wasm::Instr>, Vec<wasm::ValType>, Vec<(String, VarInfo)>), CompilerError> {
        match stmt {
            ast::Stmt::ExprStmt(expr) => {
                let mut instrs = vec![];
                let (mut sub_instrs, sub_locals) = if is_v {
                    self.code_v_expr(expr, locals)?
                } else {
                    self.code_b_expr(expr, locals)?
                };
                instrs.append(&mut sub_instrs);
                Ok((instrs, vec![wasm::ValType::I32; 1], sub_locals))
            }
            ast::Stmt::ReturnStmt(expr) => {
                let mut instrs = vec![];
                let (mut sub_instrs, sub_locals) = self.code_b_expr(expr, locals)?;
                instrs.append(&mut sub_instrs);
                instrs.push(wasm::Instr::Return);
                Ok((instrs, vec![wasm::ValType::I32; 1], sub_locals))
            }
            ast::Stmt::LetStmt { name, value } => {
                let localidx = locals.len() as u32;
                let mut locals = locals.clone();
                locals.push((
                    name.clone(),
                    VarInfo {
                        localidx: localidx,
                        closure_typeidx: u32::MAX,
                    },
                ));

                let mut instrs = vec![];
                let (mut sub_instrs, typeidx, sub_locals) = self.code_c_expr(value, locals)?;
                instrs.append(&mut sub_instrs);
                instrs.push(wasm::Instr::LocalSet(localidx));

                let new_locals = sub_locals
                    .iter()
                    .map(|local| {
                        if &local.0 == name {
                            (
                                name.clone(),
                                VarInfo {
                                    localidx: local.1.localidx,
                                    closure_typeidx: typeidx,
                                },
                            )
                        } else {
                            local.clone()
                        }
                    })
                    .collect();

                Ok((instrs, vec![wasm::ValType::I32; 1], new_locals))
            }
            _ => todo!(),
        }
    }

    fn code_c_expr(
        &mut self,
        expr: &ast::Expr,
        locals: Vec<(String, VarInfo)>,
    ) -> Result<(Vec<wasm::Instr>, wasm::TypeIdx, Vec<(String, VarInfo)>), CompilerError> {
        //TODO
        let frees: Vec<String> = vec![];
        let (instrs, sub_locals) = self.code_v_expr(
            expr,
            vec![(
                String::from("$addr"),
                VarInfo {
                    localidx: 0,
                    closure_typeidx: u32::MAX,
                },
            )],
        )?;

        let functype = wasm::FuncType {
            params: vec![],
            results: vec![wasm::ValType::I32],
        };
        let typeidx = self.module.push_type(functype);
        let funcidx = self.module.funcs.len() as u32;
        self.module.funcs.push(wasm::Func {
            ty: typeidx,
            locals: vec![wasm::ValType::I32; sub_locals.len()],
            body: wasm::Expr { instrs: instrs },
        });

        self.module.elem.push(wasm::Elem {
            table: 0,
            offset: wasm::Expr {
                instrs: vec![wasm::Instr::I32Const(self.elem_offset as i32)],
            },
            init: vec![funcidx],
        });
        self.elem_offset += 1;

        Ok((self.make_closure(self.elem_offset - 1), typeidx, locals))
    }

    fn code_b_expr(
        &mut self,
        expr: &ast::Expr,
        locals: Vec<(String, VarInfo)>,
    ) -> Result<(Vec<wasm::Instr>, Vec<(String, VarInfo)>), CompilerError> {
        match expr {
            ast::Expr::IntLiteral(v) => Ok((vec![wasm::Instr::I32Const(*v)], locals)),
            ast::Expr::BoolLiteral(b) => {
                let v = if *b { 1 } else { 0 };
                Ok((vec![wasm::Instr::I32Const(v)], locals))
            }
            ast::Expr::PrefixExpr { operator, right } => {
                let mut instrs = vec![];
                let (mut sub_instrs, sub_locals) = self.code_b_expr(right, locals)?;
                instrs.append(&mut sub_instrs);
                instrs.append(&mut self.to_unary_op(operator));
                Ok((instrs, sub_locals))
            }
            ast::Expr::InfixExpr {
                left,
                operator,
                right,
            } => {
                let mut instrs = vec![];
                let (mut sub_instrs1, sub_locals1) = self.code_b_expr(left, locals)?;
                instrs.append(&mut sub_instrs1);
                let (mut sub_instrs2, sub_locals2) = self.code_b_expr(right, sub_locals1)?;
                instrs.append(&mut sub_instrs2);
                instrs.append(&mut self.to_binary_op(operator));
                Ok((instrs, sub_locals2))
            }
            ast::Expr::IfExpr {
                condition,
                consequence,
                alternative,
            } => {
                let mut instrs = vec![];
                let (mut sub_instrs, sub_locals) = self.code_b_expr(condition, locals.clone())?;
                instrs.append(&mut sub_instrs);
                let (instrs1, blocktype1, sub_locals1) =
                    self.code_block_stmt(false, consequence, sub_locals.clone())?;
                let (instrs2, blocktype2, sub_locals2) = if let Some(alt) = alternative {
                    self.code_block_stmt(false, &alt, sub_locals)?
                } else {
                    match blocktype1.clone() {
                        wasm::BlockType::Typeidx(typeidx) => {
                            let functype = &self.module.types[typeidx as usize];
                            (
                                vec![wasm::Instr::I32Const(0); functype.results.len()],
                                blocktype1.clone(),
                                vec![],
                            )
                        }
                        wasm::BlockType::Valtype(valtype) => match valtype {
                            Some(vt) => (
                                vec![wasm::Instr::I32Const(0); 1],
                                blocktype1.clone(),
                                vec![],
                            ),
                            _ => (
                                vec![wasm::Instr::I32Const(0); 1],
                                blocktype1.clone(),
                                vec![],
                            ),
                        },
                    }
                };
                instrs.push(wasm::Instr::If {
                    blocktype: blocktype1.clone(),
                    instrs1: instrs1,
                    instrs2: instrs2,
                });
                Ok((instrs, locals))
            }
            _ => {
                let mut instrs = vec![];
                let (mut sub_instrs, sub_locals) = self.code_v_expr(expr, locals)?;
                instrs.append(&mut sub_instrs);
                instrs.append(&mut self.get_basic());
                Ok((instrs, sub_locals))
            }
        }
    }

    fn code_v_expr(
        &mut self,
        expr: &ast::Expr,
        locals: Vec<(String, VarInfo)>,
    ) -> Result<(Vec<wasm::Instr>, Vec<(String, VarInfo)>), CompilerError> {
        match expr {
            ast::Expr::IntLiteral(v) => {
                let mut instrs = vec![];
                instrs.push(wasm::Instr::I32Const(*v));
                instrs.append(&mut self.make_basic());
                Ok((instrs, locals))
            }
            ast::Expr::BoolLiteral(b) => {
                let v = if *b { 1 } else { 0 };
                let mut instrs = vec![];
                instrs.push(wasm::Instr::I32Const(v));
                instrs.append(&mut self.make_basic());
                Ok((instrs, locals))
            }
            ast::Expr::InfixExpr {
                left,
                operator,
                right,
            } => {
                let mut instrs = vec![];
                let (mut sub_instrs1, sub_locals1) = self.code_b_expr(left, locals)?;
                instrs.append(&mut sub_instrs1);
                let (mut sub_instrs2, sub_locals2) = self.code_b_expr(right, sub_locals1)?;
                instrs.append(&mut sub_instrs2);
                instrs.append(&mut self.to_binary_op(operator));
                instrs.append(&mut self.make_basic());
                Ok((instrs, sub_locals2))
            }
            ast::Expr::PrefixExpr { operator, right } => {
                let mut instrs = vec![];
                let (mut sub_instrs, sub_locals) = self.code_b_expr(right, locals)?;
                instrs.append(&mut sub_instrs);
                instrs.append(&mut self.to_unary_op(operator));
                instrs.append(&mut self.make_basic());
                Ok((instrs, sub_locals))
            }
            ast::Expr::IfExpr {
                condition,
                consequence,
                alternative,
            } => {
                let mut instrs = vec![];
                let (mut sub_instrs, sub_locals) = self.code_b_expr(condition, locals.clone())?;
                instrs.append(&mut sub_instrs);
                let (instrs1, blocktype1, sub_locals1) =
                    self.code_block_stmt(true, consequence, sub_locals.clone())?;
                let (instrs2, blocktype2, sub_locals2) = if let Some(alt) = alternative {
                    self.code_block_stmt(true, &alt, sub_locals)?
                } else {
                    match blocktype1.clone() {
                        wasm::BlockType::Typeidx(typeidx) => {
                            let functype = &self.module.types[typeidx as usize];
                            (
                                vec![wasm::Instr::I32Const(0); functype.results.len()],
                                blocktype1.clone(),
                                vec![],
                            )
                        }
                        wasm::BlockType::Valtype(valtype) => match valtype {
                            Some(vt) => (
                                vec![wasm::Instr::I32Const(0); 1],
                                blocktype1.clone(),
                                vec![],
                            ),
                            _ => (
                                vec![wasm::Instr::I32Const(0); 1],
                                blocktype1.clone(),
                                vec![],
                            ),
                        },
                    }
                };
                instrs.push(wasm::Instr::If {
                    blocktype: blocktype1.clone(),
                    instrs1: instrs1,
                    instrs2: instrs2,
                });
                Ok((instrs, locals))
            }
            ast::Expr::FuncLiteral { params, body } => {
                let mut locals = vec![(
                    String::from("$addr"),
                    VarInfo {
                        localidx: 0,
                        closure_typeidx: u32::MAX,
                    },
                )];
                for (i, p) in params.iter().enumerate() {
                    locals.push((
                        p.clone(),
                        VarInfo {
                            localidx: (i + 1) as u32,
                            closure_typeidx: u32::MAX,
                        },
                    ));
                }

                let (instrs, blocktype, sub_locals) = self.code_block_stmt(false, body, locals)?;

                let results = match blocktype {
                    wasm::BlockType::Typeidx(typeid) => {
                        self.module.types[typeid as usize].results.clone()
                    }
                    wasm::BlockType::Valtype(valtype) => match valtype {
                        Some(_) => vec![wasm::ValType::I32],
                        _ => vec![],
                    },
                };
                let functype = wasm::FuncType {
                    params: vec![wasm::ValType::I32; params.len()],
                    results: results,
                };

                let typeidx = self.module.push_type(functype);
                let funcidx = self.module.funcs.len() as u32;
                self.module.funcs.push(wasm::Func {
                    ty: typeidx,
                    locals: vec![wasm::ValType::I32; sub_locals.len()],
                    body: wasm::Expr { instrs: instrs },
                });
                Ok((vec![wasm::Instr::MacroFuncIdx(funcidx)], sub_locals))
            }
            ast::Expr::Id(id) => {
                for (name, varinfo) in locals.iter() {
                    if name == id {
                        let mut instrs = vec![];
                        instrs.push(wasm::Instr::LocalGet(varinfo.localidx));
                        instrs.append(&mut self.get_closure());
                        instrs.push(wasm::Instr::CallIndirect(varinfo.closure_typeidx));
                        return Ok((instrs, locals.clone()));
                    }
                }
                Err(CompilerError::IdNotFound(id.clone()))
            }
            ast::Expr::CallExpr { func, args } => {
                let mut instrs = vec![];
                for arg in args.iter() {
                    let (mut sub_instrs, typeidx, sub_locals) =
                        self.code_c_expr(arg, locals.clone())?;
                    instrs.append(&mut sub_instrs);
                }
                let (mut sub_instrs, sub_locals) = self.code_v_expr(func, locals.clone())?;
                println!("sub_instrs: {:#?}", sub_instrs.clone());
                match sub_instrs[0] {
                    wasm::Instr::MacroFuncIdx(funcidx) => instrs.push(wasm::Instr::Call(funcidx)),
                    _ => instrs.append(&mut sub_instrs),
                }
                Ok((instrs, sub_locals))
            }
            _ => {
                println!("{}", expr);
                todo!();
            }
        }
    }

    fn to_binary_op(&self, operator: &String) -> Vec<wasm::Instr> {
        match &operator[..] {
            "+" => vec![wasm::Instr::I32Add],
            "-" => vec![wasm::Instr::I32Sub],
            "*" => vec![wasm::Instr::I32Mul],
            "/" => vec![wasm::Instr::I32DivU],
            ">" => vec![wasm::Instr::I32GtU],
            "<" => vec![wasm::Instr::I32LtU],
            "==" => vec![wasm::Instr::I32Eq],
            "!=" => vec![wasm::Instr::I32Ne],
            _ => todo!(),
        }
    }
    fn to_unary_op(&self, operator: &String) -> Vec<wasm::Instr> {
        match &operator[..] {
            "-" => vec![wasm::Instr::I32Const(-1), wasm::Instr::I32Mul],
            "!" => vec![wasm::Instr::I32Eqz],
            _ => todo!(),
        }
    }

    fn code_block_stmt(
        &mut self,
        is_v: bool,
        block_stmt: &ast::BlockStmt,
        locals: Vec<(String, VarInfo)>,
    ) -> Result<(Vec<wasm::Instr>, wasm::BlockType, Vec<(String, VarInfo)>), CompilerError> {
        let mut instrs = vec![];
        let mut results = vec![];
        let mut all_locals = locals.clone();
        for stmt in block_stmt.stmts.iter() {
            let (mut sub_instrs, sub_types, sub_locals) =
                self.code_stmt(is_v, stmt, all_locals.clone())?;
            match stmt {
                ast::Stmt::ReturnStmt(_) => results.clear(),
                _ => {}
            }
            instrs.append(&mut sub_instrs);
            results = sub_types.clone();
            all_locals = sub_locals.clone();
        }
        if results.len() == 0 {
            Ok((instrs, wasm::BlockType::Valtype(None), all_locals))
        } else {
            let typeidx = self.module.push_type(wasm::FuncType {
                params: vec![],
                results: results,
            });
            Ok((instrs, wasm::BlockType::Typeidx(typeidx), all_locals))
        }
    }

    fn make_basic(&mut self) -> Vec<wasm::Instr> {
        let instrs = vec![
            wasm::Instr::LocalSet(0),
            wasm::Instr::I32Const(self.mem_pos as i32),
            wasm::Instr::I32Const(b'B' as i32),
            wasm::Instr::I32Store(wasm::MemArg {
                offset: 0,
                align: 2,
            }),
            wasm::Instr::I32Const((self.mem_pos + 4) as i32),
            wasm::Instr::LocalGet(0),
            wasm::Instr::I32Store(wasm::MemArg {
                offset: 0,
                align: 2,
            }),
            wasm::Instr::I32Const(self.mem_pos as i32),
        ];
        self.mem_pos += 8;
        instrs
    }
    fn get_basic(&mut self) -> Vec<wasm::Instr> {
        vec![
            wasm::Instr::LocalSet(0),
            wasm::Instr::LocalGet(0),
            wasm::Instr::I32Load(wasm::MemArg {
                offset: 0,
                align: 2,
            }),
            wasm::Instr::I32Const(b'B' as i32),
            wasm::Instr::I32Ne,
            wasm::Instr::If {
                blocktype: wasm::BlockType::Valtype(None),
                instrs1: vec![wasm::Instr::Unreachable],
                instrs2: vec![],
            },
            wasm::Instr::LocalGet(0),
            wasm::Instr::I32Const(4),
            wasm::Instr::I32Add,
            wasm::Instr::I32Load(wasm::MemArg {
                offset: 0,
                align: 2,
            }),
        ]
    }
    // elem_offset -> mem_offset
    fn make_closure(&mut self, elem_offset: u32) -> Vec<wasm::Instr> {
        let instrs = vec![
            wasm::Instr::I32Const(self.mem_pos as i32),
            wasm::Instr::I32Const(b'C' as i32),
            wasm::Instr::I32Store(wasm::MemArg {
                offset: 0,
                align: 2,
            }),
            wasm::Instr::I32Const((self.mem_pos + 4) as i32),
            wasm::Instr::I32Const(elem_offset as i32),
            wasm::Instr::I32Store(wasm::MemArg {
                offset: 0,
                align: 2,
            }),
            wasm::Instr::I32Const(self.mem_pos as i32),
        ];
        self.mem_pos += 8;
        instrs
    }
    // stack[0] is mem_offset -> elem_offset
    fn get_closure(&mut self) -> Vec<wasm::Instr> {
        vec![
            wasm::Instr::LocalSet(0),
            wasm::Instr::LocalGet(0),
            wasm::Instr::I32Load(wasm::MemArg {
                offset: 0,
                align: 2,
            }),
            wasm::Instr::I32Const(b'C' as i32),
            wasm::Instr::I32Ne,
            wasm::Instr::If {
                blocktype: wasm::BlockType::Valtype(None),
                instrs1: vec![wasm::Instr::Unreachable],
                instrs2: vec![],
            },
            wasm::Instr::LocalGet(0),
            wasm::Instr::I32Const(4),
            wasm::Instr::I32Add,
            wasm::Instr::I32Load(wasm::MemArg {
                offset: 0,
                align: 2,
            }),
        ]
    }
}

pub fn new() -> Compiler {
    Compiler {
        module: wasm::Module::new(),
        mem_pos: 0,
        elem_offset: 0,
    }
}

#[derive(Debug)]
pub enum CompilerError {
    Unexpected,
    IdNotFound(String),
}
