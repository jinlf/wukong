pub type Name = String;
#[derive(PartialEq, Debug, Clone)]
pub enum ValType {
    I32,
}
pub type ResultType = Vec<ValType>;
#[derive(PartialEq, Debug, Clone)]
pub struct FuncType {
    pub params: ResultType,
    pub results: ResultType,
}
#[derive(PartialEq, Debug, Clone)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}
pub type MemType = Limits;
#[derive(PartialEq, Debug, Clone)]
pub struct TableType {
    pub limits: Limits,
    pub elem_type: ElemType,
}
#[derive(PartialEq, Debug, Clone)]
pub enum ElemType {
    FuncRef,
}
#[derive(PartialEq, Debug, Clone)]
pub struct GlobalType {
    pub r#mut: Mut,
    pub valtype: ValType,
}
#[derive(PartialEq, Debug, Clone)]
pub enum Mut {
    Const,
    Var,
}
pub enum ExternType {
    Func(FuncType),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}
#[derive(PartialEq, Debug, Clone)]
pub enum Instr {
    I32Const(i32),
    Drop,
    LocalGet(LocalIdx),
    LocalSet(LocalIdx),
    GlobalGet(GlobalIdx),
    GlobalSet(GlobalIdx),
    I32Load(MemArg),
    I32Store(MemArg),
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtU,
    I32GtU,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivU,
    Nop,
    Unreachable,
    Block {
        blocktype: BlockType,
        instrs: Vec<Instr>,
    },
    Loop {
        blocktype: BlockType,
        instrs: Vec<Instr>,
    },
    If {
        blocktype: BlockType,
        instrs1: Vec<Instr>,
        instrs2: Vec<Instr>,
    },
    Return,
    Call(FuncIdx),
    CallIndirect(TypeIdx),

    //
    MacroFuncIdx(FuncIdx),
}
#[derive(PartialEq, Debug, Clone)]
pub enum BlockType {
    Typeidx(TypeIdx),
    Valtype(Option<ValType>),
}
#[derive(PartialEq, Debug, Clone)]
pub struct MemArg {
    pub offset: u32,
    pub align: u32,
}
#[derive(PartialEq, Debug, Clone)]
pub struct Expr {
    pub instrs: Vec<Instr>,
}
#[derive(PartialEq, Debug, Clone)]
pub struct Module {
    pub types: Vec<FuncType>,
    pub funcs: Vec<Func>,
    pub tables: Vec<Table>,
    pub mems: Vec<Mem>,
    pub globals: Vec<Global>,
    pub elem: Vec<Elem>,
    pub data: Vec<Data>,
    pub start: Option<Start>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}
pub type TypeIdx = u32;
pub type FuncIdx = u32;
pub type TableIdx = u32;
pub type MemIdx = u32;
pub type GlobalIdx = u32;
pub type LocalIdx = u32;
pub type LabelIdx = u32;
#[derive(PartialEq, Debug, Clone)]
pub struct Func {
    pub ty: TypeIdx,
    pub locals: Vec<ValType>,
    pub body: Expr,
}
pub type Table = TableType;
pub type Mem = MemType;
#[derive(PartialEq, Debug, Clone)]
pub struct Global {
    pub ty: GlobalType,
    pub init: Expr,
}
#[derive(PartialEq, Debug, Clone)]
pub struct Elem {
    pub table: TableIdx,
    pub offset: Expr,
    pub init: Vec<FuncIdx>,
}
#[derive(PartialEq, Debug, Clone)]
pub struct Data {
    pub data: MemIdx,
    pub offset: Expr,
    pub init: Vec<u8>,
}
pub type Start = FuncIdx;
#[derive(PartialEq, Debug, Clone)]
pub struct Export {
    pub name: Name,
    pub desc: ExportDesc,
}
#[derive(PartialEq, Debug, Clone)]
pub enum ExportDesc {
    Func(FuncIdx),
    Table(TableIdx),
    Mem(MemIdx),
    Global(GlobalIdx),
}
#[derive(PartialEq, Debug, Clone)]
pub struct Import {
    pub module: Name,
    pub name: Name,
    pub desc: ImportDesc,
}
#[derive(PartialEq, Debug, Clone)]
pub enum ImportDesc {
    Func(TypeIdx),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}

pub fn encode_unsigned(u: u64) -> Vec<u8> {
    if u < 128 {
        return vec![u as u8];
    }
    let mut ret: Vec<u8> = vec![];
    ret.push((u & 0x7F | 0x80) as u8);
    ret.append(&mut encode_unsigned(u >> 7));
    ret
}

pub fn encode_signed(s: i64) -> Vec<u8> {
    if s >= 0 && s < 64 {
        return vec![s as u8];
    }
    if s >= -64 && s < 0 {
        return vec![(s + 128) as u8];
    }

    let mut ret: Vec<u8> = vec![];
    ret.push((s & 0x7F | 0x80) as u8);
    ret.append(&mut encode_signed(s >> 7));
    ret
}

fn encode_vec<T>(list: Vec<T>) -> Vec<u8>
where
    T: Into<Vec<u8>>,
{
    let mut s = vec![];
    s.append(&mut encode_unsigned(list.len() as u64));
    for item in list.into_iter() {
        s.append(&mut item.into());
    }
    s
}

fn encode_name(name: Name) -> Vec<u8> {
    let mut s = vec![];
    let bytes = name.as_bytes();
    s.append(&mut encode_unsigned(bytes.len() as u64));
    s.append(&mut bytes.to_vec());
    s
}

impl Into<Vec<u8>> for ValType {
    fn into(self) -> Vec<u8> {
        match self {
            Self::I32 => vec![0x7F],
        }
    }
}
impl Into<Vec<u8>> for FuncType {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        s.push(0x60);
        s.append(&mut encode_vec(self.params));
        s.append(&mut encode_vec(self.results));
        s
    }
}
impl Into<Vec<u8>> for Limits {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        match self.max {
            Some(max) => {
                s.push(0x01);
                s.append(&mut encode_unsigned(self.min as u64));
                s.append(&mut encode_unsigned(max as u64));
            }
            _ => {
                s.push(0x00);
                s.append(&mut encode_unsigned(self.min as u64));
            }
        }
        s
    }
}
impl Into<Vec<u8>> for TableType {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        s.append(&mut self.elem_type.into());
        s.append(&mut self.limits.into());
        s
    }
}
impl Into<Vec<u8>> for ElemType {
    fn into(self) -> Vec<u8> {
        match self {
            Self::FuncRef => vec![0x70],
        }
    }
}
impl Into<Vec<u8>> for GlobalType {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        s.append(&mut self.valtype.into());
        s.append(&mut self.r#mut.into());
        s
    }
}
impl Into<Vec<u8>> for Mut {
    fn into(self) -> Vec<u8> {
        match self {
            Self::Const => vec![0x00],
            Self::Var => vec![0x01],
        }
    }
}
impl Into<Vec<u8>> for BlockType {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        match self {
            Self::Typeidx(typeidx) => s.append(&mut encode_signed(typeidx as i64)),
            Self::Valtype(valtype) => match valtype {
                Some(valtype) => s.append(&mut valtype.into()),
                _ => s.push(0x40),
            },
        }
        s
    }
}
impl Into<Vec<u8>> for Instr {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        match self {
            Self::Unreachable => s.push(0x00),
            Self::Nop => s.push(0x01),
            Self::Block { blocktype, instrs } => {
                s.append(&mut blocktype.into());
                for instr in instrs.into_iter() {
                    s.append(&mut instr.into());
                }
                s.push(0x0B)
            }
            Self::Loop { blocktype, instrs } => {
                s.append(&mut blocktype.into());
                for instr in instrs.into_iter() {
                    s.append(&mut instr.into());
                }
                s.push(0x0B)
            }
            Self::If {
                blocktype,
                instrs1,
                instrs2,
            } => {
                s.push(0x04);
                s.append(&mut blocktype.into());
                for instr in instrs1.into_iter() {
                    s.append(&mut instr.into());
                }
                if instrs2.len() != 0 {
                    s.push(0x05);
                    for instr in instrs2.into_iter() {
                        s.append(&mut instr.into());
                    }
                }
                s.push(0x0B);
            }
            Self::Return => s.push(0x0F),
            Self::Call(funcidx) => {
                s.push(0x10);
                s.append(&mut encode_unsigned(funcidx as u64));
            }
            Self::CallIndirect(typeidx) => {
                s.push(0x11);
                s.append(&mut encode_unsigned(typeidx as u64));
                s.push(0x00);
            }
            Self::Drop => s.push(0x1A),
            Self::LocalGet(localidx) => {
                s.push(0x20);
                s.append(&mut encode_unsigned(localidx as u64));
            }
            Self::LocalSet(localidx) => {
                s.push(0x21);
                s.append(&mut encode_unsigned(localidx as u64));
            }
            Self::GlobalGet(globalidx) => {
                s.push(0x23);
                s.append(&mut encode_unsigned(globalidx as u64));
            }
            Self::GlobalSet(globalidx) => {
                s.push(0x24);
                s.append(&mut encode_unsigned(globalidx as u64));
            }
            Self::I32Load(memarg) => {
                s.push(0x28);
                s.append(&mut memarg.into());
            }
            Self::I32Store(memarg) => {
                s.push(0x36);
                s.append(&mut memarg.into());
            }
            Self::I32Const(i) => {
                s.push(0x41);
                s.append(&mut encode_signed(i as i64));
            }
            Self::I32Eqz => s.push(0x45),
            Self::I32Eq => s.push(0x46),
            Self::I32Ne => s.push(0x47),
            Self::I32LtU => s.push(0x49),
            Self::I32GtU => s.push(0x4B),
            Self::I32Add => s.push(0x6A),
            Self::I32Sub => s.push(0x6B),
            Self::I32Mul => s.push(0x6C),
            Self::I32DivU => s.push(0x6E),
            _ => {
                println!("{:#?}", self);
                todo!()
            }
        }
        s
    }
}
impl Into<Vec<u8>> for MemArg {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        s.append(&mut encode_unsigned(self.align as u64));
        s.append(&mut encode_unsigned(self.offset as u64));
        s
    }
}
impl Into<Vec<u8>> for Expr {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        for instr in self.instrs.into_iter() {
            s.append(&mut instr.into());
        }
        s.push(0x0B);
        s
    }
}

fn encode_sec<T>(list: Vec<T>, id: u8) -> Vec<u8>
where
    T: Into<Vec<u8>>,
{
    let mut s = vec![];
    if list.len() != 0 {
        s.push(id);
        let mut data = encode_vec(list);
        s.append(&mut encode_unsigned(data.len() as u64));
        s.append(&mut data);
    }
    s
}

impl Into<Vec<u8>> for Import {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        s.append(&mut encode_name(self.module));
        s.append(&mut encode_name(self.name));
        s.append(&mut self.desc.into());
        s
    }
}
impl Into<Vec<u8>> for ImportDesc {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        match self {
            Self::Func(typeidx) => {
                s.push(0x00);
                s.append(&mut encode_unsigned(typeidx as u64));
            }
            Self::Table(tabletype) => {
                s.push(0x01);
                s.append(&mut tabletype.into());
            }
            Self::Mem(memtype) => {
                s.push(0x02);
                s.append(&mut memtype.into());
            }
            Self::Global(globaltype) => {
                s.push(0x03);
                s.append(&mut globaltype.into());
            }
        }
        s
    }
}
impl Into<Vec<u8>> for Global {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        s.append(&mut self.ty.into());
        s.append(&mut self.init.into());
        s
    }
}
impl Into<Vec<u8>> for Export {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        s.append(&mut encode_name(self.name));
        s.append(&mut self.desc.into());
        s
    }
}
impl Into<Vec<u8>> for ExportDesc {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        match self {
            Self::Func(funcidx) => {
                s.push(0x00);
                s.append(&mut encode_unsigned(funcidx as u64));
            }
            Self::Table(tableidx) => {
                s.push(0x01);
                s.append(&mut encode_unsigned(tableidx as u64));
            }
            Self::Mem(memidx) => {
                s.push(0x02);
                s.append(&mut encode_unsigned(memidx as u64));
            }
            Self::Global(globalidx) => {
                s.push(0x03);
                s.append(&mut encode_unsigned(globalidx as u64));
            }
        }
        s
    }
}
impl Into<Vec<u8>> for Elem {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        s.append(&mut encode_unsigned(self.table as u64));
        s.append(&mut self.offset.into());
        s.append(&mut encode_unsigned(self.init.len() as u64));
        for funcidx in self.init.into_iter() {
            s.append(&mut encode_unsigned(funcidx as u64));
        }
        s
    }
}
impl Into<Vec<u8>> for Data {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        s.append(&mut encode_unsigned(self.data as u64));
        s.append(&mut self.offset.into());
        s.append(&mut encode_unsigned(self.init.len() as u64));
        s.append(&mut self.init.clone());
        s
    }
}
impl Into<Vec<u8>> for Module {
    fn into(self) -> Vec<u8> {
        let mut s = vec![];
        s.append(&mut vec![0x00, 0x61, 0x73, 0x6D]);
        s.append(&mut vec![0x01, 0x00, 0x00, 0x00]);
        s.append(&mut encode_sec(self.types, 0x01));
        s.append(&mut encode_sec(self.imports, 0x02));

        // funcsec
        let mut ss = vec![];
        ss.append(&mut encode_unsigned(self.funcs.len() as u64));
        for func in self.funcs.iter() {
            ss.append(&mut encode_unsigned(func.ty as u64));
        }
        s.push(0x03);
        s.append(&mut encode_unsigned(ss.len() as u64));
        s.append(&mut ss);

        s.append(&mut encode_sec(self.tables, 0x04));
        s.append(&mut encode_sec(self.mems, 0x05));
        s.append(&mut encode_sec(self.globals, 0x06));
        s.append(&mut encode_sec(self.exports, 0x07));

        // starsec
        match self.start {
            Some(start) => {
                let mut ss = vec![];
                ss.append(&mut encode_unsigned(start as u64));
                s.push(0x08);
                s.append(&mut encode_unsigned(ss.len() as u64));
                s.append(&mut ss);
            }
            _ => {}
        }

        s.append(&mut encode_sec(self.elem, 0x09));
        // codesec
        let mut ss = vec![];
        ss.append(&mut encode_unsigned(self.funcs.len() as u64));
        for code in self.funcs.into_iter() {
            struct merged_local {
                n: u32,
                t: ValType,
            }
            let mut mlocal = merged_local {
                n: 0,
                t: ValType::I32,
            };

            let mut mlocals = vec![];
            for (i, valtype) in code.locals.into_iter().enumerate() {
                if i == 0 {
                    mlocal = merged_local { n: 1, t: valtype }
                } else if mlocal.t == valtype {
                    mlocal.n += 1;
                } else {
                    mlocals.push(mlocal);
                    mlocal = merged_local { n: 1, t: valtype }
                }
            }
            if mlocal.n != 0 {
                mlocals.push(mlocal);
            }

            let mut sss = vec![];
            sss.append(&mut encode_unsigned(mlocals.len() as u64));
            for mlocal in mlocals.into_iter() {
                sss.append(&mut encode_unsigned(mlocal.n as u64));
                sss.append(&mut mlocal.t.into());
            }
            sss.append(&mut code.body.into());
            ss.append(&mut encode_unsigned(sss.len() as u64));
            ss.append(&mut sss);
        }
        s.push(0x0A);
        s.append(&mut encode_unsigned(ss.len() as u64));
        s.append(&mut ss);

        s.append(&mut encode_sec(self.data, 0x0B));
        s
    }
}

impl Module {
    pub fn new() -> Self {
        Self {
            types: vec![],
            funcs: vec![],
            tables: vec![],
            mems: vec![],
            globals: vec![],
            elem: vec![],
            data: vec![],
            start: None,
            imports: vec![],
            exports: vec![],
        }
    }

    pub fn push_type(&mut self, functype: FuncType) -> u32 {
        let pos = match self.types.iter().position(|t| t == &functype) {
            Some(pos) => pos,
            _ => {
                let pos = self.types.len();
                self.types.push(functype);
                pos
            }
        };
        pos as u32
    }
}
