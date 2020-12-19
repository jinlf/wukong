use crate::wasm;
use std::io::Write;
use wasmtime::*;

pub struct VM {
    instance: wasmtime::Instance,
}
impl VM {
    pub fn run<S: AsRef<str>>(&mut self, func_name: S) -> Result<i32, VMError> {
        let func = match self.instance.get_func(func_name.as_ref()) {
            Some(func) => func,
            _ => return Err(VMError::FuncNotFound(String::from(func_name.as_ref()))),
        };
        let func_inst = func.get0::<i32>()?;
        Ok(func_inst()?)
    }
}
pub fn new(module: wasm::Module) -> Result<VM, VMError> {
    println!("{:#?}", module.clone());
    let store = Store::default();
    let bytes: Vec<u8> = module.into();
    match std::fs::File::create("test.wasm") {
        Ok(mut f) => {
            f.write_all(&bytes).unwrap();
        }
        _ => {}
    }
    println!("{:02X?}", bytes);
    let module = Module::from_binary(store.engine(), &bytes)?;
    let instance = Instance::new(&store, &module, &[])?;
    Ok(VM { instance: instance })
}

#[derive(Debug)]
pub enum VMError {
    AnyhowError(anyhow::Error),
    FuncNotFound(String),
    WasmTrap(wasmtime::Trap),
}
impl From<anyhow::Error> for VMError {
    fn from(e: anyhow::Error) -> Self {
        Self::AnyhowError(e)
    }
}
impl From<wasmtime::Trap> for VMError {
    fn from(e: wasmtime::Trap) -> Self {
        Self::WasmTrap(e)
    }
}
