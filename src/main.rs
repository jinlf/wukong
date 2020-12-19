mod ast;
mod compiler;
mod lexer;
mod parser;
mod repl;
mod token;
mod vm;
mod wasm;

fn main() {
    repl::start(&mut std::io::stdin(), &mut std::io::stdout())
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestCase<'a> {
        input: &'a str,
        expected: i32,
    }
    impl<'a> TestCase<'a> {
        fn new(input: &'a str, expected: i32) -> Self {
            Self {
                input: input,
                expected: expected,
            }
        }
    }

    #[test]
    fn test_int_expr() {
        let tests = vec![
            TestCase::new("5", 5),
            TestCase::new("-5", -5),
            TestCase::new("5+5+5+5-10", 10),
            TestCase::new("2*2*2*2*2", 32),
            TestCase::new("-50+100+-50", 0),
            TestCase::new("5*2+10", 20),
            TestCase::new("5+2*10", 25),
            TestCase::new("20+2*-10", 0),
            TestCase::new("50/2*2+10", 60),
            TestCase::new("2*(5+10)", 30),
            TestCase::new("3*3*3+10", 37),
            TestCase::new("3*(3*3)+10", 37),
            TestCase::new(" (5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for tt in tests.iter() {
            let l = lexer::new(tt.input);
            let mut p = parser::new(l);
            let program = p.parse_program().unwrap();
            let mut comp = compiler::new();
            comp.compile_program(program).unwrap();
            let mut machine = vm::new(comp.module).unwrap();
            let result = machine.run(compiler::ENTRY_POINT).unwrap();
            assert_eq!(result, tt.expected);
        }
    }

    #[test]
    fn test_bool_expr() {
        let tests = vec![
            TestCase::new("true", 1),
            TestCase::new("false", 0),
            TestCase::new("1<2", 1),
            TestCase::new("1>2", 0),
            TestCase::new("1<1", 0),
            TestCase::new("1>1", 0),
            TestCase::new("1==1", 1),
            TestCase::new("1!=1", 0),
            TestCase::new("1==2", 0),
            TestCase::new("1!=2", 1),
            TestCase::new("true==true", 1),
            TestCase::new("false==false", 1),
            TestCase::new("true==false", 0),
            TestCase::new("true!=false", 1),
            TestCase::new("false!=true", 1),
            TestCase::new("(1<2)==true", 1),
            TestCase::new("(1<2)==false", 0),
            TestCase::new("(1>2)==true", 0),
            TestCase::new("(1>2)==false", 1),
        ];
        for tt in tests.iter() {
            let l = lexer::new(tt.input);
            let mut p = parser::new(l);
            let program = p.parse_program().unwrap();
            let mut comp = compiler::new();
            comp.compile_program(program).unwrap();
            let mut machine = vm::new(comp.module).unwrap();
            let result = machine.run(compiler::ENTRY_POINT).unwrap();
            assert_eq!(result, tt.expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            TestCase::new("!true", 0),
            TestCase::new("!false", 1),
            TestCase::new("!5", 0),
            TestCase::new("!!true", 1),
            TestCase::new("!!false", 0),
            TestCase::new("!!5", 1),
        ];
        for tt in tests.iter() {
            let l = lexer::new(tt.input);
            let mut p = parser::new(l);
            let program = p.parse_program().unwrap();
            let mut comp = compiler::new();
            comp.compile_program(program).unwrap();
            let mut machine = vm::new(comp.module).unwrap();
            let result = machine.run(compiler::ENTRY_POINT).unwrap();
            assert_eq!(result, tt.expected);
        }
    }
    #[test]
    fn test_if_else_expr() {
        let tests = vec![
            TestCase::new("if(true){10}", 10),
            TestCase::new("if(false){10}", 0),
            TestCase::new("if(1){10}", 10),
            TestCase::new("if(1<2){10}", 10),
            TestCase::new("if(1>2){10}", 0),
            TestCase::new("if(1>2){10}else{20}", 20),
            TestCase::new("if(1<2){10}else{20}", 10),
        ];
        for tt in tests.iter() {
            let l = lexer::new(tt.input);
            let mut p = parser::new(l);
            let program = p.parse_program().unwrap();
            let mut comp = compiler::new();
            comp.compile_program(program).unwrap();
            let mut machine = vm::new(comp.module).unwrap();
            let result = machine.run(compiler::ENTRY_POINT).unwrap();
            assert_eq!(result, tt.expected);
        }
    }
    #[test]
    fn test_return_stmts() {
        let tests = vec![
            TestCase::new("return 10;", 10),
            TestCase::new("return 10;9", 10),
            TestCase::new("return 2*5;9", 10),
            TestCase::new("9;return 2*5;9", 10),
            TestCase::new(
                r#"
            if(10>1){
                if(10>1){
                    return 10;
                }
                return 1;
            }
            "#,
                10,
            ),
        ];
        for tt in tests.iter() {
            let l = lexer::new(tt.input);
            let mut p = parser::new(l);
            let program = p.parse_program().unwrap();
            let mut comp = compiler::new();
            comp.compile_program(program).unwrap();
            let mut machine = vm::new(comp.module).unwrap();
            let result = machine.run(compiler::ENTRY_POINT).unwrap();
            assert_eq!(result, tt.expected);
        }
    }

    #[test]
    fn test_let_stmts() {
        let tests = vec![
            TestCase::new("let a=5;a;", 5),
            // TestCase::new("let a=5*5;a;", 25),
            // TestCase::new("let a=5;let b=a; b;", 5),
            // TestCase::new("let a=5;let b=a; let c= a+b+5;c;", 15),
        ];
        for tt in tests.iter() {
            let l = lexer::new(tt.input);
            let mut p = parser::new(l);
            let program = p.parse_program().unwrap();
            let mut comp = compiler::new();
            comp.compile_program(program).unwrap();
            let mut machine = vm::new(comp.module).unwrap();
            let result = machine.run(compiler::ENTRY_POINT).unwrap();
            assert_eq!(result, tt.expected);
        }
    }

    // #[test]
    fn test_func() {
        let tests = vec![TestCase::new(
            "let identity = fn(x) { x; }; identity(5);",
            5,
        )];
        for tt in tests.iter() {
            let l = lexer::new(tt.input);
            let mut p = parser::new(l);
            let program = p.parse_program().unwrap();
            let mut comp = compiler::new();
            comp.compile_program(program).unwrap();
            let mut machine = vm::new(comp.module).unwrap();
            let result = machine.run(compiler::ENTRY_POINT).unwrap();
            assert_eq!(result, tt.expected);
        }
    }
}
