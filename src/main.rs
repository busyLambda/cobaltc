mod parser;

fn main() {
    let input = "func add(a int, b int)";
    let mut lexer = parser::lexer::Lexer::new(input);
    let tokens = lexer.lex();
    let mut parser = parser::parser::Parser::new(&tokens);
    let func = parser.func();
    println!("{:?}", func);
    parser.errors().into_iter().for_each(|e| println!("Error: {:?}", e));
}
