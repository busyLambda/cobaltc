mod parser;

fn main() {
    let input = "func filter(a [int]) [int]";
    let mut lexer = parser::lexer::Lexer::new(input);
    let tokens = lexer.lex();
    let mut parser = parser::Parser::new(&tokens);
    let func = parser.func().unwrap();
    println!("{:?}", func)
}
