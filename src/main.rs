mod parser;

fn main() {
    let input = "func strconcat(a string, b string) string";
    let mut lexer = parser::lexer::Lexer::new(input);
    let tokens = lexer.lex();
    let mut parser = parser::parser::Parser::new(&tokens);
    let func = parser.func();
    println!("{:?}", func);
    parser.errors().into_iter().for_each(|e| println!("Error: {:?}", e));
}
