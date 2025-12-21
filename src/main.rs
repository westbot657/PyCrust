pub mod core;

#[cfg(feature="lexer")]
pub mod lexer;

#[cfg(feature="parser")]
pub mod parser;

fn main() {
    println!("Hello, world!");
}
