use anyhow::{anyhow, Result};
use crate::lexer::lexer::{Lexer, LexerContext};
use crate::lexer::unescape::unescape;
use crate::parser::node::{FileNode, Node};

fn do_unescape(msg: &str, literal: &str) -> Result<()> {
    let mut ctx = LexerContext::new("<test-unescape>".to_string());

    let s = unescape(msg, &mut ctx, true).map_err(|e| anyhow!("{e}"))?;

    println!("Got escaped string: {msg} as {s}");

    assert_eq!(s, literal);

    Ok(())
}

#[test]
pub fn test_unescape() -> Result<()> {

    do_unescape("Hello,\\nWorld!\\ttab 1\\ttab 2\\r\\nwindows crlf\\r\\n", "Hello,\nWorld!\ttab 1\ttab 2\r\nwindows crlf\r\n")?;
    do_unescape("\\377", &format!("{}", 0o377 as char))?;
    do_unescape("\\u1234", "\u{1234}")?;
    do_unescape("\\U00012345", "\u{012345}")?;
    do_unescape("\\bBackspace", "\u{0008}Backspace")?;
    do_unescape("\\x7F Hex code", "\x7F Hex code")?;
    Ok(())
}

#[test]
pub fn test_lexer() -> Result<()> {

    let source = include_str!("../../test_scripts/lexer_test.py").to_string();


    let mut lexer = Lexer::new("<python-test>".to_string(), source);

    lexer.tokenize()?;

    println!("output tokens: {}", lexer.get_tokens());

    Ok(())
}

#[test]
pub fn test_parser() -> Result<()> {

    // let source = include_str!("../../test_scripts/lexer_test.py").to_string();

    let source = "f'{self.a, self.b=:{self.c}}'\n".to_string();

    let mut lexer = Lexer::new("<python-test>".to_string(), source);

    lexer.tokenize()?;
    // println!("output tokens: {}", lexer.get_tokens());

    let mut tokens = lexer.to_tokens().into_parse_tokens();

    let ast = FileNode::parse_debug(&mut tokens, false)?;

    println!("Output AST: {ast:#?}");

    Ok(())

}

