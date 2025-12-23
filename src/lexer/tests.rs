use anyhow::Result;
use crate::lexer::lexer::Lexer;

#[test]
pub fn test_lexer() -> Result<()> {

    let source = include_str!("../../test_scripts/lexer_test.py").to_string();

    let mut lexer = Lexer::new(source);

    lexer.tokenize()?;

    println!("output tokens: {}", lexer.get_tokens());

    Ok(())
}

