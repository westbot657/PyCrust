use crate::core::types::{TextPosition, Token};
use anyhow::Result;

pub struct Lexer {
    source: String,
    position: TextPosition,
    tokens: Vec<Token>,
}

pub enum StringDelimiter {
    F1x3, // '''
    F2x3, // """
    F1x1, // '
    F2x1, // "
}

impl Lexer {

    pub fn new(source: String) -> Self {
        Self {
            source,
            position: TextPosition::zero(),
            tokens: Vec::new(),
        }
    }

    pub fn tokenize(&mut self) -> Result<()> {
        let source = self.source.clone();
        let length = source.len();

        while self.position.index < length {
            let remaining = &source[self.position.index..];
            if let Some(after) = remaining.strip_prefix("f'''") {
                let token = self.lex_f_string(StringDelimiter::F1x3, after)?;
            }
            else if let Some(after) = remaining.strip_prefix("f\"\"\"") {
                let token = self.lex_f_string(StringDelimiter::F2x3, after)?;
            }
        }

        Ok(())
    }

    fn lex_f_string(&mut self, delimiter: StringDelimiter, current: &str) -> Result<(Vec<Token>, &str)> {
        todo!()
    }

}

