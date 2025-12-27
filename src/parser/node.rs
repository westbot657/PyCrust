use anyhow::Result;
use crate::core::types::Tokens;
use crate::parser::statement::*;

pub trait Node {
    fn parse(tokens: &mut Tokens, include_invalid_rules: bool) -> Result<Box<Self>>;
}



pub struct FileNode {
    statements: StatementsNode,
}


