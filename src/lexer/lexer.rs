use crate::core::types::{Comparator, Keyword, Number, Operator, Symbol, TextPosition, TextSpan, Token, TokenValue, Tokens};
use anyhow::{anyhow, Result};
use regex::Regex;

pub struct Lexer {
    source: String,
    position: TextPosition,
    tokens: Tokens,
}

pub enum StringDelimiter {
    F1x3, // '''
    F2x3, // """
    F1x1, // '
    F2x1, // "
}

trait StrSplice {
    fn splice_start(&self, text: &str, pos: &mut TextPosition) -> Option<&str>;
}

impl StrSplice for str {
    fn splice_start(&self, text: &str, pos: &mut TextPosition) -> Option<&str> {
        if self.starts_with(text) {
            let tref = &self[0..text.len()];
            pos.advance(tref);
            Some(tref)
        } else {
            None
        }
    }
}


impl Lexer {

    pub fn get_tokens(&self) -> &Tokens {
        &self.tokens
    }

    pub fn new(source: String) -> Self {
        Self {
            source,
            position: TextPosition::zero(),
            tokens: Tokens::new(),
        }
    }

    pub fn tokenize(&mut self) -> Result<()> {
        let source = self.source.clone();
        self.position.reset();

        self.lex_normal(&source)
    }

    fn lex_normal(&mut self, source: &str) -> Result<()> {

        let keyword_re = Regex::new(
            r#"(?x)^
                (?:continue|finally|nonlocal|lambda|global|import|return|
                break|class|while|yield|except|del|from|raise|elif|else|
                pass|with|try|for|and|not|def|if|in|is|as|or|True|False|None)
                \b
            "#
        )?;

        let number_re = Regex::new(
            r#"(?x)^
                (?: [1-9] (?:_?\d+)* (?:\. (?:\d+_?)+ )?
                    | 0 (?:\. (?:\d+_?)+ )?
                    | \. (?:\d+_?)+ )
            "#
        )?;

        let word_re = Regex::new(r#"(?x)^[a-zA-Z_][a-zA-Z0-9_]*\b"#)?;
        let newlines_re = Regex::new(r#"(?x)^\n\s*"#)?;
        let space_re = Regex::new(r#"^ +"#)?;

        let string_re = Regex::new(
            r#"(?x)^(?:
                "(?:\\.|[^"])"
                | '(?:\\.|[^'])'
            )"#
        )?;

        let length = source.len();

        while self.position.index < length {
            let code = &source[self.position.index..];
            let pos = self.position.clone();
            if let Some(after) = code.strip_prefix("f'''") {
                self.position.increment(4);
                let mut tokens = self.lex_f_string(StringDelimiter::F1x3, after)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("f\"\"\"") {
                self.position.increment(4);
                let mut tokens = self.lex_f_string(StringDelimiter::F2x3, after)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("f'") {
                self.position.increment(2);
                let mut tokens = self.lex_f_string(StringDelimiter::F1x1, after)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("f\"") {
                self.position.increment(2);
                let mut tokens = self.lex_f_string(StringDelimiter::F2x1, after)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(string) = string_re.captures(code) {

            }
            else if let Some(keyword) = keyword_re.captures(code) {
                let kw = &code[keyword.get(0).unwrap().range()];
                self.position.advance(kw);
                if kw == "True" {
                    self.tokens.push(Token::new(TokenValue::BooleanLiteral(true), pos.span_to(&self.position), kw))
                } else if kw == "False" {
                    self.tokens.push(Token::new(TokenValue::BooleanLiteral(false), pos.span_to(&self.position), kw))
                } else if kw == "None" {
                    self.tokens.push(Token::new(TokenValue::None, pos.span_to(&self.position), kw))
                } else {
                    let tk = Keyword::from_str(kw).unwrap();
                    self.tokens.push(Token::new(TokenValue::Keyword(tk), pos.span_to(&self.position), kw));
                }
            }
            else if let Some(word) = word_re.captures(code) {
                let w = &code[word.get(0).unwrap().range()];
                self.position.advance(w);
                self.tokens.push(Token::new(TokenValue::Word(w.to_string()), pos.span_to(&self.position), w))
            }
            else if let Some(number) = number_re.captures(code) {
                let raw_txt = &code[number.get(0).unwrap().range()];
                self.position.advance(raw_txt);
                let num_txt = (raw_txt).replace("_", "");
                let num = if num_txt.contains(".") {
                    Number::F64(num_txt.parse::<f64>()?)
                } else {
                    Number::I64(num_txt.parse::<i64>()?)
                };
                self.tokens.push(Token::new(TokenValue::NumberLiteral(num), pos.span_to(&self.position), raw_txt))
            }
            else if let Some(txt) = code.splice_start("->", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::Arrow), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(">>=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::Rsh), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(">>", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::Rsh), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("<<=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::Lsh), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("<<", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::Lsh), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(">=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Comparator(Comparator::Ge), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(">", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Comparator(Comparator::Gt), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("<=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Comparator(Comparator::Le), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("<", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Comparator(Comparator::Lt), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("==", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Comparator(Comparator::Eq), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("!=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Comparator(Comparator::Ne), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::Assign), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("**=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::Pow), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("**", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::Pow), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("//=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::Floor), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("//", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::Floor), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("+=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::Add), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("+", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::Add), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("-=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::Sub), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("-", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::Sub), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("*=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::Mul), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("*", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::Mul), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("/=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::Div), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("/", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::Div), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("%=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::Mod), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("%", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::Mod), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("|=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::BitOr), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("|", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::BitOr), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("&=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::BitAnd), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("&", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::BitAnd), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("^=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::BitXor), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("^", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::BitXor), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("~=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::AssignOperator(Operator::BitNot), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("~", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Operator(Operator::BitNot), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("(", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::LParen), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(")", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::RParen), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("[", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::LBracket), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("]", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::RBracket), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("{", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::LBrace), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("}", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::RBrace), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("@", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::Decorator), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(".", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::Dot), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(",", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::Comma), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(":", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::Colon), pos.span_to(&self.position), txt))
            }
            else if let Some(n) = newlines_re.captures(code) {
                let raw = &code[n.get(0).unwrap().range()];
                self.position.advance(raw);
                self.tokens.push(Token::new(TokenValue::LeadingWhitespace, pos.span_to(&self.position), raw))
            }
            else if let Some(s) = space_re.captures(code) {
                self.position.advance(&code[s.get(0).unwrap().range()])
            }
            else {
                let chr = &code[0..1];
                return Err(anyhow!("Unexpected character '{}' at {}", chr, self.position))
            }

        }

        Ok(())
    }

    fn lex_f_string(&mut self, delimiter: StringDelimiter, current: &str) -> Result<Tokens> {
        todo!()
    }

}

