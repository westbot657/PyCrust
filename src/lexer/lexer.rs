use crate::core::types::{Comparator, Keyword, Number, NumericValue, Operator, Symbol, TextPosition, TextSpan, Token, TokenValue, Tokens};
use anyhow::{anyhow, Result};
use regex::Regex;
use crate::lexer::unescape::unescape;


pub struct LexerResult {
    warnings: Vec<String>,
    error: Option<String>,
}
pub struct LexerContext {
    warnings: Vec<String>,
    error: Option<String>,
    pub position: TextPosition,
    file_name: String,
}

pub struct Lexer {
    source: String,
    position: TextPosition,
    tokens: Tokens,
    file_name: String,
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

impl LexerResult {

    pub fn has_error(&self) -> bool {
        self.error.is_some()
    }

    pub fn has_warnings(&self) -> bool {
        !self.warnings.is_empty()
    }

    pub fn get_error(&self) -> Option<&String> {
        if let Some(s) = &self.error {
            Some(&s)
        } else {
            None
        }
    }

    pub fn get_warnings(&self) -> &Vec<String> {
        &self.warnings
    }
}

impl LexerContext {
    pub fn new(file_name: String) -> Self {
        Self {
            warnings: Vec::new(),
            error: None,
            position: TextPosition::zero(),
            file_name,
        }
    }

    pub fn add_warning(&mut self, warning: String) {
        self.warnings.push(warning)
    }

    pub fn set_error(&mut self, error: String) {
        self.error = Some(error)
    }

    pub fn clear_error(&mut self) {
        self.error = None;
    }

    fn to_result(self) -> LexerResult {
        LexerResult {
            warnings: self.warnings,
            error: self.error,
        }
    }

}

impl Lexer {

    pub fn get_tokens(&self) -> &Tokens {
        &self.tokens
    }

    pub fn new(file_name: String, source: String) -> Self {
        Self {
            source,
            position: TextPosition::zero(),
            tokens: Tokens::new(),
            file_name,
        }
    }

    pub fn tokenize(&mut self) -> Result<LexerResult> {
        let source = self.source.clone();
        self.position.reset();

        let mut context = LexerContext::new(self.file_name.clone());

        self.lex_normal(&source, &mut context)?;

        Ok(context.to_result())
    }

    pub fn get_line_by_number(&self, line: usize) -> Option<&str> {
        self.source.lines().nth(line-1)
    }

    pub fn get_error_squiggle(start: usize, length: usize) -> String {
        " ".repeat(start) + &"^".repeat(length)
    }

    pub fn get_offset_pos(&self, pos: &TextPosition, offset: usize) -> TextPosition {
        let mut pos = pos.clone();
        let i = pos.index;
        pos.advance(&self.source[i..i+offset]);
        pos
    }

    fn lex_normal(&mut self, source: &str, context: &mut LexerContext) -> Result<()> {

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
                (?: 0[bB]_?[01](?:_?[01]+)*
                |   0[oO]_?[0-7](?:_?[0-7]+)*
                |   0[xX]_?[0-9a-fA-F](?:_?[0-9a-fA-F]+)*
                |   (?: [1-9] (?:_?\d+)* (?:\. (?:\d(?:_?\d+)*)? )?
                    |   0 (?:\. (?: \d(?:_?\d+)*)? )?
                    |   \. \d(?:_?\d+)*
                    ) (?:[eE][+\-]?\d(?:_?\d+)*)? j?
                )
            "#
        )?;
        let binary_num_re = Regex::new(r#"^0[bB](?<bin>[01]+)"#)?;
        let octal_num_re = Regex::new(r#"^0[oO](?<oct>[0-7]+)"#)?;
        let hex_num_re = Regex::new(r#"^0[xX](?<hex>[0-9a-fA-F]+)"#)?;
        let numeric_re = Regex::new(
            r#"(?x)^
                (?<real> [1-9]\d* (?:\. (?:\d*)? )?
                |        0 (?:\. (?:\d*)? )?
                |        \. \d+
                )
                (?: [eE] (?<exp> [+\-]?\d+ ) )?
                j?
            "#
        )?;

        let word_re = Regex::new(r#"(?x)^[a-zA-Z_][a-zA-Z0-9_]*\b"#)?;
        let newlines_re = Regex::new(r#"(?x)^\n\s*"#)?;
        let space_re = Regex::new(r#"^ +"#)?;

        let string_re = Regex::new(
            r#"(?x)^(?:
                "(?:\\.|[^"])*"
                | '(?:\\.|[^'])*'
            )"#
        )?;

        let length = source.len();

        while self.position.index < length {
            let code = &source[self.position.index..];
            let pos = self.position.clone();
            if let Some(after) = code.strip_prefix("f'''") {
                self.position.increment(4);
                let mut tokens = self.lex_f_string(StringDelimiter::F1x3, after, true)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("f\"\"\"") {
                self.position.increment(4);
                let mut tokens = self.lex_f_string(StringDelimiter::F2x3, after, true)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("f'") {
                self.position.increment(2);
                let mut tokens = self.lex_f_string(StringDelimiter::F1x1, after, true)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("f\"") {
                self.position.increment(2);
                let mut tokens = self.lex_f_string(StringDelimiter::F2x1, after, true)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("rf'''").or_else(|| code.strip_prefix("fr'''")) {
                self.position.increment(5);
                let mut tokens = self.lex_f_string(StringDelimiter::F1x3, after, false)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("rf\"\"\"").or_else(|| code.strip_prefix("fr\"\"\"")) {
                self.position.increment(5);
                let mut tokens = self.lex_f_string(StringDelimiter::F2x3, after, false)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("rf'").or_else(|| code.strip_prefix("fr'")) {
                self.position.increment(3);
                let mut tokens = self.lex_f_string(StringDelimiter::F1x3, after, false)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("rf\"").or_else(|| code.strip_prefix("fr\"")) {
                self.position.increment(3);
                let mut tokens = self.lex_f_string(StringDelimiter::F2x3, after, false)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("r'''") {
                self.position.increment(5);
                let mut tokens = self.lex_string(StringDelimiter::F1x3, after, true)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("r\"\"\"") {
                self.position.increment(5);
                let mut tokens = self.lex_string(StringDelimiter::F2x3, after, true)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("r'") {
                self.position.increment(3);
                let mut tokens = self.lex_string(StringDelimiter::F1x3, after, true)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("r\"") {
                self.position.increment(3);
                let mut tokens = self.lex_string(StringDelimiter::F2x3, after, true)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("rb'''").or_else(|| code.strip_prefix("br'''")) {
                self.position.increment(5);
                let mut tokens = self.lex_b_string(StringDelimiter::F1x3, after, false)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("rb\"\"\"").or_else(|| code.strip_prefix("br\"\"\"")) {
                self.position.increment(5);
                let mut tokens = self.lex_b_string(StringDelimiter::F2x3, after, false)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("rb'").or_else(|| code.strip_prefix("br'")) {
                self.position.increment(3);
                let mut tokens = self.lex_b_string(StringDelimiter::F1x3, after, false)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("rb\"").or_else(|| code.strip_prefix("br\"")) {
                self.position.increment(3);
                let mut tokens = self.lex_b_string(StringDelimiter::F2x3, after, false)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("b'''") {
                self.position.increment(5);
                let mut tokens = self.lex_b_string(StringDelimiter::F1x3, after, true)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("b\"\"\"") {
                self.position.increment(5);
                let mut tokens = self.lex_b_string(StringDelimiter::F2x3, after, true)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("b'") {
                self.position.increment(3);
                let mut tokens = self.lex_b_string(StringDelimiter::F1x3, after, true)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(after) = code.strip_prefix("b\"") {
                self.position.increment(3);
                let mut tokens = self.lex_b_string(StringDelimiter::F2x3, after, true)?;
                self.tokens.append(&mut tokens);
            }
            else if let Some(string) = string_re.captures(code) {
                let string = &code[string.get(0).unwrap().range()];
                context.position = self.position.clone();
                self.position.advance(string);
                let string_res = unescape(&string[1..string.len()-1], context);
                match string_res {
                    Ok(s) => {
                        let s = TokenValue::StringLiteral(s);
                        self.tokens.push(Token::new(s, pos.span_to(&self.position), string))
                    }
                    Err((e, p, l)) => {
                        let pos = self.get_offset_pos(&pos, p);
                        let err = format!("File \"{}\", line {}\n{}\n{}\n{}", context.file_name, pos.line, self.get_line_by_number(pos.line).unwrap(), Self::get_error_squiggle(pos.column, l), e);
                        context.set_error(err.clone());
                        return Err(anyhow!("{}", err));
                    }
                }
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

                let val = if let Some(bin) = binary_num_re.captures(&num_txt) {
                    let raw = bin.name("bin").unwrap().as_str();
                    let num = i64::from_str_radix(raw, 2)?;
                    Token::new(TokenValue::NumberLiteral(Number::Real(NumericValue::I64(num))), pos.span_to(&self.position), raw_txt)
                }
                else if let Some(oct) = octal_num_re.captures(&num_txt) {
                    let raw = oct.name("oct").unwrap().as_str();
                    let num = i64::from_str_radix(raw, 8)?;
                    Token::new(TokenValue::NumberLiteral(Number::Real(NumericValue::I64(num))), pos.span_to(&self.position), raw_txt)
                }
                else if let Some(hex) = hex_num_re.captures(&num_txt) {
                    let raw = hex.name("hex").unwrap().as_str();
                    let num = i64::from_str_radix(raw, 16)?;
                    Token::new(TokenValue::NumberLiteral(Number::Real(NumericValue::I64(num))), pos.span_to(&self.position), raw_txt)
                }
                else if let Some(num) = numeric_re.captures(&num_txt) {
                    let real = num.name("real").unwrap().as_str();
                    let exponent = num.name("exp").map(|o| o.as_str());
                    let imaginary = num_txt.ends_with("j");

                    let mut val = if real.contains(".") {
                        NumericValue::F64(real.parse::<f64>()?)
                    } else {
                        NumericValue::I64(real.parse::<i64>()?)
                    };

                    if let Some(exp) = exponent {
                        val *= 10f64.powi(exp.parse::<i32>()?);
                    }

                    let val = if imaginary {
                        Number::Complex { real: 0.into(), imaginary: val }
                    } else {
                        Number::Real(val)
                    };

                    Token::new(TokenValue::NumberLiteral(val), pos.span_to(&self.position), raw_txt)
                } else {
                    unreachable!("ERROR: number pattern else branch was reached!! ({} => {})", raw_txt, num_txt)
                };

                self.tokens.push(val);

            }
            else if code.starts_with("#") {
                if let Some(len) = code.find("\n") {
                    let slice = &code[0..len];
                    self.position.advance(slice);
                } else {
                    return Ok(()) // if there's no next \n then it's the end of file
                }
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
            else if let Some(txt) = code.splice_start("...", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Ellipsis, pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(".", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::Dot), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(",", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::Comma), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(":=", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::Walrus), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(":", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::Symbol(Symbol::Colon), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("\\", &mut self.position) {
                self.tokens.push(Token::new(TokenValue::LineContinuation, pos.span_to(&self.position), txt))
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
                let chr = &code.chars().nth(0).unwrap();
                self.position.increment(1);
                return Err(anyhow!("Unexpected character '{}' at {}", chr, self.position))
            }

        }

        Ok(())
    }

    fn lex_f_string(&mut self, delimiter: StringDelimiter, current: &str, do_unescapes: bool) -> Result<Tokens> {
        todo!()
    }

    fn lex_string(&mut self, delimiter: StringDelimiter, current: &str, do_unescapes: bool) -> Result<Tokens> {
        todo!()
    }

    fn lex_b_string(&mut self, delimiter: StringDelimiter, current: &str, do_unescapes: bool) -> Result<Tokens> {
        todo!()
    }

}

