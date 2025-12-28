use std::collections::VecDeque;
use std::mem;
use crate::core::types::{Comparator, Keyword, Number, NumericValue, Operator, PartialFString, Symbol, TextPosition, TextSpan, Token, TokenValue, Tokens};
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
    pub file_name: String,
}

pub struct Lexer {
    source: String,
    position: TextPosition,
    tokens: Tokens,
    file_name: String,
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

trait IndentTracker {
    fn set_indent(&mut self, indent: usize) -> Result<Vec<TokenValue>, String>;
}

impl IndentTracker for VecDeque<usize> {
    fn set_indent(&mut self, indent: usize) -> Result<Vec<TokenValue>, String> {
        let mut delta = Vec::new();
        if self.is_empty() {
            delta.push(TokenValue::Indent);
            self.push_back(indent);
        } else {
            let cur = *self.back().unwrap();
            if cur < indent {
                delta.push(TokenValue::Indent);
                self.push_back(indent);
            } else if cur > indent {
                while let Some(cur) = self.back() {
                    if *cur > indent {
                        self.pop_back();
                        delta.push(TokenValue::Dedent)
                    } else if *cur < indent {
                        return Err("IndentationError: unindent does not match any outer indentation level".to_string())
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(delta)
    }
}

impl Lexer {

    pub fn get_tokens(&self) -> &Tokens {
        &self.tokens
    }

    pub fn new(file_name: String, source: String) -> Self {

        let source = if !source.ends_with("\n") {
            source + "\n"
        } else {
            source
        };

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

        self.tokens = self.lex_normal(&source, &mut context, 0, false)?;

        self.cleanup_tokens()?;

        self.tokens.push(Token::new(TokenValue::EndMarker, self.position.span_to(&self.position), ""));

        Ok(context.to_result())
    }

    pub fn get_line_by_number(&self, line: usize) -> Option<&str> {
        self.source.lines().nth(line-1)
    }

    pub fn get_error_squiggle(start: usize, length: usize) -> String {
        " ".repeat(start-1) + &"^".repeat(length)
    }

    pub fn get_offset_pos(&self, pos: &TextPosition, offset: usize) -> TextPosition {
        let mut pos = pos.clone();
        let i = pos.index;
        pos.advance(&self.source[i..i+offset]);
        pos
    }

    fn clip_leading_whitespace(token: &mut Token) -> bool {
        let newline_count = token.content.matches('\n').count();

        if newline_count == 1 {
            return false;
        }

        let bytes = token.content.as_bytes();
        let mut cut = 1;

        while cut < bytes.len() && (bytes[cut] == b' ' || bytes[cut] == b'\t') {
            cut += 1;
        }

        token.content.drain(..cut);
        true
    }

    fn get_last_line_indent(s: &str) -> usize {
        let last_line = s.rsplit('\n').next().unwrap_or(s);

        let mut indent = 0;

        for ch in last_line.chars() {
            match ch {
                ' ' => indent += 1,
                '\t' => indent += 4,
                _ => break,
            }
        }

        indent
    }

    fn handle_indentation(&mut self, mut n: &mut Token, indentation: &mut VecDeque<usize>, tokens: &mut Tokens) -> Result<()> {
        let new_indent = Self::get_last_line_indent(&n.content);
        match indentation.set_indent(new_indent) {
            Ok(delta) => {
                for val in delta {
                    tokens.push(Token::new(val, n.span.clone(), &n.content))
                }
            }
            Err(e) => {
                let line = self.get_line_by_number(n.span.end.line).unwrap();
                let squiggle = Self::get_error_squiggle(n.span.end.column, 1);
                return Err(anyhow!("  File \"{}\", line {}\n{}\n{}\n{}", self.file_name, n.span.end.line, line, squiggle, e));
            }
        }
        Ok(())
    }


    fn cleanup_tokens(&mut self) -> Result<()> {

        let mut toks = Tokens::new();
        mem::swap(&mut toks, &mut self.tokens);
        let mut iter = toks.into_iter().peekable();

        let mut tokens = Tokens::new();

        let mut indentation = VecDeque::new();
        indentation.push_back(0usize);

        loop {
            let Some(mut tok) = iter.next() else {
                break;
            };

            match &mut tok {
                Token { value: TokenValue::LineContinuation, .. } => {
                    let Some(n) = iter.peek_mut() else {
                        return Err(anyhow!("Unexpected EOF after '\\'"))
                    };
                    match n {
                        Token { value: TokenValue::LeadingWhitespace, .. } => {
                            if Self::clip_leading_whitespace(n) {
                                tokens.push(Token::new(TokenValue::Newline, n.span.clone(), "\n"));
                                self.handle_indentation(n, &mut indentation, &mut tokens)?
                            }
                        }
                        _ => return Err(anyhow!("Expected newline after '\\', got token: {n:?}" ))
                    }
                }
                Token { value: TokenValue::LeadingWhitespace, .. } => {
                    tokens.push(Token::new(TokenValue::Newline, tok.span.clone(), "\n"));
                    self.handle_indentation(&mut tok, &mut indentation, &mut tokens)?
                }
                Token { value: TokenValue::StringLiteral(_), .. } => {
                    while let Some(_) = iter.peek() {
                        if !tok.str_concat(&mut iter).map_err(|e| anyhow!("{e}"))? {
                            break;
                        }
                    }
                    tokens.push(tok);
                }
                Token { value: TokenValue::FString(_), .. } => {
                    while let Some(_) = iter.peek() {
                        if !tok.str_concat(&mut iter).map_err(|e| anyhow!("{e}"))? {
                            break;
                        }
                    }
                    tokens.push(tok);
                }
                Token { value: TokenValue::BytesLiteral(_), .. } => {
                    while let Some(_) = iter.peek() {
                        if !tok.byte_concat(&mut iter).map_err(|e| anyhow!("{e}"))? {
                            break
                        }
                    }
                    tokens.push(tok)
                }
                _ => {
                    tokens.push(tok)
                }
            }

        }

        self.tokens = tokens;
        Ok(())
    }


    fn lex_normal(&mut self, source: &str, context: &mut LexerContext, index_offset: usize, in_f_string: bool) -> Result<Tokens> {

        let mut tokens = Tokens::new();

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

        let prefix_str_re = Regex::new(
            r#"(?x)^
                (?:rf?|fr?|br?|rb)?(?:"(?:"")?|'(?:'')?)
            "#
        )?;

        let length = source.len();

        let mut brace_depth = 0u32;



        while self.position.index - index_offset < length {
            let code = &source[(self.position.index - index_offset)..];
            let pos = self.position.clone();
            if let Some(prefix) = prefix_str_re.captures(code) {
                let token = self.lex_string(prefix.get(0).unwrap().as_str(), code, pos, context)?;
                tokens.push(token);
            }
            else if let Some(keyword) = keyword_re.captures(code) {
                let kw = &code[keyword.get(0).unwrap().range()];
                self.position.advance(kw);
                if kw == "True" {
                    tokens.push(Token::new(TokenValue::BooleanLiteral(true), pos.span_to(&self.position), kw))
                } else if kw == "False" {
                    tokens.push(Token::new(TokenValue::BooleanLiteral(false), pos.span_to(&self.position), kw))
                } else if kw == "None" {
                    tokens.push(Token::new(TokenValue::None, pos.span_to(&self.position), kw))
                } else {
                    let tk = Keyword::from_str(kw).unwrap();
                    tokens.push(Token::new(TokenValue::Keyword(tk), pos.span_to(&self.position), kw));
                }
            }
            else if let Some(word) = word_re.captures(code) {
                let w = &code[word.get(0).unwrap().range()];
                self.position.advance(w);
                tokens.push(Token::new(TokenValue::Word(w.to_string()), pos.span_to(&self.position), w))
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

                tokens.push(val);

            }
            else if code.starts_with("#") {
                if let Some(len) = code.find("\n") {
                    let slice = &code[0..len];
                    self.position.advance(slice);
                } else {
                    return Ok(tokens) // if there's no next \n then it's the end of file
                }
            }
            else if let Some(txt) = code.splice_start("->", &mut self.position) {
                tokens.push(Token::new(TokenValue::Symbol(Symbol::Arrow), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(">>=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::Rsh), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(">>", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::Rsh), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("<<=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::Lsh), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("<<", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::Lsh), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(">=", &mut self.position) {
                tokens.push(Token::new(TokenValue::Comparator(Comparator::Ge), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(">", &mut self.position) {
                tokens.push(Token::new(TokenValue::Comparator(Comparator::Gt), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("<=", &mut self.position) {
                tokens.push(Token::new(TokenValue::Comparator(Comparator::Le), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("<", &mut self.position) {
                tokens.push(Token::new(TokenValue::Comparator(Comparator::Lt), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("==", &mut self.position) {
                tokens.push(Token::new(TokenValue::Comparator(Comparator::Eq), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("!=", &mut self.position) {
                tokens.push(Token::new(TokenValue::Comparator(Comparator::Ne), pos.span_to(&self.position), txt))
            }
            else if code.starts_with("=") {
                if in_f_string && brace_depth == 0 {
                    return Ok(tokens)
                } else {
                    let tref = &code[0..1];
                    self.position.increment(1);
                    tokens.push(Token::new(TokenValue::Symbol(Symbol::Assign), pos.span_to(&self.position), tref))
                }
            }
            else if let Some(txt) = code.splice_start("**=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::Pow), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("**", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::Pow), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("//=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::Floor), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("//", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::Floor), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("+=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::Add), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("+", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::Add), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("-=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::Sub), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("-", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::Sub), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("*=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::Mul), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("*", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::Mul), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("/=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::Div), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("/", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::Div), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("%=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::Mod), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("%", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::Mod), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("|=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::BitOr), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("|", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::BitOr), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("&=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::BitAnd), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("&", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::BitAnd), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("^=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::BitXor), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("^", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::BitXor), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("~=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::BitNot), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("~", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::BitNot), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("(", &mut self.position) {
                brace_depth += 1;
                tokens.push(Token::new(TokenValue::Symbol(Symbol::LParen), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(")", &mut self.position) {
                if brace_depth == 0 {
                    return Err(anyhow!("Unbalanced ')'"))
                } else {
                    brace_depth -= 1;
                }
                tokens.push(Token::new(TokenValue::Symbol(Symbol::RParen), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("[", &mut self.position) {
                brace_depth += 1;
                tokens.push(Token::new(TokenValue::Symbol(Symbol::LBracket), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("]", &mut self.position) {
                if brace_depth == 0 {
                    return Err(anyhow!("Unbalanced ']'"))
                } else {
                    brace_depth -= 1;
                }
                tokens.push(Token::new(TokenValue::Symbol(Symbol::RBracket), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("{", &mut self.position) {
                brace_depth += 1;
                tokens.push(Token::new(TokenValue::Symbol(Symbol::LBrace), pos.span_to(&self.position), txt))
            }
            else if code.starts_with("}") {
                if brace_depth == 0 {
                    return if in_f_string {
                        Ok(tokens)
                    } else {
                        Err(anyhow!("Unbalanced '}}'"))
                    }
                } else {
                    brace_depth -= 1;
                }
                let tref = &code[0..1];
                self.position.increment(1);
                tokens.push(Token::new(TokenValue::Symbol(Symbol::RBrace), pos.span_to(&self.position), tref))

            }
            else if let Some(txt) = code.splice_start("@=", &mut self.position) {
                tokens.push(Token::new(TokenValue::AssignOperator(Operator::MatMul), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("@", &mut self.position) {
                tokens.push(Token::new(TokenValue::Operator(Operator::MatMul), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start("...", &mut self.position) {
                tokens.push(Token::new(TokenValue::Ellipsis, pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(".", &mut self.position) {
                tokens.push(Token::new(TokenValue::Symbol(Symbol::Dot), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(",", &mut self.position) {
                tokens.push(Token::new(TokenValue::Symbol(Symbol::Comma), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(":=", &mut self.position) {
                tokens.push(Token::new(TokenValue::Symbol(Symbol::Walrus), pos.span_to(&self.position), txt))
            }
            else if let Some(txt) = code.splice_start(";", &mut self.position) {
                tokens.push(Token::new(TokenValue::Symbol(Symbol::Semicolon), pos.span_to(&self.position), txt))
            }
            else if code.starts_with(":") {
                if in_f_string && brace_depth == 0 {
                    return Ok(tokens)
                } else {
                    let tref = &code[0..1];
                    self.position.increment(1);
                    tokens.push(Token::new(TokenValue::Symbol(Symbol::Colon), pos.span_to(&self.position), tref))
                }
            }
            else if in_f_string && brace_depth == 0 && code.starts_with("!") {
                return Ok(tokens)
            }
            else if let Some(txt) = code.splice_start("\\", &mut self.position) {
                if brace_depth == 0 {
                    tokens.push(Token::new(TokenValue::LineContinuation, pos.span_to(&self.position), txt))
                }
            }
            else if let Some(n) = newlines_re.captures(code) {
                let raw = &code[n.get(0).unwrap().range()];
                self.position.advance(raw);
                if brace_depth > 0 {
                    continue;
                }
                tokens.push(Token::new(TokenValue::LeadingWhitespace, pos.span_to(&self.position), raw))
            }
            else if let Some(s) = space_re.captures(code) {
                self.position.advance(&code[s.get(0).unwrap().range()])
            }
            else {
                self.position.increment(1);
                let err = format!("\n  File \"{}\", line {}\n{}\n{}\nSyntax Error: invalid syntax", context.file_name, pos.line, self.get_line_by_number(pos.line).unwrap(), Self::get_error_squiggle(pos.column, 1));
                context.set_error(err.clone());
                return Err(anyhow!("{}", err))
            }

        }

        Ok(tokens)
    }

    #[cfg(feature = "named-unicode")]
    fn translate_unicode_name(&self, name: &str, start_idx: usize, context: &mut LexerContext) -> Result<String> {
        unicode_names2::character(name)
            .map(|c| c.to_string())
            .ok_or_else(|| anyhow!(
                "  File \"{}\", line {}\n{}\n{}\nSyntaxError: (unicode error) 'unicodeescape' codec can't decode bytes in position {}-{}: unknown Unicode character name",
                context.file_name, self.position.line, self.get_line_by_number(self.position.line).unwrap(),
                Self::get_error_squiggle(self.position.column, name.len()), start_idx, start_idx + name.len()
        ))
    }

    #[cfg(not(feature = "named-unicode"))]
    fn translate_unicode_name(&self, name: &str, _start_idx: usize, context: &mut LexerContext) -> Result<String> {
        context.add_warning(format!("{}:{}: SyntaxWarning: pycrust feature not enabled: 'named-unicode'", context.file_name, self.position.line));
        Ok(format!("\\N{{{name}}}"))
    }

    fn lex_string(&mut self, prefix: &str, raw_code: &str, pos: TextPosition, context: &mut LexerContext) -> Result<Token> {
        let mut string = String::new();

        let is_raw = prefix.contains("r");
        let is_format = prefix.contains("f");
        let is_bytes = prefix.contains("b");
        let prefix_len: usize = if is_raw { 1 } else { 0 } + if is_format { 1 } else { 0 } + if is_bytes { 1 } else { 0 };
        let delimiter = &prefix[prefix_len..];
        let is_multiline = delimiter.len() == 3;

        self.position.advance(prefix);
        let code = &raw_code[prefix.len()..];

        if is_format { // NOTE: format strings are mutually exclusive to byte strings
            let relative = self.position.index;

            let mut parts = Vec::new();

            'f_str: loop {
                let start = self.position.index - relative;
                let slice = &code[start..];

                if slice.starts_with("\\{") {
                    string += "\\";
                    self.position.increment(1);
                }
                else if slice.starts_with("\\\n") {
                    self.position.advance("\\\n");
                }
                else if slice.starts_with("\\N{") {
                    if let Some(end) = slice.find("}") {
                        let name = &slice[3..end];
                        let raw = &slice[0..end+1];
                        let literal = self.translate_unicode_name(name, start, context)?;
                        string += &literal;
                        self.position.advance(raw);
                    } else {
                        string += "\\N";
                        self.position.increment(2);
                    }
                }
                else if slice.starts_with("\\") {
                    let start = &slice[0..2];
                    string += start;
                    self.position.advance(start);
                }
                else if slice.starts_with("{{") {
                    string += "{";
                    self.position.increment(2);
                }
                else if slice.starts_with("}}") {
                    string += "}";
                    self.position.increment(2);
                }
                else if slice.starts_with("}") {
                    let err = format!(
                        "  File \"{}\", line {}\n{}\n{}\nSyntaxError: f-string: single '}}' is not allowed",
                        context.file_name, self.position.line, self.get_line_by_number(self.position.line).unwrap(), Self::get_error_squiggle(self.position.column, 1)
                    );
                    context.set_error(err.clone());
                    return Err(anyhow!("{err}"))
                }
                else if slice.starts_with("{") {
                    self.position.increment(1);
                    parts.push(PartialFString::StringContent(string));
                    string = String::new();
                    let next = &slice[1..];
                    let tokens = self.lex_normal(next, context, self.position.index, true)?;

                    let start = self.position.index - relative;
                    let slice = &code[start..];

                    let spec = self.parse_format_specifier(slice, context)?;

                    parts.push(PartialFString::TokenStream(tokens, spec));
                }
                else if slice.starts_with(delimiter) {
                    self.position.advance(delimiter);
                    if !string.is_empty() {
                        parts.push(PartialFString::StringContent(string));
                    }
                    break 'f_str;
                }
                else if slice.starts_with("\n") {
                    if is_multiline {
                        string += "\n";
                        self.position.advance("\n");
                    } else {
                        let err = format!(
                            "  File \"{}\", line {}\n{}\n{}\nSyntaxError: unterminated string literal (detected at line {})",
                            context.file_name, pos.line, self.get_line_by_number(pos.line).unwrap(), Self::get_error_squiggle(pos.column, 3), pos.line
                        );
                        context.set_error(err.clone());
                        return Err(anyhow!("{err}"))
                    }
                }
                else {
                    let c = &slice[0..1];
                    string += c;
                    self.position.advance(c);
                }

            }

            let literal = &code[0..self.position.index-relative];

            if !is_raw {
                for part in &mut parts {
                    match part {
                        PartialFString::StringContent(s) => *part = PartialFString::StringContent(unescape(&s, context, true).unwrap()),
                        _ => {}
                    }
                }
            }

            Ok(Token::new(TokenValue::FString(parts), pos.span_to(&self.position), literal))
        } else {
            let relative = self.position.index;
            loop {
                let start = self.position.index - relative;
                let next = &code[start..];
                if next.starts_with("\\\n") {
                    self.position.advance("\\\n");
                } else if next.starts_with("\\") {
                    string += &next[0..2];
                    self.position.advance(&next[0..2]);
                } else if next.starts_with(delimiter) {
                    break;
                } else {
                    string += &next[0..1];
                    self.position.advance(&next[0..1]);
                }
            }
            self.position.advance(delimiter);

            let literal_size = self.position.index - pos.index;

            if !is_raw {
                match unescape(&string, context, !is_bytes) {
                    Ok(s) => string = s,
                    Err(e) => {
                        context.set_error(e.clone());
                        return Err(anyhow!("{e}"))
                    }
                }
            }

            if is_bytes {
                Ok(Token::new(TokenValue::BytesLiteral(string.into_bytes()), pos.span_to(&self.position), &raw_code[0..literal_size]))
            } else {
                Ok(Token::new(TokenValue::StringLiteral(string), pos.span_to(&self.position), &raw_code[0..literal_size]))
            }

        }

    }

    fn parse_format_specifier(&mut self, code: &str, context: &mut LexerContext) -> Result<Vec<PartialFString>> {

        let relative = self.position.index;

        let mut parts = Vec::new();

        let mut string = String::new();

        loop {
            let start = self.position.index - relative;
            let slice = &code[start..];

            if slice.starts_with("}") {
                self.position.increment(1);
                if !string.is_empty() {
                    parts.push(PartialFString::StringContent(string));
                }
                return Ok(parts)
            }
            else if slice.starts_with("{") {
                self.position.increment(1);
                parts.push(PartialFString::StringContent(string));
                string = String::new();
                let next = &slice[1..];
                let tokens = self.lex_normal(next, context, self.position.index, true)?;

                let start = self.position.index - relative;
                let slice = &code[start..];

                let spec = self.parse_format_specifier(slice, context)?;

                parts.push(PartialFString::TokenStream(tokens, spec));
            }
            else if slice.starts_with("\\{") {
                string += "\\";
                self.position.increment(1);
            }
            else if slice.starts_with("\\") {
                let start = &slice[0..2];
                string += start;
                self.position.advance(start);
            }
            else {
                let c = &slice[0..1];
                string += c;
                self.position.advance(c);
            }

        }

    }


}




