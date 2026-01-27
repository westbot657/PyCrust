use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::ops::MulAssign;
use std::slice::Iter;
use std::vec::IntoIter;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
pub struct TextPosition {
    pub index: usize,
    pub column: usize,
    pub line: usize,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
pub struct TextSpan {
    pub start: TextPosition,
    pub end: TextPosition,
}

impl TextSpan {
    pub fn new(start: TextPosition, end: TextPosition) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "snake_case"))]
pub enum Keyword {
    Return,
    Continue,
    Break,
    If,
    Elif,
    Else,
    While,
    For,
    In,
    Is,
    As,
    With,
    Yield,
    Not,
    And,
    Or,
    Class,
    Def,
    Try,
    Except,
    Finally,
    Import,
    From,
    Raise,
    Global,
    Nonlocal,
    Pass,
    Lambda,
    Del,
    Assert,
    Async,
    Await,
}

impl Keyword {
    pub fn from_str(text: &str) -> Option<Self> {
        Some(match text {
            "return" => Self::Return,
            "continue" => Self::Continue,
            "break" => Self::Break,
            "if" => Self::If,
            "elif" => Self::Elif,
            "else" => Self::Else,
            "while" => Self::While,
            "for" => Self::For,
            "in" => Self::In,
            "is" => Self::Is,
            "as" => Self::As,
            "with" => Self::With,
            "yield" => Self::Yield,
            "not" => Self::Not,
            "and" => Self::And,
            "or" => Self::Or,
            "class" => Self::Class,
            "def" => Self::Def,
            "try" => Self::Try,
            "except" => Self::Except,
            "finally" => Self::Finally,
            "import" => Self::Import,
            "from" => Self::From,
            "raise" => Self::Raise,
            "global" => Self::Global,
            "nonlocal" => Self::Nonlocal,
            "pass" => Self::Pass,
            "lambda" => Self::Lambda,
            "del" => Self::Del,
            "assert" => Self::Assert,
            "async" => Self::Async,
            "await" => Self::Await,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
pub enum Symbol {
    #[cfg_attr(feature = "serial", serde(rename="("))]
    LParen,
    #[cfg_attr(feature = "serial", serde(rename=")"))]
    RParen,
    #[cfg_attr(feature = "serial", serde(rename="["))]
    LBracket,
    #[cfg_attr(feature = "serial", serde(rename="]"))]
    RBracket,
    #[cfg_attr(feature = "serial", serde(rename="{"))]
    LBrace,
    #[cfg_attr(feature = "serial", serde(rename="}"))]
    RBrace,
    #[cfg_attr(feature = "serial", serde(rename="->"))]
    Arrow,
    #[cfg_attr(feature = "serial", serde(rename="="))]
    Assign,
    #[cfg_attr(feature = "serial", serde(rename="."))]
    Dot,
    #[cfg_attr(feature = "serial", serde(rename=","))]
    Comma,
    #[cfg_attr(feature = "serial", serde(rename=":"))]
    Colon,
    #[cfg_attr(feature = "serial", serde(rename=":="))]
    Walrus,
    #[cfg_attr(feature = "serial", serde(rename=";"))]
    Semicolon,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
pub enum Comparator {
    #[cfg_attr(feature = "serial", serde(rename="=="))]
    Eq,
    #[cfg_attr(feature = "serial", serde(rename="!="))]
    Ne,
    #[cfg_attr(feature = "serial", serde(rename="<"))]
    Lt,
    #[cfg_attr(feature = "serial", serde(rename=">"))]
    Gt,
    #[cfg_attr(feature = "serial", serde(rename="<="))]
    Le,
    #[cfg_attr(feature = "serial", serde(rename=">="))]
    Ge,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
pub enum Operator {
    #[cfg_attr(feature = "serial", serde(rename="+"))]
    Add,
    #[cfg_attr(feature = "serial", serde(rename="-"))]
    Sub,
    #[cfg_attr(feature = "serial", serde(rename="*"))]
    Mul,
    #[cfg_attr(feature = "serial", serde(rename="/"))]
    Div,
    #[cfg_attr(feature = "serial", serde(rename="**"))]
    Pow,
    #[cfg_attr(feature = "serial", serde(rename="%"))]
    Mod,
    #[cfg_attr(feature = "serial", serde(rename="//"))]
    Floor,
    #[cfg_attr(feature = "serial", serde(rename="<<"))]
    Lsh,
    #[cfg_attr(feature = "serial", serde(rename=">>"))]
    Rsh,
    #[cfg_attr(feature = "serial", serde(rename="|"))]
    BitOr,
    #[cfg_attr(feature = "serial", serde(rename="&"))]
    BitAnd,
    #[cfg_attr(feature = "serial", serde(rename="^"))]
    BitXor,
    #[cfg_attr(feature = "serial", serde(rename="~"))]
    BitNot,
    #[cfg_attr(feature = "serial", serde(rename="@"))]
    MatMul,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "snake_case"))]
pub enum NumericValue {
    I64(i64),
    F64(f64),
}

impl From<i64> for NumericValue {
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}

impl From<f64> for NumericValue {
    fn from(value: f64) -> Self {
        Self::F64(value)
    }
}

impl MulAssign<NumericValue> for NumericValue {
    fn mul_assign(&mut self, rhs: NumericValue) {
        match (&self, rhs) {
            (NumericValue::I64(i), NumericValue::I64(i2)) => *self = NumericValue::I64(*i * i2),
            (NumericValue::I64(i), NumericValue::F64(f2)) => *self = NumericValue::F64(*i as f64 * f2),
            (NumericValue::F64(f), NumericValue::I64(i2)) => *self = NumericValue::F64(*f * i2 as f64),
            (NumericValue::F64(f), NumericValue::F64(f2)) => *self = NumericValue::F64(*f * f2),
        }
    }
}

impl MulAssign<f64> for NumericValue {
    fn mul_assign(&mut self, rhs: f64) {
        match &self {
            NumericValue::I64(i) => *self = NumericValue::F64(*i as f64 * rhs),
            NumericValue::F64(f) => *self = NumericValue::F64(*f * rhs),
        }
    }
}

impl MulAssign<i64> for NumericValue {
    fn mul_assign(&mut self, rhs: i64) {
        match &self {
            NumericValue::I64(i) => *self = NumericValue::I64(*i * rhs),
            NumericValue::F64(f) => *self = NumericValue::F64(*f * rhs as f64)
        }
    }
}


#[derive(Debug, Clone)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "snake_case"))]
pub enum Number {
    Real(NumericValue),
    Complex { real: NumericValue, imaginary: NumericValue },
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "snake_case"))]
pub enum PartialFString {
    StringContent(String),
    TokenStream(Tokens, Vec<PartialFString>),
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "snake_case"))]
pub enum TokenValue {
    Symbol(Symbol),
    Keyword(Keyword),
    Word(String),
    Operator(Operator),
    AssignOperator(Operator),
    Comparator(Comparator),
    StringLiteral(String),
    BytesLiteral(Vec<u8>),
    BooleanLiteral(bool),
    NumberLiteral(Number),
    Comment(String),
    FString(Vec<PartialFString>),
    LeadingWhitespace,
    Indent,
    Dedent,
    None,
    LineContinuation,
    Ellipsis,
    EndMarker,
    Newline,
}

#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub span: TextSpan,
    pub content: String,
}

impl Token {
    pub fn new(value: TokenValue, span: TextSpan, content: impl ToString) -> Self {
        Self {
            value,
            span,
            content: content.to_string(),
        }
    }

    fn convert_str_to_f_str(&mut self) {
        match self {
            Token { value: TokenValue::StringLiteral(s), span, content } => {
                let s = s.clone();
                let span = span.clone();
                let content = content.clone();
                *self = Token {
                    value: TokenValue::FString(vec![PartialFString::StringContent(s)]),
                    span,
                    content,
                }
            }
            Token { value: TokenValue::FString(_), .. } => {}
            _ => unreachable!("This function should only be called on string literal tokens")
        }
    }

    fn merge_f_str(&mut self, other: &Token) {
        match (&self, other) {
            (
                Token { value: TokenValue::FString(parts), span, content },
                Token { value: TokenValue::FString(parts2), span: span2, content: content2 }
            ) => {
                let mut parts = parts.clone();
                let mut p2 = parts2.clone();
                parts.append(&mut p2);
                let mut content = content.clone();
                content += content2;
                *self = Token {
                    value: TokenValue::FString(parts),
                    span: TextSpan::new(span.start.clone(), span2.end.clone()),
                    content,
                }
            }
            _ => unreachable!("This function should only be called with 2 f-string tokens")
        }
    }

    pub fn str_concat(&mut self, iter: &mut Peekable<IntoIter<Token>>) -> Result<bool, String> {
        match (&self, iter.peek().unwrap()) {
            (
                Token { value: TokenValue::StringLiteral(s), span, content },
                Token { value: TokenValue::StringLiteral(s2), span: span2, content: content2 },
            ) => {
                *self = Token {
                    value: TokenValue::StringLiteral(s.clone() + s2),
                    span: TextSpan::new(span.start.clone(), span2.end.clone()),
                    content: content.clone() + content2
                };
                let _ = iter.next().unwrap();
                Ok(true)
            }
            (
                Token { value: TokenValue::FString(_), .. },
                Token { value: TokenValue::FString(_) | TokenValue::StringLiteral(_), .. },
            ) => {
                let mut r = iter.next().unwrap();
                r.convert_str_to_f_str();
                self.merge_f_str(&r);
                Ok(true)
            }
            (
                Token { value: TokenValue::StringLiteral(_), .. },
                Token { value: TokenValue::FString(_), .. },
            ) => {
                self.convert_str_to_f_str();
                let r = iter.next().unwrap();
                self.merge_f_str(&r);
                Ok(true)
            }
            _ => Ok(false)
        }
    }

    pub fn byte_concat(&mut self, iter: &mut Peekable<IntoIter<Token>>) -> Result<bool, String> {
        match (&self, iter.peek().unwrap()) {
            (
                Token { value: TokenValue::BytesLiteral(bytes), span, content },
                Token { value: TokenValue::BytesLiteral(bytes2), span: span2, content: content2 }
            ) => {
                let mut bytes = bytes.clone();
                let mut b2 = bytes2.clone();
                bytes.append(&mut b2);

                *self = Token {
                    value: TokenValue::BytesLiteral(bytes),
                    span: TextSpan::new(span.start.clone(), span2.end.clone()),
                    content: content.clone() + content2
                };
                let _ = iter.next().unwrap();
                Ok(true)
            }
            _ => Ok(false)
        }
    }

}

impl TextPosition {
    pub fn zero() -> Self {
        Self {
            index: 0,
            column: 1,
            line: 1,
        }
    }

    pub fn advance(&mut self, content: &str) {
        for ch in content.chars() {
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.index += 1;
        }
    }

    pub fn increment(&mut self, offset: usize) {
        self.index += offset;
        self.column += offset;
    }

    pub fn span_to(&self, end: &Self) -> TextSpan {
        TextSpan::new(self.clone(), end.clone())
    }

    pub fn reset(&mut self) {
        *self = Self::zero()
    }

}

impl Display for PartialFString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PartialFString::StringContent(s) => write!(f, "[[{s:?}]]::str"),
            PartialFString::TokenStream(t, spec) => write!(f, "[[\n{t}\n]]:(\n{}\n)::expression",
                spec.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(",\n")
            )
        }
    }
}

impl Display for TextPosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:?}]::{}:[{}]", self.content, self.value, self.span)
    }
}

impl Display for TokenValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenValue::Word(_) => write!(f, "word"),
            TokenValue::Symbol(_) => write!(f, "symbol"),
            TokenValue::Keyword(_) => write!(f, "keyword"),
            TokenValue::Operator(_) => write!(f, "operator"),
            TokenValue::AssignOperator(_) => write!(f, "assign-operator"),
            TokenValue::Comparator(_) => write!(f, "comparison"),
            TokenValue::StringLiteral(_) => write!(f, "string-literal"),
            TokenValue::BytesLiteral(_) => write!(f, "bytes-literal"),
            TokenValue::BooleanLiteral(_) => write!(f, "boolean-literal"),
            TokenValue::NumberLiteral(_) => write!(f, "number-literal"),
            TokenValue::Comment(_) => write!(f, "comment"),
            TokenValue::FString(parts) => write!(f, "{{\n{}\n}}::f-string", parts.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(",\n")),
            TokenValue::LeadingWhitespace => write!(f, "leading-whitespace"),
            TokenValue::Indent => write!(f, "INDENT"),
            TokenValue::Dedent => write!(f, "DEDENT"),
            TokenValue::None => write!(f, "none"),
            TokenValue::LineContinuation => write!(f, "line-continuation"),
            TokenValue::Ellipsis => write!(f, "ellipsis"),
            TokenValue::EndMarker => write!(f, "END-MARKER"),
            TokenValue::Newline => write!(f, "NEWLINE"),
        }
    }
}

impl Display for TextSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
pub struct Tokens {
    tokens: Vec<Token>,
}

impl Tokens {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
        }
    }
    
    pub fn iter(&self) -> Iter<'_, Token> {
        self.tokens.iter()
    }
    
    pub fn into_iter(self) -> IntoIter<Token> {
        self.tokens.into_iter()
    }
    
    pub fn append(&mut self, other: &mut Tokens) {
        self.tokens.append(&mut other.tokens);
    }

    pub fn push(&mut self, token: Token) {
        self.tokens.push(token)
    }

    pub fn into_parse_tokens(self) -> ParseTokens {
        ParseTokens {
            offset: 0,
            tokens: self.tokens,
            snapshots: Vec::new(),
        }
    }

}

impl From<Vec<Token>> for Tokens {
    fn from(value: Vec<Token>) -> Self {
        Self { tokens: value }
    }
}

impl Into<Vec<Token>> for Tokens {
    fn into(self) -> Vec<Token> {
        self.tokens
    }
}

impl Display for Tokens {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for t in &self.tokens {
            write!(f, "{}\n", t)?
        }
        Ok(())
    }
}

pub struct ParseTokens {
    offset: usize,
    tokens: Vec<Token>,
    snapshots: Vec<usize>,
}

impl ParseTokens {

    pub fn get(&self, index: usize) -> Option<&Token> {
        self.tokens.get(self.offset + index)
    }

    pub fn consume_next(&mut self) -> Option<&Token> {
        let t = self.tokens.get(self.offset);
        if let Some(_) = t {
            self.offset += 1;
        };
        t
    }

    pub fn backtrack(&mut self) {
        if self.offset > 0 {
            self.offset -= 1;
        }
    }

    pub fn snapshot(&mut self) {
        self.snapshots.push(self.offset);
    }
    
    pub fn discard_snapshot(&mut self) {
        self.snapshots.pop();
    }
    
    pub fn restore(&mut self) {
        if let Some(old) = self.snapshots.pop() {
            self.offset = old;
        }
    }
    
    pub fn is_empty(&self) -> bool {
        self.tokens.len() - self.offset == 0
    }
    
}
