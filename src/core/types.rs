use std::fmt::{Display, Formatter};
use std::ops::MulAssign;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
pub struct TextPosition {
    pub index: usize,
    pub column: usize,
    pub line: usize,
}

#[derive(Debug)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
pub struct TextSpan {
    start: TextPosition,
    end: TextPosition,
}

impl TextSpan {
    pub fn new(start: TextPosition, end: TextPosition) -> Self {
        Self {
            start, end
        }
    }
}

#[derive(Debug)]
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
            _ => return None,
        })
    }
}

#[derive(Debug)]
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
    #[cfg_attr(feature = "serial", serde(rename="@"))]
    Decorator,
    #[cfg_attr(feature = "serial", serde(rename="="))]
    Assign,
    #[cfg_attr(feature = "serial", serde(rename="."))]
    Dot,
    #[cfg_attr(feature = "serial", serde(rename=","))]
    Comma,
    #[cfg_attr(feature = "serial", serde(rename=":"))]
    Colon,
}

#[derive(Debug)]
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

#[derive(Debug)]
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
}

#[derive(Debug)]
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


#[derive(Debug)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "snake_case"))]
pub enum Number {
    Real(NumericValue),
    Complex { real: NumericValue, imaginary: NumericValue },
}

#[derive(Debug)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "snake_case"))]
pub enum PartialFString {
    StringContent(String),
    TokenStream(Vec<Token>)
}

#[derive(Debug)]
#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "snake_case"))]
pub enum TokenValue {
    Symbol(Symbol),
    Keyword(Keyword),
    Word(String),
    Operator(Operator),
    AssignOperator(Operator),
    Comparator(Comparator),
    StringLiteral(String),
    BooleanLiteral(bool),
    NumberLiteral(Number),
    Comment(String),
    FString(Vec<PartialFString>),
    Newline,
    LeadingWhitespace,
    Indent,
    Dedent,
    None,
}

#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug)]
pub struct Token {
    value: TokenValue,
    span: TextSpan,
    content: String,
}

impl Token {
    pub fn new(value: TokenValue, span: TextSpan, content: impl ToString) -> Self {
        Self {
            value,
            span,
            content: content.to_string(),
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
            TokenValue::BooleanLiteral(_) => write!(f, "boolean-literal"),
            TokenValue::NumberLiteral(_) => write!(f, "number-literal"),
            TokenValue::Comment(_) => write!(f, "comment"),
            TokenValue::FString(_) => write!(f, "f-string"),
            TokenValue::Newline => write!(f, "newline"),
            TokenValue::LeadingWhitespace => write!(f, "leading-whitespace"),
            TokenValue::Indent => write!(f, "indent"),
            TokenValue::Dedent => write!(f, "dedent"),
            TokenValue::None => write!(f, "none"),
        }
    }
}

impl Display for TextSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

pub struct Tokens {
    tokens: Vec<Token>
}

impl Tokens {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
        }
    }
    pub fn append(&mut self, other: &mut Tokens) {
        self.tokens.append(&mut other.tokens);
    }

    pub fn push(&mut self, token: Token) {
        self.tokens.push(token)
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

