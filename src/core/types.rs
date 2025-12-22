

#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
pub struct TextPosition {
    pub index: usize,
    pub column: usize,
    pub row: usize,
}

#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
pub struct TextSpan {
    start: TextPosition,
    end: TextPosition,
}

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
}

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

#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "snake_case"))]
pub enum Number {
    I64(i64),
    F64(f64),
}

#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize), serde(rename_all = "snake_case"))]
pub enum PartialFString {
    StringContent(String),
    TokenStream(Vec<Token>)
}

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
    DocComment(String),
    FString(Vec<PartialFString>),
    Newline,
    LeadingWhitespace,
    Indent,
    Dedent,
}

#[cfg_attr(feature = "serial", derive(serde::Serialize, serde::Deserialize))]
pub struct Token {
    value: TokenValue,
    span: TextSpan,
    content: String,
}

impl TextPosition {
    pub fn zero() -> Self {
        Self {
            index: 0,
            column: 0,
            row: 0,
        }
    }
}



