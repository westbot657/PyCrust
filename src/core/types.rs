

pub struct TextPosition {
    index: usize,
    column: usize,
    row: usize,
}

pub struct TextSpan {
    start: TextPosition,
    end: TextPosition,
}

pub struct Token {
    span: TextSpan,
}

pub struct TokenRef<'t> {
    token: &'t Token,
    content: &'t str
}




