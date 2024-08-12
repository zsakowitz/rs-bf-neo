pub enum Token<'a> {
    Comment(&'a str),
    Word(&'a str),
    Int(u32),
    Char(char),
    Str(String),

    ArrowRight,
    Semicolon,
    BraceOpen,
    BraceClose,
    EqualSign,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Comma,
    Period,
    Percent,
}

pub fn tokenize(source: &str) -> Vec<Token> {
    let mut output = Vec::new();
    output
}
