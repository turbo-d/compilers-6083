use std::fmt;

#[derive(Clone, PartialEq)]
pub enum Token {
    Unknown,
    EOF,
    Program,
    Is,
    Global,
    Procedure,
    Variable,
    Begin,
    If,
    Then,
    Else,
    For,
    Return,
    EndProgram,
    EndProcedure,
    EndIf,
    EndFor,
    True,
    False,
    Not,
    IntType,
    FloatType,
    StringType,
    BoolType,
    Identifier(String),
    Number(f32),
    String(String),
    Colon,
    Semicolon,
    Period,
    Comma,
    LParen,
    RParen,
    LSquare,
    RSquare,
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Assign,
    LT,
    LTE,
    GT,
    GTE,
    Eq,
    NotEq,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Unknown => write!(f, "<\"Unknown\", Token::Unknown>"),
            Token::EOF => write!(f, "<\"EOF\", Token::EOF>"),
            Token::Program => write!(f, "<\"program\", Token::Program>"),
            Token::Is => write!(f, "<\"is\", Token::Is>"),
            Token::Global => write!(f, "<\"global\", Token::Global>"),
            Token::Procedure => write!(f, "<\"procedure\", Token::Procedure>"),
            Token::Variable => write!(f, "<\"variable\", Token::Variable>"),
            Token::Begin => write!(f, "<\"begin\", Token::Begin>"),
            Token::If => write!(f, "<\"if\", Token::If>"),
            Token::Then => write!(f, "<\"then\", Token::Then>"),
            Token::Else => write!(f, "<\"else\", Token::Else>"),
            Token::For => write!(f, "<\"for\", Token::For>"),
            Token::Return => write!(f, "<\"return\", Token::Return>"),
            Token::EndProgram => write!(f, "<\"end program\", Token::EndProgram>"),
            Token::EndProcedure => write!(f, "<\"end procedure\", Token::EndProcedure>"),
            Token::EndIf => write!(f, "<\"end if\", Token::EndIf>"),
            Token::EndFor => write!(f, "<\"end for\", Token::EndFor>"),
            Token::True => write!(f, "<\"true\", Token::True>"),
            Token::False => write!(f, "<\"false\", Token::False>"),
            Token::Not => write!(f, "<\"not\", Token::Not>"),
            Token::IntType => write!(f, "<\"integer\", Token::IntType>"),
            Token::FloatType => write!(f, "<\"float\", Token::FloatType>"),
            Token::StringType => write!(f, "<\"string\", Token::StringType>"),
            Token::BoolType => write!(f, "<\"bool\", Token::BoolType>"),
            Token::Identifier(i) => write!(f, "<\"{}\", Token::Identifier>", i),
            Token::Number(n) => write!(f, "<\"{}\", Token::Number>", n),
            Token::String(s) => write!(f, "<\"{}\", Token::String>", s),
            Token::Colon => write!(f, "<\":\", Token::Colon>"),
            Token::Semicolon => write!(f, "<\";\", Token::Semicolon>"),
            Token::Period => write!(f, "<\".\", Token::Period>"),
            Token::Comma => write!(f, "<\",\", Token::Comma>"),
            Token::LParen => write!(f, "<\"(\", Token::LParen>"),
            Token::RParen => write!(f, "<\")\", Token::RParen>"),
            Token::LSquare => write!(f, "<\"[\", Token::LSquare>"),
            Token::RSquare => write!(f, "<\"]\", Token::RSquare>"),
            Token::Add => write!(f, "<\"+\", Token::Add>"),
            Token::Sub => write!(f, "<\"-\", Token::Sub>"),
            Token::Mul => write!(f, "<\"*\", Token::Mul>"),
            Token::Div => write!(f, "<\"/\", Token::Div>"),
            Token::And => write!(f, "<\"&\", Token::And>"),
            Token::Or => write!(f, "<\"|\", Token::Or>"),
            Token::Assign => write!(f, "<\":=\", Token::Assign>"),
            Token::LT => write!(f, "<\"<\", Token::LT>"),
            Token::LTE => write!(f, "<\"<=\", Token::LTE>"),
            Token::GT => write!(f, "<\">\", Token::GT>"),
            Token::GTE => write!(f, "<\">=\", Token::GTE>"),
            Token::Eq => write!(f, "<\"==\", Token::Eq>"),
            Token::NotEq => write!(f, "<\"!=\", Token::NotEq>"),
       }
    }
}

