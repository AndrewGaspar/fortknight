use crate::span::Span;

#[derive(Copy, Clone, Debug)]
pub struct FormatToken {
    pub kind: FormatTokenKind,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FormatTokenKind {
    CharLiteralConstant,
    DigitString,
    Plus,
    Minus,
    Slash,
    Asterisk,
    Colon,
    Dot,
    Comma,

    LeftParen,
    RightParen,

    A,
    B,
    BN,
    BZ,
    D,
    DC,
    DP,
    DT,
    E,
    EN,
    ES,
    EX,
    F,
    G,
    I,
    L,
    O,
    P,
    RC,
    RD,
    RN,
    RP,
    RU,
    RZ,
    S,
    SP,
    SS,
    T,
    TL,
    TR,
    X,
    Z,

    Unknown,
}

impl FormatTokenKind {
    pub fn friendly_name(&self) -> String {
        use FormatTokenKind::*;

        match self {
            CharLiteralConstant => "char-literal-constant".into(),
            DigitString => "digit-string".into(),

            Minus => "`-`".into(),
            Plus => "`+`".into(),
            Slash => "`/`".into(),
            Asterisk => "`*`".into(),
            Colon => "`:`".into(),
            Dot => "`.`".into(),
            Comma => "`,`".into(),

            LeftParen => "`(`".into(),
            RightParen => "`)`".into(),

            A => "A".into(),
            B => "B".into(),
            BN => "BN".into(),
            BZ => "BZ".into(),
            E => "E".into(),
            EN => "EN".into(),
            ES => "ES".into(),
            EX => "EX".into(),
            D => "D".into(),
            DC => "DC".into(),
            DP => "DP".into(),
            DT => "DT".into(),
            F => "F".into(),
            G => "G".into(),
            I => "I".into(),
            L => "L".into(),
            O => "O".into(),
            P => "P".into(),
            RC => "RC".into(),
            RD => "RD".into(),
            RN => "RN".into(),
            RP => "RP".into(),
            RU => "RU".into(),
            RZ => "RZ".into(),
            S => "S".into(),
            SP => "SP".into(),
            SS => "SS".into(),
            T => "T".into(),
            TL => "TL".into(),
            TR => "TR".into(),
            X => "X".into(),
            Z => "Z".into(),

            Unknown => "unknown".into(),
        }
    }
}
