use super::{CaseInsensitiveUserStr, UserStr};
use crate::data::FileData;
use crate::intern::{InternedName, StringInterner};
use crate::span::Span;

#[derive(Copy, Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    fn get_internable_span(&self, file_data: &FileData) -> Option<Span> {
        let span = match self.kind {
            TokenKind::Name => self.span,
            TokenKind::Keyword(k) => self.span,
            TokenKind::DefinedOperator => {
                let text = file_data.read_span(&self.span);
                debug_assert_eq!(".", &text[..1], "Internal error: Invariant that DefinedOperator starts with a . was not upheld.");
                debug_assert_eq!(".", &text[text.len()-1..], "Internal error: Invariant that DefinedOperator starts with a . was not upheld.");

                let mut span = self.span;
                span.start += 1;
                span.end -= 1;
                span
            }
            _ => return None,
        };

        Some(span)
    }

    pub fn try_intern(
        &self,
        interner: &mut StringInterner,
        file_data: &FileData,
    ) -> Option<InternedName> {
        let span = self.get_internable_span(file_data)?;

        Some(InternedName {
            id: interner
                .intern_string(CaseInsensitiveUserStr::new(file_data.read_span(&span)).to_string()),
            case_sensitive_id: interner
                .intern_string(UserStr::new(file_data.read_span(&span)).to_string()),
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    // user strings
    Name,
    Keyword(KeywordTokenKind),
    IntegerLiteralConstant,
    CharLiteralConstant,
    DigitString,
    DefinedOperator,

    // Symbols
    And,
    Equivalent,
    NotEquivalent,
    Not,
    Or,
    EqualsOp,
    NotEqualsOp,
    LessThan,
    LessThanOrEquals,
    GreaterThan,
    GreaterThanOrEquals,
    True,
    False,

    Arrow,
    Equals,
    Minus,
    Percent,
    Plus,
    Slash,
    SlashSlash,
    Star,
    StarStar,
    Colon,
    ColonColon,

    // structure
    SemiColon,
    NewLine,
    Comma,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum KeywordTokenKind {
    // statements
    Program,
    EndProgram,
    Module,
    EndModule,
    End,
    Contains,
    Function,
    EndFunction,
    Subroutine,
    EndSubroutine,
    Submodule,
    EndSubmodule,
    Procedure,
    EndProcedure,
    Interface,
    EndInterface,
    Implicit,
    None,

    // Intrinsic Types
    Real,
    Double,
    Precision,
    DoublePrecision,
    Complex,
    Character,
    Logical,
    Integer,
    Kind,

    // Derived Types
    Enum,
    EndEnum,
    Type,
    EndType,
    Class,

    // modules
    Use,
    NonIntrinsic,
    Only,
    Operator,
    // Section 8: Attribute declarations and specifications
    // 8.2: Type Declaration Statement
    Allocatable,
    Asynchronous,
    Codimension,
    Contiguous,
    Dimension,
    External,
    Intent,
    Intrinsic,
    Optional,
    Parameter,
    Pointer,
    Protected,
    Save,
    Target,
    Value,
    Volatile,
    // 8.5.2: Accessibility Attribute
    Public,
    Private,
    // 8.5.5: BIND attribute for data entities
    Bind,
    C,
    Name,
    // 8.5.10: INTENT attribute
    In,
    Out,
    InOut,
    // 8.9: NAMELIST statement
    Namelist,
    // 8.10.1: EQUIVALENCE statement
    Equivalence,
    // 8.10.2: COMMON statement

    // Section 9: Use of data objects
    Allocate,
    Deallocate,

    // Section 10: Expressions and Assignment
    Where,
    ElseWhere,
    EndWhere,
    Forall,
    EndForall,

    // Section 11: Execution Control
    Associate,
    EndAssociate,
    Block,
    EndBlock,
    Change,
    Team,
    EndTeam,
    Critical,
    EndCritical,
    Do,
    EndDo,
    If,
    Then,
    Else,
    ElseIf,
    EndIf,
    Case,
    Select,
    SelectCase,
    Default,
    EndSelect,
    Rank,
    // Select,
    // Default,
    // Type,
    // Select,
    SelectType,
    // Class,
    Is,
    // Default,
    Exit,
    Go,
    To,
    GoTo,
    Continue,
    Stop,
    Error,
    Quiet,
    Fail,
    Image,
    Sync,
    All,
    Stat,
    Errmsg,
    Images,
    Memory,
    // Team,
    Event,
    Post,
    Wait,
    #[allow(non_camel_case_types)]
    Until_Count, // UNTIL_COUNT
    Form,
    #[allow(non_camel_case_types)]
    New_Index, // NEW_INDEX
    Lock,
    #[allow(non_camel_case_types)]
    Acquired_Lock, // ACQUIRED_LOCK
    Unlock,

    // Section 12: Files
    File,
    EndFile,
    Backspace,
    Rewind,
    Flush,
    Inquire,
    Print,
    Open,
    Close,
    Read,
    Write,
    // Wait,

    // Section 12: File keywords
    Access,
    Action,
    Advance,
    // Asynchronous,
    Blank,
    Decimal,
    Delim,
    Direct,
    Encoding,
    Eor,
    Err,
    Exist,
    // File,
    Fmt,
    // Form,
    Formatted,
    Id,
    Iolength,
    Iomsg,
    Iostat,
    // Name,
    Named,
    NewUnit,
    Nextrec,
    Nml,
    Number,
    Opened,
    Pad,
    Pending,
    Pos,
    Position,
    // Read,
    Readwrite,
    Rec,
    Recl,
    Round,
    Sequential,
    Sign,
    Size,
    Status,
    Stream,
    Unformatted,
    Unit,
    // Write,

    // Section 14: Program Units
    // Block,
    Data,
    BlockData,
    EndBlockData,

    // Section 15: Procedures
    // 15.5.1: Syntax of a procedure reference
    Call,
}

lazy_static::lazy_static! {
    pub static ref KEYWORDS_TRIE: radix_trie::Trie<String, KeywordTokenKind> = {
        use std::iter::FromIterator;
        radix_trie::Trie::from_iter(
            KEYWORDS.iter().map(|kind| (format!("{:?}", kind).to_lowercase(), *kind)))
    };
}

pub const KEYWORDS: &'static [KeywordTokenKind] = {
    use self::KeywordTokenKind::*;

    &[
        Allocatable,
        Asynchronous,
        Bind,
        C,
        Call,
        Character,
        Codimension,
        Complex,
        Contains,
        Contiguous,
        Dimension,
        Double,
        End,
        EndFunction,
        EndInterface,
        EndModule,
        EndProcedure,
        EndProgram,
        EndSubmodule,
        EndSubroutine,
        External,
        Function,
        In,
        InOut,
        Integer,
        Intent,
        Intrinsic,
        Implicit,
        Kind,
        Logical,
        Module,
        Name,
        NonIntrinsic,
        None,
        Only,
        Operator,
        Optional,
        Out,
        Parameter,
        Pointer,
        Precision,
        Print,
        Private,
        Program,
        Protected,
        Public,
        Real,
        Save,
        Subroutine,
        Submodule,
        Target,
        Use,
        Value,
        Volatile,
        Procedure,
        Interface,
        DoublePrecision,
        Enum,
        EndEnum,
        Type,
        EndType,
        Class,
        Namelist,
        Equivalence,
        Allocate,
        Deallocate,
        Where,
        ElseWhere,
        EndWhere,
        Forall,
        EndForall,
        Associate,
        EndAssociate,
        Block,
        EndBlock,
        Change,
        Team,
        EndTeam,
        Critical,
        EndCritical,
        Do,
        EndDo,
        If,
        Then,
        Else,
        ElseIf,
        EndIf,
        Case,
        Select,
        SelectCase,
        Default,
        EndSelect,
        Rank,
        SelectType,
        Is,
        Exit,
        Go,
        To,
        GoTo,
        Continue,
        Stop,
        Error,
        Quiet,
        Fail,
        Image,
        Sync,
        All,
        Stat,
        Errmsg,
        Images,
        Memory,
        Event,
        Post,
        Wait,
        Until_Count,
        Form,
        New_Index,
        Lock,
        Acquired_Lock,
        Unlock,
        File,
        EndFile,
        Backspace,
        Rewind,
        Flush,
        Inquire,
        Open,
        Close,
        Read,
        Write,
        Access,
        Action,
        Advance,
        Blank,
        Decimal,
        Delim,
        Direct,
        Encoding,
        Eor,
        Err,
        Exist,
        Fmt,
        Formatted,
        Id,
        Iolength,
        Iomsg,
        Iostat,
        Named,
        NewUnit,
        Nextrec,
        Nml,
        Number,
        Opened,
        Pad,
        Pending,
        Pos,
        Position,
        Readwrite,
        Rec,
        Recl,
        Round,
        Sequential,
        Sign,
        Size,
        Status,
        Stream,
        Unformatted,
        Unit,
        Data,
        BlockData,
        EndBlockData,
    ]
};

pub const INTRINSIC_OPERATORS: &'static [(&'static str, TokenKind)] = {
    use self::TokenKind::*;

    &[
        ("AND", And),
        ("EQ", EqualsOp),
        ("EQV", Equivalent),
        ("FALSE", False),
        ("GE", GreaterThanOrEquals),
        ("GT", GreaterThan),
        ("LE", LessThanOrEquals),
        ("LT", LessThan),
        ("NE", NotEqualsOp),
        ("NEQV", NotEquivalent),
        ("NOT", Not),
        ("OR", Or),
        ("TRUE", True),
    ]
};
