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
            TokenKind::Name | TokenKind::Keyword(_) => self.span,
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

    NewLine,
    Commentary,

    RealLiteralConstant,
    IntegerLiteralConstant,
    CharLiteralConstant,
    DigitString,
    DefinedOperator,

    // Symbols
    And,
    EquivalentOp,
    NotEquivalentOp,
    NotOp,
    OrOp,
    EqualsOp,
    NotEqualsOp,
    LessThanOp,
    LessThanOrEqualsOp,
    GreaterThanOp,
    GreaterThanOrEqualsOp,
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
    Dot,
    Comma,
    SemiColon,
    EqualsEquals,
    SlashEquals,
    LeftAngle,
    LeftAngleEquals,
    RightAngle,
    RightAngleEquals,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum KeywordTokenKind {
    // statements
    End,
    Implicit,
    None,

    // Derived Types
    // Section 7: Types
    // 7.3.2.1: Type Specifier Syntax
    Type,
    Class,
    // 7.4: Intrinsic Types
    Real,
    Double,
    Precision,
    DoublePrecision,
    Complex,
    Character,
    Logical,
    Integer,
    Kind,
    // 7.4.4.2: Character Type Specifier
    Len,
    // 7.5.2: Derived-type definition
    // Type,
    Abstract,
    Bind,
    C,
    Extends,
    EndType,
    // 7.5.2.3: Sequence type
    Sequence,
    // 7.5.3.1: Tyep parameter definition statement
    // Kind,
    // Len,
    // 7.5.4.1: Component definition statement
    Allocatable,
    Codimension,
    Contiguous,
    Dimension,
    Pointer,
    // Procedure,
    Nopass,
    Pass,
    // 7.5.4.8: Component accessiblity
    Private,
    // Pointer,
    // 7.5.5: Type-bound procedures
    // Private,
    // Procedure,
    Generic,
    Deferred,
    #[allow(non_camel_case_types)]
    Non_Overridable,
    // Nopass,
    // Pass,
    // 7.5.6.1: FINAL statement
    Final,
    // 7.6: Enumerations and Enumerators
    Enum,
    // Bind,
    // C,
    Enumerator,
    EndEnum,

    // Section 8: Attribute declarations and specifications
    // 8.2: Type Declaration Statement
    // Allocatable,
    Asynchronous,
    // Codimension,
    // Contiguous,
    // Dimension,
    External,
    Intent,
    Intrinsic,
    Optional,
    Parameter,
    // Pointer,
    Protected,
    Save,
    Target,
    Value,
    Volatile,
    // 8.5.2: Accessibility Attribute
    Public,
    // Private,
    // 8.5.5: BIND attribute for data entities
    // Bind,
    // C,
    Name,
    // 8.5.8: DIMENSION attribute
    // Dimension,
    // 8.5.10: INTENT attribute
    In,
    Out,
    InOut,
    // 8.8: IMPORT statement
    Import,
    Only,
    All,
    // None,
    // 8.9: NAMELIST statement
    Namelist,
    // 8.10.1: EQUIVALENCE statement
    Equivalence,
    // 8.10.2: COMMON statement
    Common,

    // Section 9: Use of data objects
    // 9.4.4: Complex parts
    Re,
    Im,
    // 9.6: Image selectors
    Stat,
    Team,
    #[allow(non_camel_case_types)]
    Team_Number,
    // 9.7.1: ALLOCATE statement
    Allocate,
    Errmsg,
    Mold,
    Source,
    // Stat,
    // 9.7.2: NULLIFY statement
    Nullify,
    // 9.7.3: DEALLOCATE statement
    Deallocate,
    // Stat,
    // Errmsg,

    // Section 10: Expressions and Assignment
    Where,
    Else,
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
    // Team,
    EndTeam,
    Critical,
    EndCritical,
    Do,
    EndDo,
    If,
    Then,
    // Else,
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
    // All,
    // Stat,
    // Errmsg,
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

    // Section 13: Input/output editing
    // 13.2.1: FORMAT statement
    Format,

    // Section 14: Program Units
    // 14.1: Main Program
    Program,
    EndProgram,
    // 14.2: Modules
    Module,
    EndModule,
    // 14.2.2: The USE statement and use association
    Use,
    // Only,
    //Intrinsic,
    #[allow(non_camel_case_types)]
    Non_Intrinsic,
    Operator,
    Submodule,
    EndSubmodule,
    // Block,
    Data,
    BlockData,
    EndBlockData,

    // Section 15: Procedures
    // 15.4.3.2: Interface block
    Interface,
    // Abstract,
    EndInterface,
    // Module,
    // Procedure,
    // Operator,
    Assignment,
    // Read,
    // Formatted,
    // Unformatted,
    // Write,
    // Formatted,
    // Unformatted,
    // 15.4.3.3: Generic statement
    // Generic,
    // 15.4.3.5: EXTERNAL statement
    // External,
    // 15.4.3.6: Procedure declaration statement
    // Procedure,
    // Intent,
    // Optional,
    // Pointer,
    // Protected,
    // Save,
    // 15.4.3.7: INTRINSIC statement
    // Intrinsic,
    // 15.5.1: Syntax of a procedure reference
    Call,
    // 15.6: Procedure definition
    // 15.6.2: Procedures defined by subprograms
    // 15.6.2.1: General
    Elemental,
    Impure,
    // Module,
    #[allow(non_camel_case_types)]
    Non_Recursive,
    Pure,
    Recursive,
    // 15.6.2.2: Function subprogram
    Function,
    Result,
    EndFunction,
    // 15.6.2.3: Subroutine subprogram
    Subroutine,
    EndSubroutine,
    // 15.6.2.5: Separate module procedures
    // Module,
    Procedure,
    EndProcedure,
    // 15.6.2.6: ENTRY statement
    Entry,
    // 15.6.2.7: RETURN statement
    Return,
    // 15.6.2.8: CONTAINS statement
    Contains,
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
        Non_Intrinsic,
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
        Format,
        Len,
        Abstract,
        Extends,
        Sequence,
        Nopass,
        Pass,
        Generic,
        Deferred,
        Non_Overridable,
        Final,
        Enumerator,
        Import,
        Common,
        Re,
        Im,
        Team_Number,
        Mold,
        Source,
        Nullify,
        Assignment,
        Elemental,
        Impure,
        Non_Recursive,
        Pure,
        Recursive,
        Result,
        Entry,
        Return,
    ]
};

pub const INTRINSIC_OPERATORS: &'static [(&'static str, TokenKind)] = {
    use self::TokenKind::*;

    &[
        ("AND", And),
        ("EQ", EqualsOp),
        ("EQV", EquivalentOp),
        ("FALSE", False),
        ("GE", GreaterThanOrEqualsOp),
        ("GT", GreaterThanOp),
        ("LE", LessThanOrEqualsOp),
        ("LT", LessThanOp),
        ("NE", NotEqualsOp),
        ("NEQV", NotEquivalentOp),
        ("NOT", NotOp),
        ("OR", OrOp),
        ("TRUE", True),
    ]
};
