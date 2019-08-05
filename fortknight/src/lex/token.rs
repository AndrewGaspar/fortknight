use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use crate::data::FileData;
use crate::intern::{InternedName, StringInterner};
use crate::span::Span;
use crate::string::{CaseInsensitiveContinuationStr, ContinuationStr};

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
            id: interner.intern_string(
                CaseInsensitiveContinuationStr::new(file_data.read_span(&span)).to_string(),
            ),
            case_sensitive_id: interner
                .intern_string(ContinuationStr::new(file_data.read_span(&span)).to_string()),
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum KeywordTokenKind {
    /// This must be the first keyword value in this list.
    MinimumKeywordKind = 1,

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
    // Kind,
    // 7.4.4.2: Character Type Specifier
    // Len,
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
    // All,
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
    // #[allow(non_camel_case_types)]
    // Team_Number,
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
    // Rank,
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
    // Sign,
    // Size,
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

    // Section 16: Intrinsic procedures and modules
    Abs,
    Achar,
    Acos,
    Acosh,
    Adjustl,
    Adjustr,
    Aimag,
    Aint,
    All,
    Allocated,
    Anint,
    Any,
    Asin,
    Asinh,
    Associated,
    Atan,
    Atan2,
    Atanh,
    #[allow(non_camel_case_types)]
    Atomic_Add,
    #[allow(non_camel_case_types)]
    Atomic_And,
    #[allow(non_camel_case_types)]
    Atomic_Cas,
    #[allow(non_camel_case_types)]
    Atomic_Define,
    #[allow(non_camel_case_types)]
    Atomic_Fetch_Add,
    #[allow(non_camel_case_types)]
    Atomic_Fetch_And,
    #[allow(non_camel_case_types)]
    Atomic_Fetch_Or,
    #[allow(non_camel_case_types)]
    Atomic_Fetch_Xor,
    #[allow(non_camel_case_types)]
    Atomic_Or,
    #[allow(non_camel_case_types)]
    Atomic_Ref,
    #[allow(non_camel_case_types)]
    Atomic_Xor,
    #[allow(non_camel_case_types)]
    Bessel_J0,
    #[allow(non_camel_case_types)]
    Bessel_J1,
    #[allow(non_camel_case_types)]
    Bessel_JN,
    #[allow(non_camel_case_types)]
    Bessel_Y0,
    #[allow(non_camel_case_types)]
    Bessel_Y1,
    #[allow(non_camel_case_types)]
    Bessel_YN,
    BGE,
    BGT,
    #[allow(non_camel_case_types)]
    BIT_SIZE,
    BLE,
    BLT,
    Btest,
    Ceiling,
    Char,
    Cmplx,
    #[allow(non_camel_case_types)]
    Co_Broadcast,
    #[allow(non_camel_case_types)]
    Co_Max,
    #[allow(non_camel_case_types)]
    Co_Min,
    #[allow(non_camel_case_types)]
    Co_Reduce,
    #[allow(non_camel_case_types)]
    Co_Sum,
    #[allow(non_camel_case_types)]
    Command_Argument_Count,
    Conjg,
    Cos,
    Cosh,
    Coshape,
    Count,
    #[allow(non_camel_case_types)]
    Cpu_Time,
    Cshift,
    #[allow(non_camel_case_types)]
    Date_And_Time,
    Dble,
    Digits,
    Dim,
    #[allow(non_camel_case_types)]
    Dot_Product,
    Dprod,
    Dshiftl,
    Dshiftr,
    Eoshift,
    Epsilon,
    Erf,
    Erfc,
    #[allow(non_camel_case_types)]
    Erfc_Scaled,
    #[allow(non_camel_case_types)]
    Event_Query,
    #[allow(non_camel_case_types)]
    Execute_Command_Line,
    Exp,
    Exponent,
    #[allow(non_camel_case_types)]
    Extends_Type_Of,
    #[allow(non_camel_case_types)]
    Failed_Images,
    Findloc,
    Floor,
    Fraction,
    Gamma,
    #[allow(non_camel_case_types)]
    Get_Command,
    #[allow(non_camel_case_types)]
    Get_Command_Argument,
    #[allow(non_camel_case_types)]
    Get_Environment_Variable,
    #[allow(non_camel_case_types)]
    Get_Team,
    Huge,
    Hypot,
    Iachar,
    Iall,
    Iand,
    Iany,
    Ibclr,
    Ibits,
    Ibset,
    Ichar,
    Ieor,
    #[allow(non_camel_case_types)]
    Image_Index,
    #[allow(non_camel_case_types)]
    Image_Status,
    Index,
    Int,
    Ior,
    Iparity,
    Ishft,
    Ishftc,
    #[allow(non_camel_case_types)]
    Is_Contiguous,
    #[allow(non_camel_case_types)]
    Is_Iostat_End,
    #[allow(non_camel_case_types)]
    Is_Iostat_Eor,
    Kind,
    Lbound,
    Lcobound,
    Leadz,
    Len,
    #[allow(non_camel_case_types)]
    Len_Trim,
    LGE,
    LGT,
    LLE,
    LLT,
    Log,
    #[allow(non_camel_case_types)]
    Log_Gamma,
    Log10,
    // Logical,
    Maskl,
    Maskr,
    Matmul,
    Max,
    Maxexponent,
    Maxloc,
    Maxval,
    Merge,
    #[allow(non_camel_case_types)]
    Merge_Bits,
    Min,
    Minexponent,
    Minloc,
    Minval,
    Mod,
    Modulo,
    #[allow(non_camel_case_types)]
    Move_Alloc,
    Mvbits,
    Nearest,
    #[allow(non_camel_case_types)]
    New_Line,
    Nint,
    Norm2,
    Not,
    Null,
    #[allow(non_camel_case_types)]
    Num_Images,
    #[allow(non_camel_case_types)]
    Out_Of_Range,
    Pack,
    Parity,
    Popcnt,
    Poppar,
    // Precision,
    Present,
    Product,
    Radix,
    #[allow(non_camel_case_types)]
    Random_Init,
    #[allow(non_camel_case_types)]
    Random_Number,
    #[allow(non_camel_case_types)]
    Random_Seed,
    Range,
    Rank,
    // Real,
    Reduce,
    Repeat,
    Reshape,
    Rrspacing,
    #[allow(non_camel_case_types)]
    Same_Type_As,
    Scale,
    Scan,
    #[allow(non_camel_case_types)]
    Selected_Char_Kind,
    #[allow(non_camel_case_types)]
    Selected_Int_Kind,
    #[allow(non_camel_case_types)]
    Selected_Real_Kind,
    #[allow(non_camel_case_types)]
    Set_Exponent,
    Shape,
    Shifta,
    Shiftl,
    Shiftr,
    Sign,
    Sin,
    Sinh,
    Size,
    Spacing,
    Spread,
    Sqrt,
    #[allow(non_camel_case_types)]
    Stopped_Images,
    #[allow(non_camel_case_types)]
    Storage_Size,
    Sum,
    #[allow(non_camel_case_types)]
    System_Clock,
    Tan,
    Tanh,
    #[allow(non_camel_case_types)]
    Team_Number,
    #[allow(non_camel_case_types)]
    This_Image,
    Tiny,
    Trailz,
    Transfer,
    Transpose,
    Trim,
    Ubound,
    Ucobound,
    Unpack,
    Verify,

    /// This must be the final value in this list.
    MaximumKeywordKind,
}

impl KeywordTokenKind {
    pub fn all_keywords() -> impl Iterator<Item = Self> {
        let min = KeywordTokenKind::MinimumKeywordKind.to_usize().unwrap() + 1;
        let max = KeywordTokenKind::MaximumKeywordKind.to_usize().unwrap();

        (min..max).map(|k| KeywordTokenKind::from_usize(k).unwrap())
    }

    pub fn is_intrinsic_type(self) -> bool {
        let min = KeywordTokenKind::Real.to_usize().unwrap();
        let max = KeywordTokenKind::Integer.to_usize().unwrap();
        let value = self.to_usize().unwrap();

        value >= min && value <= max
    }

    pub fn is_intrinsic_procedure(self) -> bool {
        let min = KeywordTokenKind::Abs.to_usize().unwrap();
        let max = KeywordTokenKind::Verify.to_usize().unwrap();
        let value = self.to_usize().unwrap();

        // Logical, Precision, and Real are also intrinsic types
        value >= min && value <= max
            || self == KeywordTokenKind::Logical
            || self == KeywordTokenKind::Precision
            || self == KeywordTokenKind::Real
    }
}

lazy_static::lazy_static! {
    pub static ref KEYWORDS_TRIE: radix_trie::Trie<String, KeywordTokenKind> = {
        use std::iter::FromIterator;

        radix_trie::Trie::from_iter(
            KeywordTokenKind::all_keywords().map(
                |kind| (format!("{:?}", kind).to_lowercase(), kind)))
    };
}

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
