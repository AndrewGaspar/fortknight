use num_bigint::BigUint;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use typed_arena::Arena;

use crate::data::FileData;
use crate::intern::{InternedName, StringInterner};
use crate::num::Uint;
use crate::span::{Location, Span};
use crate::string::{CaseInsensitiveContinuationStr, ContinuationStr};

#[derive(Copy, Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn is_name(&self) -> bool {
        self.kind.is_name()
    }

    /// True if the token is a routine attribute
    pub fn is_routine_attribute(&self) -> bool {
        match self.kind {
            TokenKind::Keyword(KeywordTokenKind::Elemental)
            | TokenKind::Keyword(KeywordTokenKind::Impure)
            | TokenKind::Keyword(KeywordTokenKind::Module)
            | TokenKind::Keyword(KeywordTokenKind::Non_Recursive)
            | TokenKind::Keyword(KeywordTokenKind::Pure)
            | TokenKind::Keyword(KeywordTokenKind::Recursive) => true,
            _ => false,
        }
    }

    pub fn is_intrinsic_type(&self) -> bool {
        match self.kind {
            TokenKind::Keyword(KeywordTokenKind::Integer)
            | TokenKind::Keyword(KeywordTokenKind::Real)
            | TokenKind::Keyword(KeywordTokenKind::Double)
            | TokenKind::Keyword(KeywordTokenKind::DoublePrecision)
            | TokenKind::Keyword(KeywordTokenKind::Complex)
            | TokenKind::Keyword(KeywordTokenKind::Character)
            | TokenKind::Keyword(KeywordTokenKind::Logical) => true,
            _ => false,
        }
    }

    /// Returns true if the token is maybe the beginning of a routine prefix
    pub fn is_maybe_routine_prefix(&self) -> bool {
        if self.is_routine_attribute() {
            return true;
        }

        if self.is_intrinsic_type() {
            return true;
        }

        match self.kind {
            TokenKind::Keyword(KeywordTokenKind::Type)
            | TokenKind::Keyword(KeywordTokenKind::Class) => true,
            _ => false,
        }
    }

    fn get_internable_span(&self, contents: &str) -> Option<Span> {
        let span = match self.kind {
            TokenKind::Name | TokenKind::Keyword(_) | TokenKind::Letter(_) => self.span,
            TokenKind::DefinedOperator => {
                let text = &contents[self.span.start as usize..self.span.end as usize];
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
        let span = self.get_internable_span(&file_data.contents[self.span.file_id.0 as usize])?;

        Some(InternedName {
            id: interner.intern_string(
                CaseInsensitiveContinuationStr::new(file_data.read_span(&span)).to_string(),
            ),
            case_sensitive_id: interner
                .intern_string(ContinuationStr::new(file_data.read_span(&span)).to_string()),
        })
    }

    pub fn try_intern_contents(
        &self,
        interner: &mut StringInterner,
        contents: &str,
    ) -> Option<InternedName> {
        let span = self.get_internable_span(contents)?;

        Some(InternedName {
            id: interner.intern_string(
                CaseInsensitiveContinuationStr::new(
                    &contents[span.start as usize..span.end as usize],
                )
                .to_string(),
            ),
            case_sensitive_id: interner.intern_string(
                ContinuationStr::new(&contents[span.start as usize..span.end as usize]).to_string(),
            ),
        })
    }

    pub fn start(&self) -> Location {
        self.span.start_location()
    }

    pub fn end(&self) -> Location {
        self.span.end_location()
    }

    pub fn friendly_name(&self) -> String {
        self.kind.friendly_name()
    }

    /// R604: literal-constant
    pub fn try_into_uint<'a>(&self, contents: &str, arena: &'a Arena<BigUint>) -> Option<Uint<'a>> {
        fn convert_to_uint<'input, 'a, CharToDigit>(
            cont_str: ContinuationStr<'input>,
            arena: &'a Arena<BigUint>,
            radix: u8,
            char_to_digit: CharToDigit,
        ) -> Uint<'a>
        where
            CharToDigit: Fn(char) -> u8,
        {
            enum Either {
                Small(u32),
                Big(BigUint),
            }

            let result = cont_str.iter().fold(Either::Small(0), |mut u, c| {
                let digit = char_to_digit(c);
                debug_assert!(digit < radix);

                loop {
                    return match u {
                        Either::Small(x) => {
                            match x
                                .checked_mul(radix as u32)
                                .and_then(|x| x.checked_add(digit as u32))
                            {
                                Some(x) => Either::Small(x),
                                None => {
                                    u = Either::Big(x.into());
                                    continue;
                                }
                            }
                        }
                        Either::Big(mut x) => {
                            x *= radix as u32;
                            x += digit as u32;
                            Either::Big(x)
                        }
                    };
                }
            });

            match result {
                Either::Small(x) => Uint::Small(x),
                Either::Big(x) => Uint::Big(arena.alloc(x)),
            }
        }

        let contents = &contents[self.span.start as usize..self.span.end as usize];

        Some(match self.kind {
            TokenKind::DigitString => {
                let cont_str = ContinuationStr::new(contents);
                convert_to_uint(cont_str, arena, 10, |c| {
                    let c = c as u8;
                    debug_assert!(c >= b'0' && c <= b'9', "{}", c);
                    c - b'0'
                })
            }
            TokenKind::BinaryConstant => {
                // skip leading b' and ending '
                let cont_str = ContinuationStr::new(
                    &contents[self.span.start as usize + 2..self.span.end as usize - 1],
                );
                convert_to_uint(cont_str, arena, 2, |c| {
                    let c = c as u8;
                    debug_assert!(c >= b'0' && c <= b'1', "{}", c);
                    c - b'0'
                })
            }
            TokenKind::OctalConstant => {
                // skip leading o' and ending '
                let cont_str = ContinuationStr::new(
                    &contents[self.span.start as usize + 2..self.span.end as usize - 1],
                );
                convert_to_uint(cont_str, arena, 8, |c| {
                    let c = c as u8;
                    debug_assert!(c >= b'0' && c <= b'7', "{}", c);
                    c - b'0'
                })
            }
            TokenKind::HexConstant => {
                // skip leading z' and ending '
                let cont_str = ContinuationStr::new(
                    &contents[self.span.start as usize + 2..self.span.end as usize - 1],
                );
                convert_to_uint(cont_str, arena, 16, |c| {
                    let c = c as u8;
                    debug_assert!(
                        (c >= b'0' && c <= b'9')
                            || (c >= b'a' && c <= b'f')
                            || (c >= b'A' && c <= b'F'),
                        "{}",
                        c
                    );
                    if c >= b'0' && c <= b'9' {
                        c - b'0'
                    } else if c >= b'a' && c <= b'f' {
                        c - b'a' + 10
                    } else {
                        debug_assert!(c >= b'A' && c <= b'F', "{}", c);
                        c - b'A' + 10
                    }
                })
            }
            _ => return None,
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    // user strings
    Name,
    Keyword(KeywordTokenKind),
    Letter(Letter),

    NewLine,
    Commentary,

    RealLiteralConstant,
    CharLiteralConstant,
    DigitString,
    BinaryConstant,
    OctalConstant,
    HexConstant,

    DefinedOperator,

    // Symbols
    AndOp,
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
    SlashRightParen,
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
    Underscore,

    LeftParen,
    LeftParenSlash,
    RightParen,
    LeftBracket,
    RightBracket,

    // gcc Preprocessor tokens - only tokenized when enabled
    Pound,
    CBlockCommentary,

    // Returned when an unknown token is encountered
    Unknown,
}

impl TokenKind {
    pub fn friendly_name(&self) -> String {
        use TokenKind::*;

        match self {
            Name => "name".into(),
            Keyword(keyword) => format!("`{:?}`", keyword).to_uppercase(),
            Letter(letter) => format!("`{:?}`", letter).to_uppercase(),

            NewLine => "new-line".into(),
            Commentary => "commentary".into(),

            RealLiteralConstant => "real-literal-constant".into(),
            CharLiteralConstant => "char-literal-constant".into(),
            DigitString => "digit-string".into(),
            BinaryConstant => "binary-constant".into(),
            OctalConstant => "octal-constant".into(),
            HexConstant => "hex-constant".into(),

            DefinedOperator => "defined-operator".into(),

            AndOp => "`.AND.`".into(),
            EquivalentOp => "`.EQV.`".into(),
            NotEquivalentOp => "`.NEQV.".into(),
            NotOp => "`.NOT.`".into(),
            OrOp => "`.OR.`".into(),
            EqualsOp => "`.EQ.`".into(),
            NotEqualsOp => "`.NE.`".into(),
            LessThanOp => "`.LT.`".into(),
            LessThanOrEqualsOp => "`.LE.`".into(),
            GreaterThanOp => "`.GT.`".into(),
            GreaterThanOrEqualsOp => "`.GE.`".into(),
            True => "`.TRUE.`".into(),
            False => "`.FALSE.`".into(),

            Arrow => "`=>`".into(),
            Equals => "`=`".into(),
            Minus => "`-`".into(),
            Percent => "`%`".into(),
            Plus => "`+`".into(),
            Slash => "`/`".into(),
            SlashSlash => "`//`".into(),
            SlashRightParen => "`/)`".into(),
            Star => "`*`".into(),
            StarStar => "`**`".into(),
            Colon => "`:`".into(),
            ColonColon => "`::`".into(),
            Dot => "`.`".into(),
            Comma => "`,`".into(),
            SemiColon => "`;`".into(),
            EqualsEquals => "`==`".into(),
            SlashEquals => "`/=`".into(),
            LeftAngle => "`<`".into(),
            LeftAngleEquals => "`<=`".into(),
            RightAngle => "`>`".into(),
            RightAngleEquals => "`>=`".into(),
            Underscore => "`_`".into(),

            LeftParen => "`(`".into(),
            LeftParenSlash => "`(/`".into(),
            RightParen => "`)`".into(),
            LeftBracket => "`[`".into(),
            RightBracket => "`]`".into(),

            Pound => "`#`".into(),
            CBlockCommentary => "c-block-commentary".into(),

            Unknown => "unknown".into(),
        }
    }

    pub fn is_eos(&self) -> bool {
        match self {
            TokenKind::NewLine | TokenKind::SemiColon | TokenKind::Commentary => true,
            _ => false,
        }
    }

    /// R603: name
    #[inline]
    pub fn is_name(&self) -> bool {
        match self {
            TokenKind::Name | TokenKind::Keyword(_) | TokenKind::Letter(_) => true,
            _ => false,
        }
    }

    /// R604: constant
    #[inline]
    pub fn is_constant_start(&self) -> bool {
        if self.is_literal_constant_start() {
            return true;
        }

        self.is_name()
    }

    /// R604: literal-constant
    #[inline]
    pub fn is_literal_constant_start(&self) -> bool {
        if *self == TokenKind::DigitString || *self == TokenKind::RealLiteralConstant {
            return true;
        }

        if self.is_complex_literal_constant_start() {
            return true;
        }

        if self.is_logical_literal_constant_start() {
            return true;
        }

        if self.is_char_literal_constant_start() {
            return true;
        }

        if self.is_boz_literal_constant() {
            return true;
        }

        false
    }

    /// R709: kind-param
    #[inline]
    pub fn is_kind_param_start(&self) -> bool {
        match self {
            TokenKind::DigitString => true,
            t if t.is_name() => true,
            _ => false,
        }
    }

    /// R712: sign
    ///     is +
    ///     or -
    #[inline]
    pub fn is_sign(&self) -> bool {
        match self {
            TokenKind::Plus | TokenKind::Minus => true,
            _ => false,
        }
    }

    /// R718: complex-literal-constant
    #[inline]
    pub fn is_complex_literal_constant_start(&self) -> bool {
        *self == TokenKind::LeftParen
    }

    /// R724: char-literal-constant
    #[inline]
    pub fn is_char_literal_constant_start(&self) -> bool {
        if self.is_kind_param_start() {
            return true;
        }

        *self == TokenKind::CharLiteralConstant
    }

    /// R725: logical-literal-constant
    #[inline]
    pub fn is_logical_literal_constant_start(&self) -> bool {
        match self {
            TokenKind::True | TokenKind::False => true,
            _ => false,
        }
    }

    /// R764: boz-literal-constant
    #[inline]
    pub fn is_boz_literal_constant(&self) -> bool {
        match self {
            TokenKind::BinaryConstant | TokenKind::OctalConstant | TokenKind::HexConstant => true,
            _ => false,
        }
    }

    /// R769: array-constructor
    #[inline]
    pub fn is_array_constructor_start(&self) -> bool {
        *self == TokenKind::LeftBracket || *self == TokenKind::LeftParenSlash
    }

    /// R901: designator
    #[inline]
    pub fn is_designator_start(&self) -> bool {
        // self.is_name() || self.is_array_element_start() || self.is_array_section_start()
        //     || self.is_coindexed_named_object_start() || self.is_complex_part_designator_start()
        //     || self.is_structure_component_start() || self.is_substring_start()

        // designators always start with a name
        self.is_name()
    }

    /// R908: substring
    #[inline]
    pub fn is_substring_start(&self) -> bool {
        self.is_parent_string_start()
    }

    /// R909: parent-string
    #[inline]
    pub fn is_parent_string_start(&self) -> bool {
        self.is_name()
    }

    /// R911: data-ref
    #[inline]
    pub fn is_data_ref_start(&self) -> bool {
        self.is_part_ref_start()
    }

    /// R912: part-ref
    #[inline]
    pub fn is_part_ref_start(&self) -> bool {
        self.is_name()
    }

    /// R913: structure-component
    #[inline]
    pub fn is_structure_component_start(&self) -> bool {
        self.is_data_ref_start()
    }

    /// R914: coindexed-named-object
    #[inline]
    pub fn is_coindexed_named_object_start(&self) -> bool {
        self.is_data_ref_start()
    }

    /// R915: complex-part-designator
    #[inline]
    pub fn is_complex_part_designator_start(&self) -> bool {
        self.is_data_ref_start()
    }

    /// R917: array-element
    #[inline]
    pub fn is_array_element_start(&self) -> bool {
        self.is_data_ref_start()
    }

    /// R918: array-section
    #[inline]
    pub fn is_array_section_start(&self) -> bool {
        self.is_data_ref_start() || self.is_complex_part_designator_start()
    }

    /// R1001: primary
    #[inline]
    pub fn is_primary_start(&self) -> bool {
        self.is_literal_constant_start()
            || self.is_name() // covers designator, structure-constructor, function-reference, 
                              // type-param-inquiry, type-param-name
            || self.is_array_constructor_start()
            || *self == TokenKind::LeftParen
    }

    /// R1002: level-1-expr
    #[inline]
    pub fn is_level_1_expr_start(&self) -> bool {
        self.is_primary_start() || *self == TokenKind::DefinedOperator
    }

    /// R1004: mult-operand
    #[inline]
    pub fn is_mult_operand_start(&self) -> bool {
        self.is_level_1_expr_start()
    }

    /// R1005: add-operand
    #[inline]
    pub fn is_add_operand_start(&self) -> bool {
        self.is_mult_operand_start()
    }

    /// R1006: level-2-expr
    #[inline]
    pub fn is_level_2_expr_start(&self) -> bool {
        self.is_add_op() || self.is_add_operand_start()
    }

    /// R1007: power-op
    #[inline]
    pub fn is_power_op(&self) -> bool {
        *self == TokenKind::StarStar
    }

    /// R1008: mult-op
    #[inline]
    pub fn is_mult_op(&self) -> bool {
        match self {
            TokenKind::Star | TokenKind::Slash => true,
            _ => false,
        }
    }

    /// R1009: add-op
    #[inline]
    pub fn is_add_op(&self) -> bool {
        match self {
            TokenKind::Plus | TokenKind::Minus => true,
            _ => false,
        }
    }

    /// R1010: level-3-expr
    #[inline]
    pub fn is_level_3_expr_start(&self) -> bool {
        self.is_level_2_expr_start()
    }

    /// R1012: level-4-expr
    #[inline]
    pub fn is_level_4_expr_start(&self) -> bool {
        self.is_level_3_expr_start()
    }

    /// R1014: and-operand
    #[inline]
    pub fn is_and_operand_start(&self) -> bool {
        self.is_level_4_expr_start() || *self == TokenKind::NotOp
    }

    /// R1015: or-operand
    #[inline]
    pub fn is_or_operand_start(&self) -> bool {
        self.is_and_operand_start()
    }

    /// R1016: equiv-operand
    #[inline]
    pub fn is_equiv_operand_start(&self) -> bool {
        self.is_or_operand_start()
    }

    /// R1017: level-5-expr
    #[inline]
    pub fn is_level_5_expr_start(&self) -> bool {
        self.is_equiv_operand_start()
    }

    /// R1022: expr
    #[inline]
    pub fn is_expr_start(&self) -> bool {
        self.is_level_5_expr_start()
    }

    #[inline]
    pub fn is_intrinsic_type_spec_start(&self) -> bool {
        match self {
            TokenKind::Keyword(KeywordTokenKind::Integer)
            | TokenKind::Keyword(KeywordTokenKind::Real)
            | TokenKind::Keyword(KeywordTokenKind::Double)
            | TokenKind::Keyword(KeywordTokenKind::DoublePrecision)
            | TokenKind::Keyword(KeywordTokenKind::Complex)
            | TokenKind::Keyword(KeywordTokenKind::Character)
            | TokenKind::Keyword(KeywordTokenKind::Logical) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_declaration_type_spec_start(&self) -> bool {
        if self.is_intrinsic_type_spec_start() {
            return true;
        }

        match self {
            TokenKind::Keyword(KeywordTokenKind::Type)
            | TokenKind::Keyword(KeywordTokenKind::Class) => true,
            _ => false,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, FromPrimitive, ToPrimitive, PartialOrd, Ord)]
pub enum Letter {
    A = 0,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, FromPrimitive, ToPrimitive, PartialOrd, Ord)]
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
    // C, // Parsed as a letter
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
    pub fn all() -> impl Iterator<Item = Self> {
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
            KeywordTokenKind::all().map(
                |kind| (format!("{:?}", kind).to_lowercase(), kind)))
    };
}

pub const INTRINSIC_OPERATORS: &'static [(&'static str, TokenKind)] = {
    use self::TokenKind::*;

    &[
        ("AND", AndOp),
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
