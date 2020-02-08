use crate::intern::InternedName;
use crate::num::Uint;
use crate::parser::lex::Letter;
use crate::span::Span;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Spanned<T>
where
    T: Copy + Clone,
{
    pub val: T,
    pub span: Span,
}

impl<T: Copy + Clone> Spanned<T> {
    pub fn new(val: T, span: Span) -> Self {
        Spanned { val, span }
    }
}

/// R605: literal-constant
///     is int-literal-consant
///     or real-literal-constant
///     or complex-literal-constant
///     or logical-literal-constant
///     or char-literal-constant
///     or boz-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LiteralConstant<'a> {
    IntLiteralConstant(IntLiteralConstant<'a>),
    RealLiteralConstant(RealLiteralConstant<'a>),
    ComplexLiteralConstant(ComplexLiteralConstant<'a>),
    LogicalLiteralConstant(LogicalLiteralConstant<'a>),
    CharLiteralConstant(CharLiteralConstant<'a>),
    BozLiteralConstant(BozLiteralConstant<'a>),
}

/// R608: intrinsic-operator
///     is power-op
///     or mult-op
///     or add-op
///     or concat-op
///     or rel-op
///     or not-op
///     or and-op
///     or or-op
///     or equiv-op
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IntrinsicOperator {
    PowerOp(PowerOp),
    MultOp(MultOp),
    AddOp(AddOp),
    ConcatOp(ConcatOp),
    RelOp(RelOp),
    NotOp(NotOp),
    AndOp(AndOp),
    OrOp(OrOp),
    EquivOp(EquivOp),
}

/// R609: defined-operator
///     is defined-unary-op
///     or defined-binary-op
///     or extended-intrinsic-op
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum DefinedOperator {
    DefinedUnaryOrBinaryOp(InternedName),
    ExtendedIntrinsicOp(IntrinsicOperator),
}

/// R701: type-param-value
///     is scalar-int-expr
///     or *
///     or :
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TypeParamValue<'a> {
    ScalarIntExpr(&'a Expr<'a>),
    Star,
    Colon,
}

/// R702: type-spec
///     is intrinsic-type-spec
///     or derived-type-spec
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TypeSpec<'a> {
    Intrinsic(IntrinsicTypeSpec<'a>),
    Derived(DerivedTypeSpec<'a>),
}

/// R703: declaration-type-spec
///     is intrinsic-type-spec
///     or TYPE ( intrinsic-type-spec )
///     or TYPE ( derived-type-spec )
///     or CLASS ( derived-type-spec )
///     or CLASS ( * )
///     or TYPE ( * )
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum DeclarationTypeSpec<'a> {
    Intrinsic(IntrinsicTypeSpec<'a>),
    TypeIntrinsic(IntrinsicTypeSpec<'a>),
    TypeDerived(DerivedTypeSpec<'a>),
    ClassDerived(DerivedTypeSpec<'a>),
    ClassWildcard,
    TypeWildcard,
}

/// R704: intrinsic-type-spec
///     is integer-type-spec
///     or REAL [ kind-selector ]
///     or DOUBLE PRECISION
///     or COMPLEX [ kind-selector ]
///     or CHARACTER [ kind-selector ]
///     or LOGICAL [ kind-selector ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IntrinsicTypeSpec<'a> {
    Integer(IntegerTypeSpec<'a>),
    Real(Option<KindSelector<'a>>),
    DoublePrecision,
    Complex(Option<KindSelector<'a>>),
    Character(Option<KindSelector<'a>>),
    Logical(Option<KindSelector<'a>>),
}

/// R705: integer-type-spec is INTEGER [ kind-selector ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct IntegerTypeSpec<'a>(pub Option<KindSelector<'a>>);

/// R706: kind-selector is ( [ KIND = ] scalar-int-constant-expr )
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct KindSelector<'a>(pub &'a Expr<'a>);

/// R707: signed-int-literal-constant is [ sign ] int-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct SignedIntLiteralConstant<'a> {
    pub sign: Option<Sign>,
    pub int_literal_constant: IntLiteralConstant<'a>,
}

/// R708: int-literal-constant is digit-string [ _ kind-param ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct IntLiteralConstant<'a> {
    pub digit_string: Uint<'a>,
    pub kind_param: Option<KindParam<'a>>,
}

/// R709: kind-param
///     is digit-string
///     or scalar-int-constant-name
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum KindParam<'a> {
    DigitString(Uint<'a>),
    ScalarIntConstantName(InternedName),
}

/// R710: signed-digit-string is [ sign ] digit-string
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct SignedDigitString<'a> {
    pub sign: Option<Sign>,
    pub digit_string: DigitString<'a>,
}

/// R711: digit-string is digit [ digit ] ...
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct DigitString<'a>(pub Uint<'a>);

/// R712: sign
///     is +
///     or -
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Sign {
    Plus,
    Minus,
}

/// R713: signed-real-literal-constant is [ sign ] real-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct SignedRealLiteralConstant<'a> {
    pub sign: Option<Sign>,
    pub real_literal_constant: RealLiteralConstant<'a>,
}

/// R714: real-literal-constant
///     is signficand [ exponent-letter exponent ] [ _ kind-param ]
///     or digit-string exponent-letter exponent [ _ kind-param ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct RealLiteralConstant<'a> {
    pub significand: Significand<'a>,
    pub exponent_part: Option<ExponentPart<'a>>,
    pub kind_param: Option<KindParam<'a>>,
}

/// R714: exponent part of real-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ExponentPart<'a> {
    pub letter: ExponentLetter,
    pub exponent: Exponent<'a>,
}

/// R715: significand
///     is digit-string . [ digit-string ]
///     or . digit-string
///
/// We also capture simple "digit-string" (no .) with this
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Significand<'a> {
    pub integer: Option<Uint<'a>>,
    pub decimal: Option<Uint<'a>>,
}

/// R716: exponent-letter
///     is E
///     or D
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ExponentLetter {
    E,
    D,
}

/// R717: exponent is signed-digit-string
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Exponent<'a>(pub SignedDigitString<'a>);

/// R718: complex-literal-constant is ( real-part , imag-part )
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ComplexLiteralConstant<'a> {
    pub real_part: ComplexLiteralPart<'a>,
    pub imag_part: ComplexLiteralPart<'a>,
}

/// R719/R720: real-part or imag-part
///     is signed-int-literal-constant
///     or signed-real-literal-constant
///     or named-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ComplexLiteralPart<'a> {
    SignedIntLiteralConstant(SignedIntLiteralConstant<'a>),
    SignedRealLiteralConstant(SignedRealLiteralConstant<'a>),
    NamedConstant(InternedName),
}

/// R724: char-literal-constant
///     is [ kind-param ] ' [ rep-char ] ... '
///     or [ kind-param ] " [ rep-char ] ... "
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct CharLiteralConstant<'a> {
    pub kind_param: Option<KindParam<'a>>,
    pub string: &'a str,
}

/// R725: logical-literal-constant
///     is .TRUE. [ _ kind-param ]
///     or .FALSE. [ _ kind-param ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct LogicalLiteralConstant<'a> {
    pub value: bool,
    pub kind_param: Option<KindParam<'a>>,
}

/// R754: derived-type-spec is type-name [ ( type-param-spec-list ) ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct DerivedTypeSpec<'a> {
    pub name: InternedName,
    pub spec_list: &'a [TypeParamSpec<'a>],
}

/// R755: type-param-spec is [ keyword = ] type-param-value
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TypeParamSpec<'a> {
    pub keyword: Option<InternedName>,
    pub value: TypeParamValue<'a>,
}

/// R764: boz-literal-constant
///     is binary-constant
///     or octal-constant
///     or hex-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BozLiteralConstant<'a> {
    BinaryConstant(Uint<'a>),
    OctalConstant(Uint<'a>),
    HexConstant(Uint<'a>),
}

/// R769: array-constructor
///     is (/ ac-spec /)
///     or lbracket ac-spec rbracket
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ArrayConstructor<'a>(AcSpec<'a>);

/// R770: ac-spec
///     is type-spec ::
///     or [ type-sepc :: ] ac-value-list
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct AcSpec<'a> {
    type_spec: Option<TypeSpec<'a>>,
    ac_value_list: &'a [AcValue<'a>],
}

/// R773: ac-value
///     is expr
///     or ac-implied-do
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AcValue<'a> {
    Expr(&'a Expr<'a>),
    AcImpliedDo(AcImpliedDo<'a>),
}

/// R774: ac-implied-do is ( ac-value-list , ac-implied-do-control )
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct AcImpliedDo<'a> {
    ac_value_list: &'a [AcValue<'a>],
    ac_implied_do_control: AcImpliedDoControl<'a>,
}

/// R775: ac-implied-do-control
///     is [ integer-type-spec :: ] ac-do-variable = scalar-int-expr , scalar-int-expr
///         [ , scalar-int-expr ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct AcImpliedDoControl<'a> {
    integer_type_spec: Option<IntegerTypeSpec<'a>>,
    ac_do_variable: AcDoVariable,
    start: &'a Expr<'a>,
    end: &'a Expr<'a>,
    stride: Option<&'a Expr<'a>>,
}

/// R776: ac-do-variable is do-variable
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct AcDoVariable(DoVariable);

/// R863: implicit-stmt
///     is IMPLICIT implicit-spec-list
///     or IMPLICIT NONE [ ( [ implicit-none-spec-list ] ) ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ImplicitStmt<'a> {
    SpecList(&'a [Spanned<ImplicitSpec<'a>>]),
    /// R866: implicit-none-spec
    ///     is EXTERNAL
    ///     or TYPE
    NoneSpecList {
        has_external: bool,
        has_type: bool,
    },
}

/// R864: implicit-spec is declaration-type-spec ( letter-spec-list )
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ImplicitSpec<'a> {
    pub declaration_type_spec: DeclarationTypeSpec<'a>,
    pub letter_spec_list: &'a [LetterSpec],
}

/// R865: letter-spec is letter [ - letter ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct LetterSpec {
    pub start: Letter,
    pub end: Option<Letter>,
}

/// R867: import-stmt
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ImportStmt<'a> {
    NoSpecifier(&'a [Spanned<InternedName>]),
    OnlySpecifier(&'a [Spanned<InternedName>]),
    AllSpecifier,
    NoneSpecifier,
}

/// R901: designator
///     is object-name
///     or array-element
///     or array-section
///     or coindexed-named-object
///     or complex-part-designator
///     or structure-component
///     or substring
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Designator<'a> {
    ObjectName(InternedName),
    ArrayElement(ArrayElement<'a>),
}

/// R902: variable
///     is designator
///     or function-reference
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Variable<'a> {
    DesignatorOrFunctionReference(Designator<'a>),
}

/// R911: data-ref is part-ref [ % part-ref ] ...
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct DataRef<'a>(&'a [PartRef<'a>]);

/// R912: part-ref is part-name [ ( section-subscript-list ) ] [ image-selector ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct PartRef<'a> {
    name: InternedName,
    section_subscripts: &'a [SectionSubscript<'a>],
    image_selector: Option<ImageSelector<'a>>,
}

/// R917: array-element is data-ref
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ArrayElement<'a>(DataRef<'a>);

/// R919: subscript is scalar-int-expr
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Subscript<'a>(&'a Expr<'a>);

/// R920: section-subscript
///     is subscript
///     or subscript-triplet
///     or vector-subscript
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SectionSubscript<'a> {
    Subscript(Subscript<'a>),
    SubscriptTriplet(SubscriptTriplet<'a>),
    VectorSubscript(VectorSubscript<'a>),
}

/// R921: subscript-triplet is [ subscript ] : [ subscript ] [ : stride ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct SubscriptTriplet<'a> {
    start: Option<Subscript<'a>>,
    end: Option<Subscript<'a>>,
    stride: Option<Stride<'a>>,
}

/// R922: stride is scalar-int-expr
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Stride<'a>(&'a Expr<'a>);

/// R923: vector-subscript is int-expr
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct VectorSubscript<'a>(&'a Expr<'a>);

/// R924: image-selector is lbracket cosubscript-list [ , image-selector-spec-list ] rbracket
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ImageSelector<'a> {
    cosubscript_list: &'a [Cosubscript<'a>],
    image_selector_spec_lsit: &'a [ImageSelectorSpec<'a>],
}

/// R925: cosubscript is scalar-int-expr
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Cosubscript<'a>(&'a Expr<'a>);

/// R926: image-selector-spec
///     is STAT = stat-variable
///     or TEAM = team-value
///     or TEAM_NUMBER = scalar-int-expr
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ImageSelectorSpec<'a> {
    Stat(StatVariable<'a>),
    Team(TeamValue<'a>),
    TeamNumber(&'a Expr<'a>),
}

/// R942: stat-variable is scalar-int-variable
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct StatVariable<'a>(Variable<'a>);

/// R1001: primary
///     is literal-constant
///     or designator
///     or array-constructor
///     or structure-constructor
///     or function-reference
///     or type-param-inquiry
///     or type-param-name
///     or ( expr )
///
/// A primary as it appears in the classification stage, before we can differentiate between, say,
/// function names and type names
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PrimaryRaw<'a> {
    LiteralConstant(LiteralConstant<'a>),
    /// Could be a designator, structure-constructor, function-reference, or type-param-inquiry
    Designator,
    ArrayConstructor(ArrayConstructor<'a>),
    TypeParamName(InternedName),
}

/// R1007: power-op is **
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct PowerOp;

/// R1008: mult-op is *
///                or /
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MultOp {
    Multiply,
    Divide,
}

/// R1009: add-op is +
///               or -
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AddOp {
    Plus,
    Minus,
}

/// R1011: concat-op is //
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ConcatOp;

/// R1013: rel-op
///     is .EQ.
///     or .NE.
///     or .LT.
///     or .LE.
///     or .GT.
///     or .GE.
///     or ==
///     or /=
///     or <
///     or <=
///     or >
///     or >=
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RelOp {
    EqualNamed,
    NotEqualNamed,
    LessThanNamed,
    LessThanOrEqualNamed,
    GreaterThanNamed,
    GreaterThanOrEqualNamed,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

/// R1018: not-op is .NOT.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct NotOp;

/// R1019: and-op is .AND.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct AndOp;

/// R1020: or-op is .OR.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct OrOp;

/// R1021: equiv-op
///     is .EQV.
///     or .NEQV.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum EquivOp {
    Equivalence,
    NonEquivalence,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Defined(InternedName),
    Equiv,
    NotEquiv,
    Or,
    And,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEquals,
    GreaterThan,
    GreaterThanOrEquals,
    Concat,
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Not,
    Plus,
    Minus,
    Defined(InternedName),
}

/// R1022: expr is [ expr defined-binary-op ] level-5-expr
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    BinaryOperation {
        lhs: &'a Expr<'a>,
        op: BinaryOp,
        rhs: &'a Expr<'a>,
    },
    UnaryOperation {
        op: UnaryOp,
        target: &'a Expr<'a>,
    },
    Primary(PrimaryRaw<'a>),
}

/// R1032: assignment-stmt is variable = expr
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct AssignmentStmt<'a> {
    pub variable: Spanned<Variable<'a>>,
    pub expr: Spanned<&'a Expr<'a>>,
}

/// R1115: team-value is scalar-expr
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TeamValue<'a>(&'a Expr<'a>);

/// R1124: do-variable is scalar-int-variable-name
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct DoVariable(InternedName);

/// R1301: format-stmt is FORMAT format-specification
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FormatStmt<'a>(FormatSpecification<'a>);

/// R1302: format-specification
///     is ( [ format-items ] )
///     or ( [ format-items , ] unlimited-format-item)
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FormatSpecification<'a> {
    format_items: &'a [FormatItem<'a>],
    unlimited_format_item: Option<UnlimitedFormatItem<'a>>,
}

/// R1304: format-item
///     is [ r ] data-edit-desc
///     or control-edit-desc
///     or char-string-edit-desc
///     or [ r ] ( format-items )
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FormatItem<'a> {
    DataEditDesc(Option<R<'a>>, DataEditDesc<'a>),
    ControlEditDesc(ControlEditDesc<'a>),
    CharStringEditDesc(CharStringEditDesc<'a>),
    Items(Option<R<'a>>, &'a [FormatItem<'a>]),
}

/// R1305: unlimited-format-item is * ( format-items )
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct UnlimitedFormatItem<'a> {
    format_items: &'a [FormatItem<'a>],
}

/// R1306: r is int-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct R<'a>(IntLiteralConstant<'a>);

/// R1307: data-edit-desc
///     is I w [ . m ]
///     or B w [ . m ]
///     or O w [ . m ]
///     or Z w [ . m ]
///     or F w . d
///     or E w . d [ E e ]
///     or EN w . d [ E e ]
///     or ES w . d [ E e ]
///     or EX w . d [ E e ]
///     or G w [ . d [ E e ] ]
///     or L w
///     or A [ w ]
///     or D w . d
///     or DT [ char-literal-constant ] [ ( v-list ) ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum DataEditDesc<'a> {
    I(W<'a>, Option<M<'a>>),
    B(W<'a>, Option<M<'a>>),
    O(W<'a>, Option<M<'a>>),
    Z(W<'a>, Option<M<'a>>),
    F(W<'a>, D<'a>),
    E(W<'a>, D<'a>, Option<E<'a>>),
    EN(W<'a>, D<'a>, Option<E<'a>>),
    ES(W<'a>, D<'a>, Option<E<'a>>),
    EX(W<'a>, D<'a>, Option<E<'a>>),
    G(W<'a>, Option<(D<'a>, Option<E<'a>>)>),
    L(W<'a>),
    A(Option<W<'a>>),
    D(W<'a>, D<'a>),
    DT(Option<CharLiteralConstant<'a>>, Option<&'a [V<'a>]>),
}

/// R1308: w is int-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct W<'a>(IntLiteralConstant<'a>);

/// R1309: m is int-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct M<'a>(IntLiteralConstant<'a>);

/// R1310: d is int-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct D<'a>(IntLiteralConstant<'a>);

/// R1311: e is int-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct E<'a>(IntLiteralConstant<'a>);

/// R1312: v is signed-int-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct V<'a>(SignedIntLiteralConstant<'a>);

/// R1313: control-edit-desc
///     is position-edit-desc
///     or [ r ] /
///     or :
///     or sign-edit-desc
///     or k P
///     or blank-interp-edit-desc
///     or round-edit-desc
///     or decimal-edit-desc
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ControlEditDesc<'a> {
    PositionEditDesc(PositionEditDesc<'a>),
    Slash(Option<R<'a>>),
    Colon,
    SignEditDesc(SignEditDesc),
    P(K<'a>),
    BlackInterpEditDesc(BlankInterpEditDesc),
    RoundEditDesc(RoundEditDesc),
    DecimalEditDesc(DecimalEditDesc),
}

/// R1314: k is signed-int-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct K<'a>(SignedIntLiteralConstant<'a>);

/// R1315: position-edit-desc
///     is T n
///     or TL n
///     or TR n
///     or n X
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PositionEditDesc<'a> {
    T(N<'a>),
    TL(N<'a>),
    TR(N<'a>),
    X(N<'a>),
}

/// R1316: n is int-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct N<'a>(IntLiteralConstant<'a>);

/// R1317: sign-edit-desc
///     is SS
///     or SP
///     or S
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SignEditDesc {
    SS,
    SP,
    S,
}

/// R1318: blank-interp-edit-desc
///     is BN
///     or BZ
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BlankInterpEditDesc {
    BN,
    BZ,
}

/// R1319: round-edit-desc
///     is RU
///     or RD
///     or RZ
///     or RN
///     or RC
///     or RP
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RoundEditDesc {
    RU,
    RD,
    RZ,
    RN,
    RC,
    RP,
}

/// R1320: decimal-edit-desc
///     is DC
///     or DP
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum DecimalEditDesc {
    DC,
    DP,
}

/// R1321: char-string-edit-desc is char-literal-constant
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct CharStringEditDesc<'a>(CharLiteralConstant<'a>);

/// R1418: parent-identifer is ancestore-module-name [ : parent-submodule-name ]
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ParentIdentifier {
    pub ancestor_module_name: InternedName,
    pub parent_submodule_name: Option<InternedName>,
}

/// R1409: use-stmt: Can have 'rename-list' or 'only-list' or neither
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ModuleImportList<'a> {
    Unspecified,
    RenameList(&'a [Spanned<Rename>]),
    OnlyList(&'a [Spanned<Only>]),
}

/// R1410: module-nature is INTRINSIC or NON_INTRINSIC
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ModuleNature {
    Unspecified,
    Intrinsic,
    NonIntrinsic,
}

/// R1411: rename
///     is local-name => use-name
///     or OPERATOR (local-defined-operator) => OPERATOR (use-defined-operator)
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Rename {
    Name {
        from: InternedName,
        to: InternedName,
    },
    Operator {
        from: InternedName,
        to: InternedName,
    },
}

/// R1412: only
///     is generic-spec
///     or only-use-name
///     or rename
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Only {
    /// We can't tell the difference between `generic-name` in `generic-spec` from `only-use-name`
    /// at this point, so that must be determined later.
    GenericOrOnlyUseName(InternedName),
    GenericSpec(GenericSpec),
    // OnlyUseName(InternedName),
    Rename(Rename),
}

/// R1508: generic-spec
///     is generic-name
///     or OPERATOR ( defined-operator )
///     or ASSIGNMENT ( = )
///     or defined-io-generic-spec
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum GenericSpec {
    GenericName(InternedName),
    Operator(DefinedOperator),
    Assignment,
    DefinedIoGenericSpec(DefinedIoGenericSpec),
}

/// R1509: defined-io-generic-spec
///     is READ ( FORMATTED )
///     or READ ( UNFORMATTED )
///     or WRITE ( FORMATTED )
///     or WRITE ( UNFORMATTED )
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum DefinedIoGenericSpec {
    ReadFormatted,
    ReadUnformatted,
    WriteFormatted,
    WriteUnformatted,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum StmtKind<'a> {
    // Bare statements
    /// Bare end statement - just `end`
    End,

    /// R863: implicit-stmt
    ///     is IMPLICIT implicit-spec-list
    ///     or IMPLICIT NONE [ ( [ implicit-none-spec-list ] ) ]
    Implicit(ImplicitStmt<'a>),

    /// R867: import-stmt
    ///     is IMPORT [ [ :: ] import-name-list ]
    ///     or IMPORT, ONLY : import-name-list
    ///     or IMPORT, NONE
    ///     or IMPORT, ALL
    Import(ImportStmt<'a>),

    /// R1032: assignment-stmt
    ///     is variable = expr
    Assignment(AssignmentStmt<'a>),

    /// R1108: block-stmt
    ///     is [ block-construct-name : ] BLOCK
    Block {
        name: Option<Spanned<InternedName>>,
    },

    /// R1110: end-block-stmt
    ///     is END BLOCK [ block-construct-name ]
    EndBlock {
        name: Option<Spanned<InternedName>>,
    },

    /// R1301: format-stmt
    ///     is FORMAT format-specification
    Format(FormatStmt<'a>),

    /// R1402: program-stmt is `program name?`
    Program {
        name: Option<Spanned<InternedName>>,
    },
    /// R1403: end-program-stmt is `end program name?` or `endprogram name?`
    EndProgram {
        name: Option<Spanned<InternedName>>,
    },

    /// R1405: module-stmt is `module name`
    Module {
        name: Spanned<InternedName>,
    },
    /// R1406: end-module-stmt is `end module name?` or `endmodule name?`
    EndModule {
        name: Option<Spanned<InternedName>>,
    },

    /// R1409: use-stmt
    ///     is USE [ [ , module-nature ] :: ] module-name [ , rename-list ]
    ///     or USE [ [ , module-nature ] :: ] module-name , ONLY : [ only-list ]
    Use {
        module_nature: ModuleNature,
        name: InternedName,
        imports: ModuleImportList<'a>,
    },

    /// R1417: submodule-stmt is `submodule ( parent-identifier ) submodule-name`
    Submodule {
        parent_identifier: ParentIdentifier,
        name: Spanned<InternedName>,
    },

    /// R1419: end-submodule-stmt is `end submodule name?` or `endsubmodule name?`
    EndSubmodule {
        name: Option<Spanned<InternedName>>,
    },

    /// R1421: block-data-stmt
    ///     is BLOCK DATA [ block-data-name ]
    BlockData {
        name: Option<Spanned<InternedName>>,
    },

    /// R1422: end-block-data-stmt
    ///     is END [ BLOCK DATA [ block-data-name ] ]
    EndBlockData {
        name: Option<Spanned<InternedName>>,
    },

    /// R1543: contains-stmt is CONTAINS
    Contains,

    Empty,

    /// Error
    Unclassifiable,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub span: Span,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct LabeledStmt<'a> {
    pub stmt: Stmt<'a>,
    pub label: Option<Spanned<u32>>,
}
