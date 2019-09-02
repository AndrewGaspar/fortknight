use crate::intern::InternedName;
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
    ScalarIntExpr(Expr<'a>),
    Star,
    Colon,
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
pub struct KindSelector<'a>(pub Expr<'a>);

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

/// R1022: expr is [ expr defined-binary-op ] level-5-expr
/// TODO: Implement
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Expr<'a>(std::marker::PhantomData<&'a ()>);

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

    /// R1108: block-stmt
    ///     is [ block-construct-name : ] BLOCK
    Block { name: Option<Spanned<InternedName>> },

    /// R1110: end-block-stmt
    ///     is END BLOCK [ block-construct-name ]
    EndBlock { name: Option<Spanned<InternedName>> },

    /// R1402: program-stmt is `program name?`
    Program { name: Option<Spanned<InternedName>> },
    /// R1403: end-program-stmt is `end program name?` or `endprogram name?`
    EndProgram { name: Option<Spanned<InternedName>> },

    /// R1405: module-stmt is `module name`
    Module { name: Spanned<InternedName> },
    /// R1406: end-module-stmt is `end module name?` or `endmodule name?`
    EndModule { name: Option<Spanned<InternedName>> },

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
    EndSubmodule { name: Option<Spanned<InternedName>> },

    /// R1421: block-data-stmt
    ///     is BLOCK DATA [ block-data-name ]
    BlockData { name: Option<Spanned<InternedName>> },

    /// R1422: end-block-data-stmt
    ///     is END [ BLOCK DATA [ block-data-name ] ]
    EndBlockData { name: Option<Spanned<InternedName>> },

    /// R1543: contains-stmt is CONTAINS
    Contains,

    /// Error
    Unclassifiable,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub span: Span,
}
