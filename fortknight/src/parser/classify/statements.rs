use crate::intern::InternedName;
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

    /// Error
    Unclassifiable,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub span: Span,
}
