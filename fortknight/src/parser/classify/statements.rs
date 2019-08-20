use crate::intern::InternedName;
use crate::span::Span;

#[derive(PartialEq, Debug)]
pub struct Spanned<T> {
    pub val: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(val: T, span: Span) -> Self {
        Spanned { val, span }
    }
}

#[derive(PartialEq, Debug)]
pub enum StmtKind {
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
    EndModule {name: Option<Spanned<InternedName>> },

    /// Error
    Unclassifiable,
}

pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}
