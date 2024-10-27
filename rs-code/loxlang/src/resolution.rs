use std::collections::HashMap;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::{
    execution_env::NativeFunc,
    parse::{ByteSpan, ParsedDeclaration, ParsedExpression, ParsedProgram, ParsedStatement},
    syntax::{
        AnnotatedExpression, AnnotatedStatement, Declaration, Expression, Program, Statement,
        Variable, VariableDecl,
    },
};

// TODO what about mutually recursive functions? On global and local level?
// Should functions be resolved differently?

pub type VarId = u64;
pub type VResolution = (VarId, usize);

pub type ResolvedExpression<'src> = AnnotatedExpression<'src, VResolution, ByteSpan>;
pub type ResolvedStatement<'src> = AnnotatedStatement<'src, VResolution, VarId, ByteSpan>;
pub type ResolvedDeclaration<'src> = Declaration<'src, VResolution, VarId, ByteSpan>;
pub type ResolvedProgram<'src> = Program<'src, VResolution, VarId, ByteSpan>;

// TODO use `miette::Error::with_source_code` instead of passing the source here
#[derive(Error, Debug, Diagnostic)]
#[error("resolution error")]
pub struct ResolutionError {
    #[source_code]
    src: String,
    #[label("Unknown variable")]
    span: SourceSpan,
    #[help]
    help: String,
}

pub fn resolve<'src>(
    x: ParsedProgram<'src>,
    src: &'src str,
) -> Result<ResolvedProgram<'src>, ResolutionError> {
    let mut resolver = Resolver {
        scopes: vec![NativeFunc::iter()
            .enumerate()
            .map(|(i, nf)| (nf.name(), i as u64))
            .collect()],

        next_id: NativeFunc::count() as u64,
        src,
    };
    return resolver.resolve_program(x);
}
pub fn resolve_expr_no_var<'src>(
    x: ParsedExpression<'src>,
    src: &'src str,
) -> Result<ResolvedExpression<'src>, ResolutionError> {
    let resolver = Resolver {
        scopes: vec![],
        next_id: 0,
        src,
    };
    return resolver.resolve_expr(x);
}

pub struct Resolver<'src> {
    // TODO how about `clock`, it needs some default mapping or such
    scopes: Vec<HashMap<&'src str, VarId>>,
    next_id: VarId,
    src: &'src str,
}

impl<'src> Resolver<'src> {
    pub fn new(src: &'src str) -> Self {
        Resolver {
            scopes: vec![NativeFunc::iter()
                .enumerate()
                .map(|(i, nf)| (nf.name(), i as u64))
                .collect()],

            next_id: NativeFunc::count() as u64,
            src,
        }
    }
    fn find_variable(&self, name: &'src str) -> Option<VResolution> {
        self.scopes
            .iter()
            .rev()
            .enumerate()
            .find_map(|(depth, scope)| scope.get(name).map(|id| (*id, depth)))
    }
    fn error(&self, name: &'src str) -> ResolutionError {
        let span_start = (name.as_ptr() as usize) - (self.src.as_ptr() as usize);
        ResolutionError {
            src: self.src.to_owned(),
            help: format!("variable '{}' not found", name),
            span: (span_start, name.len()).into(),
        }
    }
    fn declare_variable(
        &mut self,
        VariableDecl(name): VariableDecl<&'src str>,
    ) -> VariableDecl<VarId> {
        // If we want to deny shadowing, it would be checked here
        let id = self.next_id;
        self.next_id += 1;
        self.scopes.last_mut().unwrap().insert(name, id);
        VariableDecl(id)
    }
    fn resolve_expr(
        &self,
        x: ParsedExpression<'src>,
    ) -> Result<ResolvedExpression<'src>, ResolutionError> {
        let ann = x.annotation;
        match x.value {
            Expression::Nil => Ok(Expression::Nil.annotate(ann)),
            Expression::StringLiteral(s) => Ok(Expression::StringLiteral(s).annotate(ann)),
            Expression::NumberLiteral(n) => Ok(Expression::NumberLiteral(n).annotate(ann)),
            Expression::BooleanLiteral(b) => Ok(Expression::BooleanLiteral(b).annotate(ann)),
            Expression::Unary { operator, right } => Ok(Expression::Unary {
                operator,
                right: Box::new(self.resolve_expr(*right)?),
            }
            .annotate(ann)),
            Expression::Binary {
                left,
                operator,
                right,
            } => Ok(Expression::Binary {
                left: Box::new(self.resolve_expr(*left)?),
                operator,
                right: Box::new(self.resolve_expr(*right)?),
            }
            .annotate(ann)),
            Expression::FunctionCall(f, args) => Ok(Expression::FunctionCall(
                Box::new(self.resolve_expr(*f)?),
                args.into_iter()
                    .map(|a| Ok(self.resolve_expr(a)?))
                    .collect::<Result<Vec<_>, _>>()?,
            )
            .annotate(ann)),
            Expression::Assignment(Variable(v), e) => {
                let r = self.find_variable(v).ok_or_else(|| self.error(v))?;
                let e = self.resolve_expr(*e)?;
                Ok(Expression::Assignment(Variable(r), Box::new(e)).annotate(ann))
            }
            Expression::Identifier(Variable(v)) => {
                let r = self.find_variable(v).ok_or_else(|| self.error(v))?;
                Ok(Expression::Identifier(Variable(r)).annotate(ann))
            }
        }
    }
    fn resolve_statement(
        &mut self,
        stmt: ParsedStatement<'src>,
    ) -> Result<AnnotatedStatement<'src, VResolution, VarId, ByteSpan>, ResolutionError> {
        let ann = stmt.annotation;
        match stmt.value {
            Statement::Expression(e) => self
                .resolve_expr(e)
                .map(|e| Statement::Expression(e).annotate(ann)),
            Statement::Print(e) => self
                .resolve_expr(e)
                .map(|e| Statement::Print(e).annotate(ann)),
            Statement::If(e, s1, s2) => {
                let e = self.resolve_expr(e)?;
                let s1 = self.resolve_statement(*s1)?;
                let s2 = s2.map(|s| self.resolve_statement(*s)).transpose()?;
                Ok(Statement::If(e, Box::new(s1), s2.map(Box::new)).annotate(ann))
            }
            Statement::While(e, s) => {
                let e = self.resolve_expr(e)?;
                let s = self.resolve_statement(*s)?;
                Ok(Statement::While(e, Box::new(s)).annotate(ann))
            }
            Statement::Block(decls) => {
                self.scopes.push(HashMap::new());
                let decls: Vec<_> = decls
                    .into_iter()
                    .map(|d| self.resolve_declaration(d))
                    .collect::<Result<Vec<_>, _>>()?;
                self.scopes.pop();

                Ok(Statement::Block(decls).annotate(ann))
            }
            Statement::Return(expr) => self
                .resolve_expr(expr)
                .map(|e| Statement::Return(e).annotate(ann)),
        }
    }
    pub fn resolve_declaration(
        &mut self,
        x: ParsedDeclaration<'src>,
    ) -> Result<ResolvedDeclaration<'src>, ResolutionError> {
        match x {
            Declaration::Var(v, e) => {
                // First resolve the expression in the scope without the new variable
                let e = e.map(|e| self.resolve_expr(e)).transpose()?;
                let v = self.declare_variable(v);
                Ok(Declaration::Var(v, e))
            }
            Declaration::Function { name, args, body } => {
                let name = self.declare_variable(name);
                self.scopes.push(HashMap::new());
                let args = args.into_iter().map(|a| self.declare_variable(a)).collect();
                let body = self.resolve_statement(body)?;
                self.scopes.pop();
                Ok(Declaration::Function { name, args, body })
            }
            Declaration::Statement(s) => self.resolve_statement(s).map(Declaration::Statement),
        }
    }
    fn resolve_program(
        &mut self,
        x: ParsedProgram<'src>,
    ) -> Result<ResolvedProgram<'src>, ResolutionError> {
        Ok(Program {
            decls: x
                .decls
                .into_iter()
                .map(|d| self.resolve_declaration(d))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}
