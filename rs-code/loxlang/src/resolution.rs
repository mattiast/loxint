use std::collections::HashMap;

use crate::syntax::{
    Declaration, Expression, ForLoopDef, Program, Statement, VResolution, VarId, Variable,
    VariableDecl,
};

pub fn resolve<'src>(
    x: Program<'src, &'src str, &'src str>,
) -> Result<Program<'src, VResolution, VarId>, ()> {
    let mut resolver = Resolver {
        scopes: vec![HashMap::from([("clock", 1)])],
        next_id: 2,
    };
    return resolver.resolve_program(x);
}
pub fn resolve_expr_no_var<'src>(
    x: Expression<'src, &'src str>,
) -> Result<Expression<'src, VResolution>, ()> {
    let resolver = Resolver {
        scopes: vec![],
        next_id: 0,
    };
    return resolver.resolve_expr(x);
}

struct Resolver<'src> {
    // TODO how about `clock`, it needs some default mapping or such
    scopes: Vec<HashMap<&'src str, VarId>>,
    next_id: VarId,
}

impl<'src> Resolver<'src> {
    fn find_variable(&self, name: &'src str) -> Option<VResolution> {
        self.scopes
            .iter()
            .rev()
            .enumerate()
            .find_map(|(depth, scope)| scope.get(name).map(|id| (*id, depth)))
    }
    fn declare_variable(
        &mut self,
        VariableDecl(name): VariableDecl<&'src str>,
    ) -> VariableDecl<VarId> {
        let id = self.next_id;
        self.next_id += 1;
        self.scopes.last_mut().unwrap().insert(name, id);
        VariableDecl(id)
    }
    fn resolve_expr(
        &self,
        x: Expression<'src, &'src str>,
    ) -> Result<Expression<'src, VResolution>, ()> {
        match x {
            Expression::Nil => Ok(Expression::Nil),
            Expression::StringLiteral(s) => Ok(Expression::StringLiteral(s)),
            Expression::NumberLiteral(n) => Ok(Expression::NumberLiteral(n)),
            Expression::BooleanLiteral(b) => Ok(Expression::BooleanLiteral(b)),
            Expression::Unary { operator, right } => Ok(Expression::Unary {
                operator,
                right: Box::new(self.resolve_expr(*right)?),
            }),
            Expression::Binary {
                left,
                operator,
                right,
            } => Ok(Expression::Binary {
                left: Box::new(self.resolve_expr(*left)?),
                operator,
                right: Box::new(self.resolve_expr(*right)?),
            }),
            Expression::FunctionCall(f, args) => Ok(Expression::FunctionCall(
                Box::new(self.resolve_expr(*f)?),
                args.into_iter()
                    .map(|a| self.resolve_expr(a))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Expression::Assignment(Variable(v), e) => {
                let r = self.find_variable(v).ok_or(())?;
                let e = self.resolve_expr(*e)?;
                Ok(Expression::Assignment(Variable(r), Box::new(e)))
            }
            Expression::Identifier(Variable(v)) => {
                let r = self.find_variable(v).ok_or(())?;
                Ok(Expression::Identifier(Variable(r)))
            }
        }
    }
    fn resolve_statement(
        &mut self,
        x: Statement<'src, &'src str, &'src str>,
    ) -> Result<Statement<'src, VResolution, VarId>, ()> {
        match x {
            Statement::Expression(e) => self.resolve_expr(e).map(Statement::Expression),
            Statement::Print(e) => self.resolve_expr(e).map(Statement::Print),
            Statement::If(e, s1, s2) => {
                let e = self.resolve_expr(e)?;
                let s1 = self.resolve_statement(*s1)?;
                let s2 = s2.map(|s| self.resolve_statement(*s)).transpose()?;
                Ok(Statement::If(e, Box::new(s1), s2.map(Box::new)))
            }
            Statement::While(e, s) => {
                let e = self.resolve_expr(e)?;
                let s = self.resolve_statement(*s)?;
                Ok(Statement::While(e, Box::new(s)))
            }
            Statement::For(
                ForLoopDef {
                    var_name,
                    start,
                    cond,
                    increment,
                },
                s,
            ) => {
                let start = start.map(|e| self.resolve_expr(e)).transpose()?;
                self.scopes.push(HashMap::new());
                let var_name = var_name.map(|v| self.declare_variable(v));
                let cond = cond.map(|e| self.resolve_expr(e)).transpose()?;
                let increment = increment.map(|e| self.resolve_expr(e)).transpose()?;

                let s = self.resolve_statement(*s)?;
                self.scopes.pop();
                Ok(Statement::For(
                    ForLoopDef {
                        var_name,
                        start,
                        cond,
                        increment,
                    },
                    Box::new(s),
                ))
            }
            Statement::Block(decls) => {
                self.scopes.push(HashMap::new());
                let decls: Vec<_> = decls
                    .into_iter()
                    .map(|d| self.resolve_declaration(d))
                    .collect::<Result<Vec<_>, _>>()?;
                self.scopes.pop();

                Ok(Statement::Block(decls))
            }
        }
    }
    fn resolve_declaration(
        &mut self,
        x: Declaration<'src, &'src str, &'src str>,
    ) -> Result<Declaration<'src, VResolution, VarId>, ()> {
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
        x: Program<'src, &'src str, &'src str>,
    ) -> Result<Program<'src, VResolution, VarId>, ()> {
        Ok(Program {
            decls: x
                .decls
                .into_iter()
                .map(|d| self.resolve_declaration(d))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}
