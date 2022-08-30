use super::*;

// don't worry: look at the prettier API.
// all of these additions are NOT doing O(n^2) string concats.

pub trait Print: IRNode {
    fn print<'ir>(ir: &'ir IR, item: &Node<Self>) -> Doc<'ir>;
}

impl Print for Expr {
    fn print<'ir>(ir: &'ir IR, expr: &Node<Self>) -> Doc<'ir> {
        match &ir[expr] {
            Expr::UnOp { op, arg } => {
                Doc::Empty + *UnOp::get_tokens().left(op) + Print::print(ir, arg)
            }
            Expr::BinOp { op, left, right } => Doc::group(
                Print::print(ir, left)
                    + Doc::NewlineOrSpace
                    + *BinOp::get_tokens().left(op)
                    + Doc::NewlineOrSpace
                    + Print::print(ir, right),
            ),
            Expr::MultiOp { op, args } => Doc::group(
                args.iter()
                    .map(|node| Print::print(ir, node))
                    .intersperse(Doc::NewlineOrSpace + *MultiOp::get_tokens().left(op) + " ")
                    .reduce(|a, b| a + b)
                    .expect("multiops should not be empty"), // FIXME default to unit
            ),
            Expr::NullaryOp(op) => Doc::text(*NullaryOp::get_tokens().left(op)),
            Expr::IntConst(const_) => Doc::text(format!("{}", const_)),
            Expr::Invocation { function, args } => {
                Doc::text(&ir[function].name)
                    + "("
                    + Doc::group(Doc::indent(
                        4,
                        Doc::NewlineOrEmpty
                            + args
                                .iter()
                                .map(|node| Print::print(ir, node))
                                .intersperse(Doc::text(",") + Doc::NewlineOrSpace)
                                .reduce(|a, b| a + b)
                                .unwrap_or(Doc::Empty)
                            + Doc::NewlineOrEmpty,
                    ))
                    + ")"
            }
            Expr::DynamicInvocation {
                function_reference,
                args,
            } => {
                Doc::text("(")
                    + Print::print(ir, function_reference)
                    + ")"
                    + "("
                    + Doc::group(Doc::indent(
                        4,
                        Doc::NewlineOrEmpty
                            + args
                                .iter()
                                .map(|node| Print::print(ir, node))
                                .intersperse(Doc::text(",") + Doc::NewlineOrSpace)
                                .reduce(|a, b| a + b)
                                .unwrap_or(Doc::Empty)
                            + Doc::NewlineOrEmpty,
                    ))
                    + ")"
            }
            Expr::FunctionReference(function_reference) => Doc::text(&ir[function_reference].name),
            Expr::Var(var) => match &ir[var].name {
                VarName::Anonymous(id) => Doc::text(format!("{}", id)),
                VarName::Named(name) => Doc::text(name),
            },
        }
    }
}

impl Print for Var {
    fn print<'ir>(ir: &'ir IR, var: &Node<Var>) -> Doc<'ir> {
        match &ir[var].name {
            VarName::Anonymous(id) => Doc::text("$") + format!("{}", id),
            VarName::Named(name) => Doc::text(name),
        }
    }
}

impl Print for Stmt {
    fn print<'ir>(ir: &'ir IR, stmt: &Node<Self>) -> Doc<'ir> {
        match &ir[stmt] {
            Stmt::DerefAssignment { deref, expr } => {
                Doc::text("*") + Print::print(ir, deref) + " = " + Print::print(ir, expr) + ";"
            }
            Stmt::Assignment { var, value } => {
                Print::print(ir, var) + " = " + Print::print(ir, value) + ";"
            }
            Stmt::Print { expr } => Doc::text("print ") + Print::print(ir, expr) + ";",
            Stmt::Block { ops } =>
            // we don't add a group here to avoid flattening blocks to 1 line
            {
                Doc::text("{")
                    + Doc::indent(
                        4,
                        ops.iter()
                            .map(|stmt| Doc::NewlineOrSpace + Print::print(ir, stmt))
                            .reduce(|a, b| a + b)
                            .unwrap_or(Doc::Empty),
                    )
                    + Doc::NewlineOrSpace
                    + "}"
            }
            Stmt::If {
                condition,
                then,
                else_,
            } => {
                Doc::text("if (")
                    + Print::print(ir, condition)
                    + ") "
                    + Print::print(ir, then)
                    + else_
                        .map(|else_| Doc::text(" else ") + Print::print(ir, &else_))
                        .unwrap_or(Doc::Empty)
            }
            Stmt::While { condition, body } => {
                Doc::text("while (")
                    + Print::print(ir, condition)
                    + Doc::text(") ")
                    + Print::print(ir, body)
            }
        }
    }
}

impl Print for Function {
    fn print<'ir>(ir: &'ir IR, function: &Node<Self>) -> Doc<'ir> {
        let function_ = &ir[function];
        let header = Doc::text(&function_.name)
            + "("
            + function_
                .params
                .iter()
                .map(|param| Print::print(ir, param))
                .intersperse(Doc::text(", "))
                .reduce(|a, b| a + b)
                .unwrap_or(Doc::Empty)
            + ")";

        if let Some(body) = &function_.body {
            let just_body = [*body];
            let body_stmts = if let Stmt::Block { ops } = &ir[body] {
                &ops[..]
            } else {
                &just_body[..]
            };

            let result = if let Some(result) = &function_.result {
                Print::print(ir, result)
            } else {
                Doc::Empty
            };

            header
                + Doc::group(
                    Doc::text(" {")
                        + Doc::indent(
                            4,
                            body_stmts
                                .iter()
                                .map(|stmt| Doc::NewlineOrSpace + Print::print(ir, stmt))
                                .reduce(|a, b| a + b)
                                .unwrap_or(Doc::Empty)
                                + Doc::NewlineOrSpace
                                + "return "
                                + result
                                + ";",
                        )
                        + Doc::NewlineOrSpace
                        + "}",
                )
        } else {
            header + ";"
        }
    }
}
