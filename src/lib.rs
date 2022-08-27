//! Exercises for: Lecture Notes on Static Analysis, schwartzbach
//! https://people.cs.vt.edu/~gback/cs6304.spring07/papers/static.pdf
//!
//! General problem: when ECSifying IRs, rust's pattern matching and debugging
//! facilities start being fairly useless. Oh well.

#![allow(unused)]

pub mod ecs;

pub mod prettier;

/// Tiny imperative programming language
///
/// TODO: formatting and unification
pub mod tip {
    use super::ecs::*;
    use ahash::HashMap;
    use std::{
        cell::RefCell,
        collections::HashSet,
        ops::{Index, IndexMut},
        rc::Rc,
    };

    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum NullaryOp {
        Malloc,
    }
    use NullaryOp::*;
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum UnaryOp {
        /// For ints
        Negative,
        /// For bools
        LogicalNegate,
        AddressOf,
        Deref,
    }
    use UnaryOp::*;
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum BinOp {
        Divide,
        GreaterThan,
    }
    use BinOp::*;
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum MultiOp {
        Add,
        Multiply,
        Equals,
    }
    use MultiOp::*;

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Expr {
        UnaryOp {
            op: UnaryOp,
            arg: Node<Expr>,
        },
        BinOp {
            op: BinOp,
            left: Node<Expr>,
            right: Node<Expr>,
        },
        MultiOp {
            op: MultiOp,
            args: HashSet<Node<Expr>>,
        },
        NullaryOp(NullaryOp),
        IntConst(i32),
        Invocation {
            function: Node<Function>,
            args: Vec<Node<Expr>>,
        },
        DynamicInvocation {
            function_reference: Node<Expr>,
            args: Vec<Node<Expr>>,
        },
        FunctionReference(Node<Function>),
        Var(Node<Var>),
    }
    impl IRNode for Expr {
        fn get_storage(ir: &IR) -> &Storage<Self> {
            &ir.expressions
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Var {
        scope: Node<Function>,
        name: Option<String>,
    }
    impl IRNode for Var {
        fn get_storage(ir: &IR) -> &Storage<Self> {
            &ir.vars
        }
    }

    /// Statement IR nodes.
    /// We don't bother with variable declarations since they are function-scoped.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Statement {
        DerefAssignment {
            /// Note: This argument *is dereferenced*. It does NOT have to begin
            /// with a dereference expression!
            deref: Node<Expr>,
            expr: Node<Expr>,
        },
        Assignment {
            var: Node<Var>,
            expr: Node<Expr>,
        },
        Print {
            expr: Node<Expr>,
        },
        Block {
            ops: Vec<Node<Statement>>,
        },
        /// this is a really dumb way to represent condition lattices lol
        If {
            /// integer 1 is true, all else is false
            condition: Node<Expr>,
            then: Node<Statement>,
            else_: Node<Statement>,
        },
        While {
            condition: Node<Expr>,
            body: Node<Statement>,
        },
    }
    impl IRNode for Statement {
        fn get_storage(ir: &IR) -> &Storage<Self> {
            &ir.statements
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Function {
        name: String,
        params: Vec<Node<Var>>,
        statements: Vec<Node<Statement>>,
        result: Option<Node<Expr>>,
    }
    impl IRNode for Function {
        fn get_storage(ir: &IR) -> &Storage<Self> {
            &ir.functions
        }
    }

    pub struct IR {
        expressions: Storage<Expr>,
        statements: Storage<Statement>,
        vars: Storage<Var>,
        functions: Storage<Function>,
        // note: no variable shadowing
        named_vars: HashMap<(Node<Function>, String), Node<Var>>,
        named_functions: HashMap<String, Node<Function>>,
    }

    trait IRNode: Sized {
        fn get_storage(ir: &IR) -> &Storage<Self>;
    }

    impl<T: IRNode> Index<Node<T>> for IR {
        type Output = T;

        fn index(&self, index: Node<T>) -> &Self::Output {
            &T::get_storage(self)[index]
        }
    }
    impl<T: IRNode> Index<&Node<T>> for IR {
        type Output = T;

        fn index(&self, index: &Node<T>) -> &Self::Output {
            &T::get_storage(self)[*index]
        }
    }

    impl IR {
        pub fn validate_keys(&self) {
            for ((function, name), var) in &self.named_vars {
                assert_eq!(
                    self[var].name.as_ref(),
                    Some(name),
                    "var {:?} name key {} does not match body {:?}",
                    var,
                    name,
                    self[var].name
                );
            }
            for (name, function) in &self.named_functions {
                assert_eq!(
                    name, &self[function].name,
                    "function {:?} should be named {} but is named {}",
                    function, name, &self[function].name
                )
            }
        }

        pub fn validate_no_cycles(&self) {
            /* TODO */
        }

        pub fn validate_scoping(&self) {
            /* TODO: check all variable expressions used only in their containing fns
              (this is not necessary in better IRs (e.g. Thorin, Leißa et al '15),
              but i am being as basic as possible rn.
            */

            // ...musing: this sort of scoping management is what proper lattice management can really help with...
        }

        pub fn validate(&self) {
            self.validate_keys();
            self.validate_no_cycles();
        }

        pub fn declare_function(&mut self, name: String) -> Node<Function> {
            assert!(
                !self.named_functions.contains_key(&name),
                "function named {} already exists",
                name
            );

            let function = Function {
                name: name.clone(),
                params: vec![],
                statements: vec![],
                result: None,
            };

            let function = self.functions.insert(function);
            self.named_functions.insert(name, function);
            function
        }

        pub fn implement_function(&mut self, function: Node<Function>) -> FunctionBuilder {
            FunctionBuilder {
                ir: Rc::new(RefCell::new(self)),
                function,
            }
        }

        pub fn declare_and_implement(&mut self, name: String) -> (Node<Function>, FunctionBuilder) {
            let node = self.declare_function(name);
            (node, self.implement_function(node))
        }

        pub fn print_function(&self, function: Node<Function>) -> String {
            // need to collect usage counts to decide when to print intermediates
            // on their own
            unimplemented!()
        }
    }

    #[derive(Clone)]
    pub struct FunctionBuilder<'a> {
        ir: Rc<RefCell<&'a mut IR>>,
        function: Node<Function>,
    }

    impl<'a> FunctionBuilder<'a> {
        pub fn return_(self, expression: Node<Expr>) {
            self.ir.borrow_mut().functions[self.function].result = Some(expression)
        }

        pub fn expr(&self, expression: Expr) -> Node<Expr> {
            self.ir.borrow_mut().expressions.insert(expression)
        }

        pub fn statement(&self, statement: Statement) -> Node<Statement> {
            self.ir.borrow_mut().statements.insert(statement)
        }

        pub fn declare_var(&self, name: &str) -> Node<Var> {
            self.ir.borrow_mut().vars.insert(Var {
                name: Some(name.into()),
                scope: self.function,
            })
        }

        // this is pretty much the only place that invocation order matters for builder stuff... oh well
        pub fn declare_param(&self, function: Node<Function>, name: &str) -> Node<Var> {
            let param = self.declare_var(name);

            let mut ir = self.ir.borrow_mut();
            assert!(!ir.named_vars.contains_key(&(function, name.into()))); // FIXME string clone

            ir.functions[function].params.push(param.clone());
            param
        }

        pub fn unary_op(&self, op: UnaryOp, arg: Node<Expr>) -> Node<Expr> {
            self.expr(Expr::UnaryOp { op, arg })
        }

        pub fn bin_op(&self, op: BinOp, left: Node<Expr>, right: Node<Expr>) -> Node<Expr> {
            self.expr(Expr::BinOp { op, left, right })
        }

        pub fn multi_op(&self, op: MultiOp, args: &[Node<Expr>]) -> Node<Expr> {
            let args = args.into_iter().map(Clone::clone).collect();
            self.expr(Expr::MultiOp { op, args })
        }

        pub fn int(&self, value: i32) -> Node<Expr> {
            self.expr(Expr::IntConst(value))
        }

        pub fn negative(&self, arg: Node<Expr>) -> Node<Expr> {
            self.unary_op(UnaryOp::Negative, arg)
        }
        pub fn logical_negate(&self, arg: Node<Expr>) -> Node<Expr> {
            self.unary_op(UnaryOp::LogicalNegate, arg)
        }
        pub fn deref(&self, arg: Node<Expr>) -> Node<Expr> {
            self.unary_op(UnaryOp::Deref, arg)
        }

        pub fn add(&self, args: &[Node<Expr>]) -> Node<Expr> {
            self.multi_op(MultiOp::Add, args)
        }

        pub fn multiply(&self, args: &[Node<Expr>]) -> Node<Expr> {
            self.multi_op(MultiOp::Multiply, args)
        }

        pub fn equals(&self, args: &[Node<Expr>]) -> Node<Expr> {
            self.multi_op(MultiOp::Equals, args)
        }

        pub fn subtract(&self, left: Node<Expr>, right: Node<Expr>) -> Node<Expr> {
            self.add(&[left, self.negative(right)])
        }

        pub fn divide(&self, left: Node<Expr>, right: Node<Expr>) -> Node<Expr> {
            self.bin_op(BinOp::Divide, left, right)
        }

        pub fn greater_than(&self, left: Node<Expr>, right: Node<Expr>) -> Node<Expr> {
            self.bin_op(BinOp::GreaterThan, left, right)
        }

        pub fn invoke(&self, function: Node<Function>, args: &[Node<Expr>]) -> Node<Expr> {
            self.expr(Expr::Invocation {
                function,
                args: args.into_iter().cloned().collect(),
            })
        }

        pub fn dynamic_invoke(
            &self,
            function_reference: Node<Expr>,
            args: &[Node<Expr>],
        ) -> Node<Expr> {
            self.expr(Expr::DynamicInvocation {
                function_reference,
                args: args.into_iter().cloned().collect(),
            })
        }

        pub fn if_(
            &self,
            condition: Node<Expr>,
            then: Node<Statement>,
            else_: Node<Statement>,
        ) -> Node<Statement> {
            self.statement(Statement::If {
                condition,
                then,
                else_,
            })
        }

        pub fn while_(&self, condition: Node<Expr>, body: Node<Statement>) -> Node<Statement> {
            self.statement(Statement::While { condition, body })
        }

        pub fn block(&self, ops: &[Node<Statement>]) -> Node<Statement> {
            self.statement(Statement::Block {
                ops: ops.into_iter().cloned().collect(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
