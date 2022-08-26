//! Exercises for: Lecture Notes on Static Analysis, schwartzbach
//! https://people.cs.vt.edu/~gback/cs6304.spring07/papers/static.pdf
//!
//! General problem: when ECSifying IRs, rust's pattern matching and debugging
//! facilities start being fairly useless. Oh well.

pub mod ecs;

/// Tiny imperative programming language
///
/// TODO: formatting and unification
pub mod tip {
    use super::ecs::*;
    use ahash::HashMap;
    use std::{cell::RefCell, collections::HashSet, ops::Index};

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
        Negate,
        AddressOf,
        Deref,
    }
    use UnaryOp::*;
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum BinOp {
        Divide,
        Subtract,
    }
    use BinOp::*;
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum MultiOp {
        Add,
        Multiply,
    }
    use MultiOp::*;

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Expression {
        UnaryOp {
            op: UnaryOp,
            arg: Node<Expression>,
        },
        BinOp {
            op: BinOp,
            left: Node<Expression>,
            right: Node<Expression>,
        },
        MultiOp {
            op: MultiOp,
            args: HashSet<Node<Expression>>,
        },
        NullaryOp(NullaryOp),
        IntConst(i32),
        Invocation(Node<Function>),
        DynamicInvocation(Node<Expression>),
        Var(Node<Var>),
    }
    impl IRNode for Expression {
        fn get_storage(ir: &IR) -> &Storage<Self> {
            &ir.expressions
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Var {
        Named(String),
        Anonymous,
    }
    impl IRNode for Var {
        fn get_storage(ir: &IR) -> &Storage<Self> {
            &ir.vars
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Statement {
        Assignment {
            var: Node<Var>,
            expr: Node<Expression>,
        },
        Print {
            expr: Node<Expression>,
        },
        Block {
            ops: Vec<Node<Statement>>,
        },
        /// this is a really dumb way to represent condition lattices lol
        If {
            /// integer 1 is true, all else is false
            condition: Node<Expression>,
            then: Node<Statement>,
            otherwise: Node<Statement>,
        },
        While {
            condition: Node<Expression>,
            body: Node<Statement>,
        },
        Declare {
            vars: HashSet<Node<Var>>,
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
        result: Node<Expression>,
    }
    impl IRNode for Function {
        fn get_storage(ir: &IR) -> &Storage<Self> {
            &ir.functions
        }
    }

    pub struct IR {
        expressions: Storage<Expression>,
        statements: Storage<Statement>,
        vars: Storage<Var>,
        functions: Storage<Function>,
        named_vars: HashMap<String, Node<Expression>>,
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
            for (name, var) in &self.named_vars {
                let var = if let Expression::Var(var) = &self[var] {
                    var
                } else {
                    panic!(
                        "expression {:?} should be Expression::Var but is {:?}",
                        var, &self[*var]
                    )
                };
                let name_ = if let Var::Named(name_) = &self[var] {
                    name_
                } else {
                    panic!("var {:?} should be named but is unnamed", var)
                };

                assert_eq!(
                    name_,
                    name,
                    "named_var says expression {} is named {} but it is named {}",
                    var.id(),
                    name,
                    name_
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

        pub fn validate(&self) {
            self.validate_keys();
            self.validate_no_cycles();
        }

        pub fn int(&mut self, value: i32) -> Node<Expression> {
            self.expressions.insert(Expression::IntConst(value))
        }

        pub fn un_op(&mut self, op: UnaryOp, arg: Node<Expression>) -> Node<Expression> {
            self.expressions.insert(Expression::UnaryOp { op, arg })
        }

        pub fn bin_op(
            &mut self,
            op: BinOp,
            left: Node<Expression>,
            right: Node<Expression>,
        ) -> Node<Expression> {
            self.expressions
                .insert(Expression::BinOp { op, left, right })
        }

        pub fn multi_op(&mut self, op: MultiOp, args: &[Node<Expression>]) -> Node<Expression> {
            let args = args.into_iter().map(Clone::clone).collect();
            self.expressions.insert(Expression::MultiOp { op, args })
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
