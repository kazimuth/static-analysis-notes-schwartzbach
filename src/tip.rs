use super::ecs::*;
use super::prettier::Doc;
use crate::collections::Bidirectional;

use ahash::HashMap;
use itertools::Itertools;
use std::{
    cell::RefCell,
    collections::HashSet,
    hash::Hash,
    ops::{Index, IndexMut},
    rc::Rc,
};

pub mod printing;
pub use printing::Print;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum NullaryOp {
    Malloc,
}
use NullaryOp::*;
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum UnOp {
    /// For ints
    Negative,
    /// For bools
    LogicalNegate,
    AddressOf,
    Deref,
}
use UnOp::*;
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Divide,
    GreaterThan,
    Subtract,
}
use BinOp::*;
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MultiOp {
    Add,
    Multiply,
    Equals,
}
use MultiOp::*;

trait TokenEnum: Eq + Hash + Sized + Clone {
    fn get_tokens() -> &'static Bidirectional<Self, &'static str>;
}
lazy_static::lazy_static! {
    static ref NULLARY_OP_TOKENS: Bidirectional<NullaryOp, &'static str> = {
        let mut b = Bidirectional::default();
        b.insert(NullaryOp::Malloc, "malloc");
        b
    };
    static ref UN_OP_TOKENS: Bidirectional<UnOp, &'static str> = {
        let mut b = Bidirectional::default();
        b.insert(UnOp::Negative, "-");
        b.insert(UnOp::LogicalNegate, "!");
        b.insert(UnOp::AddressOf, "&");
        b.insert(UnOp::Deref, "*");
        b
    };
    static ref BIN_OP_TOKENS: Bidirectional<BinOp, &'static str> = {
        let mut b = Bidirectional::default();
        b.insert(BinOp::Divide, "/");
        b.insert(BinOp::GreaterThan, ">");
        b.insert(BinOp::Subtract, "-");
        b
    };
    static ref MULTI_OP_TOKENS: Bidirectional<MultiOp, &'static str> = {
        let mut b = Bidirectional::default();
        b.insert(MultiOp::Add, "+");
        b.insert(MultiOp::Multiply, "*");
        b.insert(MultiOp::Equals, "==");
        b
    };
}
impl TokenEnum for NullaryOp {
    fn get_tokens() -> &'static Bidirectional<Self, &'static str> {
        &*NULLARY_OP_TOKENS
    }
}
impl TokenEnum for UnOp {
    fn get_tokens() -> &'static Bidirectional<Self, &'static str> {
        &*UN_OP_TOKENS
    }
}
impl TokenEnum for BinOp {
    fn get_tokens() -> &'static Bidirectional<Self, &'static str> {
        &*BIN_OP_TOKENS
    }
}
impl TokenEnum for MultiOp {
    fn get_tokens() -> &'static Bidirectional<Self, &'static str> {
        &*MULTI_OP_TOKENS
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    UnOp {
        op: UnOp,
        arg: Node<Expr>,
    },
    BinOp {
        op: BinOp,
        left: Node<Expr>,
        right: Node<Expr>,
    },
    MultiOp {
        op: MultiOp,
        // strictly speaking this should be a bag but i haven't set up discrimination machinery yet
        args: Vec<Node<Expr>>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VarName {
    Anonymous(u32), // invariant: must be the index of this var in the function's local_vars
    Named(String),
}
impl VarName {
    pub fn anonymous_id(&self) -> u32 {
        match self {
            VarName::Anonymous(id) => *id,
            _ => panic!("var {:?} has no anonymous id", self),
        }
    }
    pub fn name(&self) -> &str {
        match self {
            VarName::Named(name) => &name,
            _ => panic!("varname {:?} has no name", self),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var {
    scope: Node<Function>,
    name: VarName,
}
impl IRNode for Var {
    fn get_storage(ir: &IR) -> &Storage<Self> {
        &ir.vars
    }
}

/// Stmt IR nodes.
/// We don't bother with variable declarations since they are function-scoped.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    DerefAssignment {
        /// Note: This argument *is dereferenced*. It does NOT have to begin
        /// with a dereference expression!
        deref: Node<Expr>,
        expr: Node<Expr>,
    },
    Assignment {
        var: Node<Var>,
        value: Node<Expr>,
    },
    Print {
        expr: Node<Expr>,
    },
    Block {
        ops: Vec<Node<Stmt>>,
    },
    /// this is a really dumb way to represent condition lattices lol
    If {
        /// integer 1 is true, all else is false
        condition: Node<Expr>,
        then: Node<Stmt>,
        else_: Option<Node<Stmt>>,
    },
    While {
        condition: Node<Expr>,
        body: Node<Stmt>,
    },
}
impl IRNode for Stmt {
    fn get_storage(ir: &IR) -> &Storage<Self> {
        &ir.statements
    }
}
impl Stmt {
    pub fn is_block(&self) -> bool {
        match self {
            Stmt::Block { .. } => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    name: String,
    params: Vec<Node<Var>>,
    local_vars: HashMap<VarName, Node<Var>>,
    body: Option<Node<Stmt>>,
    result: Option<Node<Expr>>,
}
impl IRNode for Function {
    fn get_storage(ir: &IR) -> &Storage<Self> {
        &ir.functions
    }
}

#[derive(Default)]
pub struct IR {
    expressions: Storage<Expr>,
    statements: Storage<Stmt>,
    vars: Storage<Var>,
    functions: Storage<Function>,
    // note: no variable shadowing
    named_functions: HashMap<String, Node<Function>>,
}

pub trait IRNode: Sized {
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
        for var in &self.vars {
            let var_ = &self[var];
            let function_ = &self[var_.scope];
            assert_eq!(function_.local_vars[&var_.name], var);
        }
        for function in &self.functions {
            let function_ = &self[function];
            for (_, var) in &function_.local_vars {
                assert_eq!(self[var].scope, function);
            }
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
    }

    pub fn validate_blocks(&self) {
        for stmt in &self.statements {
            match &self[stmt] {
                Stmt::If { then, else_, .. } => {
                    assert!(self[then].is_block());
                    if let Some(else_) = else_ {
                        assert!(self[else_].is_block());
                    }
                }
                Stmt::While { body, .. } => assert!(self[body].is_block()),
                _ => (),
            }
        }
    }

    pub fn validate(&self) {
        self.validate_keys();
        self.validate_no_cycles();
        self.validate_scoping();
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
            body: None,
            result: None,
            local_vars: HashMap::default(),
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
}

#[derive(Clone)]
pub struct FunctionBuilder<'a> {
    ir: Rc<RefCell<&'a mut IR>>,
    function: Node<Function>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn finish(self, body: Node<Stmt>, result: Node<Expr>) {
        let mut ir = self.ir.borrow_mut();
        ir.functions[self.function].body = Some(body);
        ir.functions[self.function].result = Some(result);
    }

    pub fn expr(&self, expression: Expr) -> Node<Expr> {
        self.ir.borrow_mut().expressions.insert(expression)
    }

    pub fn statement(&self, statement: Stmt) -> Node<Stmt> {
        self.ir.borrow_mut().statements.insert(statement)
    }

    pub fn declare_var(&self, name: VarName) -> Node<Var> {
        assert!(!self.ir.borrow()[self.function]
            .local_vars
            .contains_key(&name));

        let mut ir = self.ir.borrow_mut();

        let var = ir.vars.insert(Var {
            name: name.clone(),
            scope: self.function,
        });
        ir.functions[self.function].local_vars.insert(name, var);
        var
    }

    pub fn declare_named_var(&self, name: impl Into<String>) -> Node<Var> {
        self.declare_var(VarName::Named(name.into()))
    }

    pub fn declare_anonymous_var(&self, id: u32) -> Node<Var> {
        self.declare_var(VarName::Anonymous(id))
    }

    // this is pretty much the only place that invocation order matters for builder stuff... oh well
    pub fn declare_param(&self, name: impl Into<String>) -> Node<Var> {
        let param = self.declare_named_var(name);

        let mut ir = self.ir.borrow_mut();

        ir.functions[self.function].params.push(param.clone());
        param
    }

    pub fn unary_op(&self, op: UnOp, arg: Node<Expr>) -> Node<Expr> {
        self.expr(Expr::UnOp { op, arg })
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
        self.unary_op(UnOp::Negative, arg)
    }
    pub fn logical_negate(&self, arg: Node<Expr>) -> Node<Expr> {
        self.unary_op(UnOp::LogicalNegate, arg)
    }
    pub fn deref(&self, arg: Node<Expr>) -> Node<Expr> {
        self.unary_op(UnOp::Deref, arg)
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
        self.bin_op(BinOp::Subtract, left, right)
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
        then: Node<Stmt>,
        else_: Option<Node<Stmt>>,
    ) -> Node<Stmt> {
        self.statement(Stmt::If {
            condition,
            then,
            else_,
        })
    }

    pub fn while_(&self, condition: Node<Expr>, body: Node<Stmt>) -> Node<Stmt> {
        self.statement(Stmt::While { condition, body })
    }

    pub fn block(&self, ops: &[Node<Stmt>]) -> Node<Stmt> {
        self.statement(Stmt::Block {
            ops: ops.into_iter().cloned().collect(),
        })
    }

    pub fn assign(&self, var: Node<Var>, value: Node<Expr>) -> Node<Stmt> {
        self.statement(Stmt::Assignment { var, value })
    }

    pub fn value(&self, var: Node<Var>) -> Node<Expr> {
        self.expr(Expr::Var(var))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn print_example_programs() {
        let mut ir = IR::default();

        let (iterative_fibbonacci, builder) =
            ir.declare_and_implement("iterative_fibbonacci".to_string());

        let n = builder.declare_param("n".to_string());
        let f = builder.declare_named_var("f".to_string());

        let n_ = builder.value(n);
        let f_ = builder.value(f);

        let body = builder.block(&[
            builder.assign(f, builder.int(1)),
            builder.while_(
                builder.greater_than(n_, builder.int(0)),
                builder.block(&[
                    builder.assign(f, builder.multiply(&[f_, n_])),
                    builder.assign(n, builder.subtract(n_, builder.int(1))),
                ]),
            ),
        ]);

        builder.finish(body, f_);

        assert_eq!(
            Print::print(&ir, &iterative_fibbonacci)
                .prepare()
                .layout(80),
            "iterative_fibbonacci(n) {
    f = 1;
    while (n > 0) {
        f = f * n;
        n = n - 1;
    }
    return f;
}"
        );
    }
}
