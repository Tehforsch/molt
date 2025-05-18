use crate::{
    grammar::{self, Pattern},
    mangle::Unmangle,
};

pub(crate) trait Convert<T> {
    fn convert(self) -> T;
}

impl<Out, In: Convert<Out>> Convert<Pattern<Out>> for Pattern<In> {
    fn convert(self) -> Pattern<Out> {
        match self {
            Pattern::Exact(t) => Pattern::Exact(Box::new(t.convert())),
            Pattern::Pattern(syn_var) => Pattern::Pattern(syn_var),
        }
    }
}

impl Convert<grammar::ItemConst> for syn::ItemConst {
    fn convert(self) -> grammar::ItemConst {
        grammar::ItemConst {
            _attrs: self.attrs,
            _vis: self.vis,
            ident: self.ident.unmangle(),
            _generics: self.generics,
            _ty: self.ty,
            expr: self.expr.unmangle().convert(),
        }
    }
}

impl Convert<grammar::Expr> for syn::Expr {
    fn convert(self) -> grammar::Expr {
        match self {
            syn::Expr::Array(expr_array) => grammar::Expr::Array(expr_array),
            syn::Expr::Assign(expr_assign) => grammar::Expr::Assign(expr_assign),
            syn::Expr::Async(expr_async) => grammar::Expr::Async(expr_async),
            syn::Expr::Await(expr_await) => grammar::Expr::Await(expr_await),
            syn::Expr::Binary(expr_binary) => grammar::Expr::Binary(expr_binary),
            syn::Expr::Block(expr_block) => grammar::Expr::Block(expr_block),
            syn::Expr::Break(expr_break) => grammar::Expr::Break(expr_break),
            syn::Expr::Call(expr_call) => grammar::Expr::Call(expr_call),
            syn::Expr::Cast(expr_cast) => grammar::Expr::Cast(expr_cast),
            syn::Expr::Closure(expr_closure) => grammar::Expr::Closure(expr_closure),
            syn::Expr::Const(expr_const) => grammar::Expr::Const(expr_const),
            syn::Expr::Continue(expr_continue) => grammar::Expr::Continue(expr_continue),
            syn::Expr::Field(expr_field) => grammar::Expr::Field(expr_field),
            syn::Expr::ForLoop(expr_for_loop) => grammar::Expr::ForLoop(expr_for_loop),
            syn::Expr::Group(expr_group) => grammar::Expr::Group(expr_group),
            syn::Expr::If(expr_if) => grammar::Expr::If(expr_if),
            syn::Expr::Index(expr_index) => grammar::Expr::Index(expr_index),
            syn::Expr::Infer(expr_infer) => grammar::Expr::Infer(expr_infer),
            syn::Expr::Let(expr_let) => grammar::Expr::Let(expr_let),
            syn::Expr::Lit(expr_lit) => grammar::Expr::Lit(expr_lit),
            syn::Expr::Loop(expr_loop) => grammar::Expr::Loop(expr_loop),
            syn::Expr::Macro(expr_macro) => grammar::Expr::Macro(expr_macro),
            syn::Expr::Match(expr_match) => grammar::Expr::Match(expr_match),
            syn::Expr::MethodCall(expr_method_call) => grammar::Expr::MethodCall(expr_method_call),
            syn::Expr::Paren(expr_paren) => grammar::Expr::Paren(expr_paren),
            syn::Expr::Path(expr_path) => grammar::Expr::Path(expr_path),
            syn::Expr::Range(expr_range) => grammar::Expr::Range(expr_range),
            syn::Expr::RawAddr(expr_raw_addr) => grammar::Expr::RawAddr(expr_raw_addr),
            syn::Expr::Reference(expr_reference) => grammar::Expr::Reference(expr_reference),
            syn::Expr::Repeat(expr_repeat) => grammar::Expr::Repeat(expr_repeat),
            syn::Expr::Return(expr_return) => grammar::Expr::Return(expr_return),
            syn::Expr::Struct(expr_struct) => grammar::Expr::Struct(expr_struct),
            syn::Expr::Try(expr_try) => grammar::Expr::Try(expr_try),
            syn::Expr::TryBlock(expr_try_block) => grammar::Expr::TryBlock(expr_try_block),
            syn::Expr::Tuple(expr_tuple) => grammar::Expr::Tuple(expr_tuple),
            syn::Expr::Unary(expr_unary) => grammar::Expr::Unary(expr_unary),
            syn::Expr::Unsafe(expr_unsafe) => grammar::Expr::Unsafe(expr_unsafe),
            syn::Expr::While(expr_while) => grammar::Expr::While(expr_while),
            syn::Expr::Yield(expr_yield) => grammar::Expr::Yield(expr_yield),
            _ => todo!(),
        }
    }
}
