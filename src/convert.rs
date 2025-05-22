use crate::{
    ctx::ConvertCtx,
    grammar::{self},
};

pub(crate) trait Convert<T> {
    fn convert(self, ctx: &mut impl ConvertCtx) -> T;
}

impl Convert<grammar::Item> for syn::Item {
    fn convert(self, ctx: &mut impl ConvertCtx) -> grammar::Item {
        match self {
            syn::Item::Const(s) => grammar::Item::Const(ctx.convert(s)),
            syn::Item::Enum(s) => grammar::Item::Enum(s),
            syn::Item::ExternCrate(s) => grammar::Item::ExternCrate(s),
            syn::Item::Fn(s) => grammar::Item::Fn(ctx.convert(s)),
            syn::Item::ForeignMod(s) => grammar::Item::ForeignMod(s),
            syn::Item::Impl(s) => grammar::Item::Impl(s),
            syn::Item::Macro(s) => grammar::Item::Macro(s),
            syn::Item::Mod(s) => grammar::Item::Mod(s),
            syn::Item::Static(s) => grammar::Item::Static(s),
            syn::Item::Struct(s) => grammar::Item::Struct(s),
            syn::Item::Trait(s) => grammar::Item::Trait(s),
            syn::Item::TraitAlias(s) => grammar::Item::TraitAlias(s),
            syn::Item::Type(s) => grammar::Item::Type(s),
            syn::Item::Union(s) => grammar::Item::Union(s),
            syn::Item::Use(s) => grammar::Item::Use(s),
            _ => todo!(),
        }
    }
}

impl Convert<grammar::ItemConst> for syn::ItemConst {
    fn convert(self, ctx: &mut impl ConvertCtx) -> grammar::ItemConst {
        grammar::ItemConst {
            _attrs: self.attrs,
            _vis: self.vis,
            ident: ctx.add_convert(self.ident),
            _generics: self.generics,
            _ty: self.ty,
            expr: ctx.add_convert(*self.expr),
            const_token: self.const_token,
            semi_token: self.semi_token,
        }
    }
}

impl Convert<grammar::Expr> for syn::Expr {
    fn convert(self, ctx: &mut impl ConvertCtx) -> grammar::Expr {
        match self {
            syn::Expr::Array(expr_array) => grammar::Expr::Array(expr_array),
            syn::Expr::Assign(expr_assign) => grammar::Expr::Assign(expr_assign),
            syn::Expr::Async(expr_async) => grammar::Expr::Async(expr_async),
            syn::Expr::Await(expr_await) => grammar::Expr::Await(expr_await),
            syn::Expr::Binary(expr_binary) => grammar::Expr::Binary(ctx.convert(expr_binary)),
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
            syn::Expr::Lit(expr_lit) => grammar::Expr::Lit(ctx.convert(expr_lit)),
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
            syn::Expr::Unary(expr_unary) => grammar::Expr::Unary(ctx.convert(expr_unary)),
            syn::Expr::Unsafe(expr_unsafe) => grammar::Expr::Unsafe(expr_unsafe),
            syn::Expr::While(expr_while) => grammar::Expr::While(expr_while),
            syn::Expr::Yield(expr_yield) => grammar::Expr::Yield(expr_yield),
            _ => todo!(),
        }
    }
}

impl Convert<grammar::ExprBinary> for syn::ExprBinary {
    fn convert(self, ctx: &mut impl ConvertCtx) -> grammar::ExprBinary {
        grammar::ExprBinary {
            _attrs: self.attrs,
            left: ctx.add_convert(*self.left),
            op: self.op,
            right: ctx.add_convert(*self.right),
        }
    }
}

impl Convert<grammar::ExprUnary> for syn::ExprUnary {
    fn convert(self, ctx: &mut impl ConvertCtx) -> grammar::ExprUnary {
        grammar::ExprUnary {
            _attrs: self.attrs,
            op: self.op,
            expr: ctx.add_convert(*self.expr),
        }
    }
}
impl Convert<grammar::ExprLit> for syn::ExprLit {
    fn convert(self, ctx: &mut impl ConvertCtx) -> grammar::ExprLit {
        grammar::ExprLit {
            _attrs: self.attrs,
            lit: ctx.add_convert(self.lit),
        }
    }
}

impl Convert<grammar::Ident> for syn::Ident {
    fn convert(self, _: &mut impl ConvertCtx) -> grammar::Ident {
        self
    }
}

impl Convert<grammar::Lit> for syn::Lit {
    fn convert(self, _: &mut impl ConvertCtx) -> grammar::Lit {
        self
    }
}

impl Convert<grammar::ItemFn> for syn::ItemFn {
    fn convert(self, ctx: &mut impl ConvertCtx) -> grammar::ItemFn {
        let sig = ctx.add_convert(self.sig);
        grammar::ItemFn {
            _attrs: self.attrs,
            vis: self.vis,
            sig,
            block: self.block,
        }
    }
}

impl Convert<grammar::Signature> for syn::Signature {
    fn convert(self, ctx: &mut impl ConvertCtx) -> grammar::Signature {
        grammar::Signature {
            constness: self.constness,
            asyncness: self.asyncness,
            unsafety: self.unsafety,
            _abi: self.abi,
            fn_token: self.fn_token,
            ident: ctx.add_convert(self.ident),
            generics: self.generics,
            _paren_token: self.paren_token,
            inputs: self.inputs,
            _variadic: self.variadic,
            output: self.output,
        }
    }
}
