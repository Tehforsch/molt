use rust_grammar::Kind;
use syntax_ctx::{Ctx, GetKind, ToNode, Var};
use syntax_ctx::{Id, Pattern, Span};

use crate::match_pattern::PatKind;

pub(crate) struct MatchCtx<Node: GetKind> {
    pub pat_ctx: Ctx<Node>,
    pub ast_ctx: Ctx<Node>,
    rust_src: String,
    molt_src: String,
}

impl<Node: GetKind> MatchCtx<Node> {
    pub(crate) fn new(
        pat_ctx: Ctx<Node>,
        ast_ctx: Ctx<Node>,
        rust_src: &str,
        molt_src: &str,
    ) -> Self {
        Self {
            pat_ctx,
            ast_ctx,
            rust_src: rust_src.to_owned(),
            molt_src: molt_src.to_owned(),
        }
    }

    pub(crate) fn get_pat_node(&self, id: Id) -> Pattern<&Node, Id> {
        todo!()
        // match id.0 {
        //     InternalId::AstNode(idx) => Pattern::Exact(&self.ast_ctx.ctx.nodes[idx]),
        //     InternalId::PatNode(idx) => Pattern::Exact(&self.pat_ctx.ctx.nodes[idx]),
        //     InternalId::Var(_) => return Pattern::Pattern(VarId::new(id)),
        // }
    }

    pub(crate) fn get_span(&self, id: Id) -> Span {
        todo!()
        // match id.0 {
        //     InternalId::AstNode(idx) => self.ast_ctx.ctx.spans[idx],
        //     InternalId::PatNode(idx) => self.pat_ctx.ctx.spans[idx],
        //     InternalId::Var(_) => unreachable!(), // i hope
        // }
    }

    #[cfg(feature = "debug-print")]
    pub(crate) fn dump(&self) {
        println!("--------------------------------");
        // for idx in self.ast_ctx.ctx.iter() {
        //     let node = self.ast_ctx.get_node(Id(InternalId::AstNode(idx)));
        //     let kind_str = format!("{}", node.kind());
        //     println!("AstNode({:02}): {:13} = {}", idx, kind_str, node.deb(self));
        // }
        // println!("--------------------------------");
        // for idx in self.pat_ctx.ctx.iter() {
        //     let node = self.pat_ctx.get_node(Id(InternalId::PatNode(idx)));
        //     let kind_str = format!("{}", node.kind());
        //     println!("PatNode({:02}): {:13} = {}", idx, kind_str, node.deb(self));
        // }
        // println!("--------------------------------");
        // for (idx, var) in self.pat_ctx.vars.iter().enumerate() {
        //     println!("PatVar({:02}): {:14} = {:?}", idx, "", var.ident());
        // }
    }

    pub(crate) fn print<'a>(&'a self, id: Id) -> String {
        todo!()
        // let src = match id.0 {
        //     InternalId::AstNode(_) => &self.rust_src,
        //     InternalId::PatNode(_) => &self.molt_src,
        //     InternalId::Var(_) => return self.get_var(VarId(id)).to_string(),
        // };
        // let span = self.get_span(id);
        // src[span.byte_range()].to_string()
    }

    pub(crate) fn get_ast<T: ToNode<Node>>(&self, ast_id: Id) -> &T {
        match self.ast_ctx.get(ast_id) {
            Pattern::Real(t) => t,
            Pattern::Pat(_) => unreachable!(),
        }
    }

    pub(crate) fn get<T: ToNode<Node>>(&self, pat_id: Id, pat_kind: PatKind) -> Pattern<&T, Id> {
        match pat_kind {
            PatKind::FromAst => self.ast_ctx.get(pat_id),
            PatKind::FromPat => self.pat_ctx.get(pat_id),
        }
    }

    pub(crate) fn get_var(&self, var: Id) -> &Var<Node> {
        self.pat_ctx.get_var(var)
    }

    pub(crate) fn get_kind(&self, pat_id: Id, pat_kind: PatKind) -> Node::Kind {
        match pat_kind {
            PatKind::FromAst => self.ast_ctx.get_kind(pat_id),
            PatKind::FromPat => self.pat_ctx.get_kind(pat_id),
        }
    }
}
