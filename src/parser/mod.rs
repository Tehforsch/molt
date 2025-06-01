use crate::{MoltFile, RustFile, ctx::Ctx};

pub fn parse_rust_file(code: &str) -> Result<(RustFile, Ctx), crate::Error> {
    todo!()
}

pub fn parse_molt_file(code: &str) -> Result<(MoltFile, Ctx), crate::Error> {
    todo!()
}

// pub(crate) struct SpanMarker {
//     start: usize,
// }

// pub(crate) struct Parser<'a> {
//     ctx: Ctx,
//     mode: Mode,
// }

// impl<'a> Parser<'a> {
//     pub fn add_var(&self, var: Var) -> VarId {
//         self.ctx.borrow_mut().add_var(var)
//     }

//     pub fn add_var_typed<T: ToNode>(&self, var: Var) -> NodeId<T> {
//         self.ctx.borrow_mut().add_var_typed(var)
//     }

//     pub fn add_node(&self, node: Spanned<Node>) -> Id {
//         self.ctx.borrow_mut().add_node(node, self.mode)
//     }

//     pub fn add_item<T: ToNode>(&self, t: Spanned<T>) -> NodeId<T> {
//         self.ctx.borrow_mut().add(t, self.mode)
//     }
// }
