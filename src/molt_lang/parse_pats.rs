//! Parses molt patterns given the type information
//! from the typechecker (which allows us to infer the
//! syntactic kind of the variables within the pattern
//! and to which the patterns are assigned).

use super::*;
use crate::Ctx;
use crate::CtxVar;
use crate::Mode;
use crate::ctx::VarKind;
use crate::molt_lang::MoltFile;
use crate::molt_lang::typechecker::QualifiedType;
use crate::molt_lang::typechecker::TypecheckResult;
use crate::parser;
use crate::rust_grammar::Node;
use crate::rust_grammar::parse_node_with_kind;
use crate::storage::Storage;
use crate::typechecker_bug;

pub(crate) type Error = parser::Error;

impl ResolvedMoltFile {
    pub(crate) fn parse_pats(self, typeck: &TypecheckResult) -> Result<MoltFile, Error> {
        let pats = self
            .pats
            .into_iter_enumerate()
            .map(|(i, pat)| parse_pat(&self.var_names, typeck, pat, i))
            .collect::<Result<_, _>>()?;
        Ok(MoltFile {
            fns: self.fns,
            var_names: self.var_names,
            builtin_map: self.builtin_map,
            pats,
        })
    }
}

fn parse_pat(
    var_names: &Storage<VarId, Ident>,
    typeck: &TypecheckResult,
    p: UnparsedPat,
    id: PatId,
) -> Result<ParsedPat, Error> {
    let mut pat_ctx = Ctx::<Node>::new(Mode::MoltPat);
    let vars = p
        .vars
        .iter()
        .map(|(var_id, span)| {
            let name = &var_names[*var_id];
            let kind = match typeck.get_type(*var_id) {
                QualifiedType::Kind(kind) => VarKind::Single(kind),
                QualifiedType::List(ty) => {
                    let QualifiedType::Kind(kind) = *ty else {
                        typechecker_bug!()
                    };
                    VarKind::List(kind)
                }
                _ => typechecker_bug!(),
            };
            let ctx_id = pat_ctx.add_var::<Node>(CtxVar::new(name.clone(), kind));
            PatVar {
                ctx_id: ctx_id.into(),
                var_id: *var_id,
                span: *span,
            }
        })
        .collect();
    let typechecker::Type::Kind(kind) = typeck.get_pat_type(id).unwrap() else {
        unreachable!()
    };
    let result = crate::parser::parse_with_ctx(
        pat_ctx,
        |stream| parse_node_with_kind(stream, *kind),
        p.tokens,
        Mode::MoltPat,
    )?;
    Ok(ParsedPat {
        vars,
        ctx: result.ctx,
        node: result.item,
    })
}
