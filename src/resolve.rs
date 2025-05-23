use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

use codespan_reporting::files::Files;

use crate::{
    ctx::{Id, NodeId, PatCtx},
    error::{Error, ResolveError, emit_error},
    input::Input,
    parser::{Ident, Kind, MoltFile},
};

const IDENT_IDENTIFIER: char = '$';

#[derive(Clone, Debug)]
pub(crate) struct SynVarDecl {
    pub name: String,
    pub node: Option<Id>,
}

#[derive(Default)]
pub(crate) struct Dependencies {
    pub vars: HashSet<NodeId<Ident>>,
}

#[derive(Debug)]
pub(crate) struct Spec {
    pub vars: Vec<SynVarDecl>,
}

// fn get_command(mut commands: Vec<Command>, spec: &Spec) -> Result<Command, Error> {
//     if commands.is_empty() {
//         // Topological sorting ensures that this
//         // variable contains no other variables,
//         // so it is most likely the variable
//         // we want to match for.
//         if let Some(var) = spec.vars.last() {
//             Ok(Command::Match(SynVar {
//                 name: var.name.clone(),
//             }))
//         } else {
//             Err(ResolveError::NoCommandGiven.into())
//         }
//     } else if commands.len() > 1 {
//         return Err(ResolveError::MultipleCommandGiven.into());
//     } else {
//         Ok(commands.remove(0))
//     }
// }

// fn resolve_parsed_transform(tf: ParseSpec) -> Result<(PatCtx, File), Error> {
//     // Topologically sort the variable declarations according to
//     // their dependencies (i.e. which variables they reference)
//     let mut deps_map: HashMap<_, _> = tf
//         .vars
//         .iter()
//         .map(|var| {
//             Ok((
//                 var.name.clone(),
//                 var.node
//                     .as_ref()
//                     .map(|node| syn::parse2::<Dependencies>(node.clone()))
//                     .transpose()?
//                     .unwrap_or(Dependencies::default()),
//             ))
//         })
//         .collect::<Result<HashMap<_, _>, syn::Error>>()?;
//     let mut unsorted = tf.vars;
//     let mut sorted = vec![];
//     while !unsorted.is_empty() {
//         let solvable = unsorted
//             .iter()
//             .enumerate()
//             .find(|(_, var)| deps_map[&var.name].vars.is_empty());
//         if let Some((index, _)) = solvable {
//             let var = unsorted.remove(index);
//             for deps in deps_map.values_mut() {
//                 deps.vars.remove(&var.name);
//             }
//             sorted.push(var);
//         } else {
//             // TODO proper error
//             panic!("Unresolvable");
//         }
//     }
//     let kind_map: HashMap<_, _> = sorted
//         .iter()
//         .map(|var| (var.name.clone(), var.kind))
//         .collect();
//     let mut ctx = PatCtx::default();
//     let spec = Spec {
//         vars: sorted
//             .into_iter()
//             .map(|var| rewrite_fully_qualified(&mut ctx, var, &kind_map))
//             .collect::<Result<_, syn::Error>>()?,
//     };
//     let command = get_command(tf.commands, &spec)?;
//     Ok((ctx, File { spec, command }))
// }

// fn rewrite_fully_qualified(
//     ctx: &mut PatCtx,
//     var: ParseSynVarDecl,
//     kind_map: &HashMap<Ident, Kind>,
// ) -> Result<SynVarDecl, syn::Error> {
//     let node = var
//         .node
//         .map(|node| {
//             let stream = annotate(node, kind_map);
//             Ok::<_, syn::Error>(match var.kind {
//                 Kind::Expr => Node::Expr(syn::parse2::<syn::Expr>(stream)?.convert(ctx)),
//                 Kind::Ident => Node::Ident(syn::parse2::<syn::Ident>(stream)?),
//                 Kind::Lit => Node::Lit(syn::parse2::<syn::Lit>(stream)?),
//                 Kind::Item => Node::Item(syn::parse2::<syn::Item>(stream)?.convert(ctx)),
//                 Kind::Signature => {
//                     Node::Signature(syn::parse2::<syn::Signature>(stream)?.convert(ctx))
//                 }
//                 Kind::FnArg => Node::FnArg(syn::parse2::<syn::FnArg>(stream)?.convert(ctx)),
//             })
//         })
//         .transpose()?;
//     let node = node.map(|node| ctx.add_node(node));
//     Ok(SynVarDecl {
//         name: var.name.to_string(),
//         node,
//     })
// }

// fn annotate(tokens: TokenStream, map: &HashMap<Ident, Kind>) -> TokenStream {
//     let mut new = TokenStream::new();
//     let mut token_iter = tokens.into_iter();
//     while let Some(token) = token_iter.next() {
//         match token {
//             TokenTree::Group(group) => {
//                 new.append(Group::new(group.delimiter(), annotate(group.stream(), map)))
//             }
//             TokenTree::Punct(punct) => {
//                 if punct.as_char() == IDENT_IDENTIFIER {
//                     if let Some(TokenTree::Ident(ident)) = token_iter.next() {
//                         let kind = map[&ident];
//                         let tokens = mangle(&ident.to_string(), kind);
//                         for token in tokens.into_iter() {
//                             new.append(token);
//                         }
//                     } else {
//                         panic!("{} not followed by identifier.", IDENT_IDENTIFIER)
//                     }
//                 } else {
//                     new.append(punct);
//                 }
//             }
//             rest => new.append(rest),
//         }
//     }
//     new
// }
