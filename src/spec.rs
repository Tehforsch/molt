use std::{
    collections::{HashMap, HashSet},
    path::Path,
    str::FromStr,
};

use codespan_reporting::files::SimpleFile;
use proc_macro2::{Group, TokenStream, TokenTree};
use quote::TokenStreamExt;
use syn::Ident;

use crate::{
    convert::Convert,
    ctx::{Id, PatCtx},
    error::{emit_error, Error, ResolveError},
    grammar::{Kind, Node},
    mangle::mangle,
};

const IDENT_IDENTIFIER: char = '$';

pub(crate) enum Command {
    #[allow(dead_code)]
    Transform(SynVar, SynVar),
    Match(SynVar),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) struct SynVar {
    pub name: String,
}

#[derive(Clone)]
pub(crate) struct SynVarDecl {
    pub name: String,
    pub node: Option<Id>,
}

#[derive(Debug, Default)]
pub(crate) struct Dependencies {
    pub vars: HashSet<Ident>,
}

pub(crate) struct Spec {
    pub vars: Vec<SynVarDecl>,
}

pub(crate) struct FullSpec {
    pub spec: Spec,
    pub command: Command,
}

pub(crate) struct ParseSynVarDecl {
    pub name: Ident,
    // TODO make this optional and infer if possible.
    pub kind: Kind,
    pub node: Option<TokenStream>,
}

pub(crate) struct ParseSpec {
    pub vars: Vec<ParseSynVarDecl>,
    pub commands: Vec<Command>,
}

impl FullSpec {
    pub(crate) fn from_path(path: &Path) -> Result<(PatCtx, Self), Error> {
        let contents = std::fs::read_to_string(path).unwrap();
        let tokens = TokenStream::from_str(&contents).unwrap();
        let result: Result<(PatCtx, FullSpec), Error> = syn::parse2(tokens)
            .map_err(|e| e.into())
            .and_then(|res| resolve_parsed_transform(res).map_err(|e| e.into()));
        match result {
            Ok(res) => Ok(res),
            Err(err) => {
                let file = SimpleFile::new(format!("{:?}", path), contents);
                emit_error(&file, &err);
                return Err(err);
            }
        }
    }
}

fn get_single_command(mut commands: Vec<Command>) -> Result<Command, Error> {
    if commands.len() == 0 {
        return Err(ResolveError::NoCommandGiven.into());
    } else if commands.len() > 1 {
        return Err(ResolveError::MultipleCommandGiven.into());
    } else {
        Ok(commands.remove(0))
    }
}

fn resolve_parsed_transform(tf: ParseSpec) -> Result<(PatCtx, FullSpec), Error> {
    // Topologically sort the variable declarations according to
    // their dependencies (i.e. which variables they reference)
    let mut deps_map: HashMap<_, _> = tf
        .vars
        .iter()
        .map(|var| {
            Ok((
                var.name.clone(),
                var.node
                    .as_ref()
                    .map(|node| syn::parse2::<Dependencies>(node.clone()))
                    .transpose()?
                    .unwrap_or(Dependencies::default()),
            ))
        })
        .collect::<Result<HashMap<_, _>, syn::Error>>()?;
    let mut unsorted = tf.vars;
    let mut sorted = vec![];
    while !unsorted.is_empty() {
        let solvable = unsorted
            .iter()
            .enumerate()
            .find(|(_, var)| deps_map[&var.name].vars.is_empty());
        if let Some((index, _)) = solvable {
            let var = unsorted.remove(index);
            for deps in deps_map.values_mut() {
                deps.vars.remove(&var.name);
            }
            sorted.push(var);
        } else {
            // TODO proper error
            panic!("Unresolvable");
        }
    }
    let kind_map: HashMap<_, _> = sorted
        .iter()
        .map(|var| (var.name.clone(), var.kind))
        .collect();
    let mut ctx = PatCtx::default();
    let spec = Spec {
        vars: sorted
            .into_iter()
            .map(|var| rewrite_fully_qualified(&mut ctx, var, &kind_map))
            .collect::<Result<_, syn::Error>>()?,
    };
    Ok((
        ctx,
        FullSpec {
            spec,
            command: get_single_command(tf.commands)?,
        },
    ))
}

fn rewrite_fully_qualified(
    ctx: &mut PatCtx,
    var: ParseSynVarDecl,
    kind_map: &HashMap<Ident, Kind>,
) -> Result<SynVarDecl, syn::Error> {
    let node = var
        .node
        .map(|node| {
            let stream = annotate(node, kind_map);
            Ok::<_, syn::Error>(match var.kind {
                Kind::Const => Node::Const(syn::parse2::<syn::ItemConst>(stream)?.convert(ctx)),
                Kind::Expr => Node::Expr(syn::parse2::<syn::Expr>(stream)?.convert(ctx)),
                Kind::Ident => Node::Ident(syn::parse2::<syn::Ident>(stream)?),
                Kind::Lit => Node::Lit(syn::parse2::<syn::Lit>(stream)?),
                _ => todo!(),
            })
        })
        .transpose()?;
    let node = node.map(|node| ctx.add_node(node));
    Ok(SynVarDecl {
        name: var.name.to_string(),
        node,
    })
}

fn annotate(tokens: TokenStream, map: &HashMap<Ident, Kind>) -> TokenStream {
    let mut new = TokenStream::new();
    let mut token_iter = tokens.into_iter();
    while let Some(token) = token_iter.next() {
        match token {
            TokenTree::Group(group) => {
                new.append(Group::new(group.delimiter(), annotate(group.stream(), map)))
            }
            TokenTree::Punct(punct) => {
                if punct.as_char() == IDENT_IDENTIFIER {
                    if let Some(TokenTree::Ident(ident)) = token_iter.next() {
                        let kind = map[&ident];
                        let tokens = mangle(&ident.to_string(), kind);
                        for token in tokens.into_iter() {
                            new.append(token);
                        }
                    } else {
                        panic!("{} not followed by identifier.", IDENT_IDENTIFIER)
                    }
                } else {
                    new.append(punct);
                }
            }
            rest => new.append(rest),
        }
    }
    new
}
