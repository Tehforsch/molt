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
    error::emit_error,
    grammar::{Kind, Node},
    mangle::mangle,
};

const IDENT_IDENTIFIER: char = '$';

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct SynVar {
    pub name: String,
}

pub(crate) struct SynVarDecl {
    pub name: Ident,
    pub node: Option<Node>,
}

pub(crate) struct Transformation {
    pub vars: Vec<SynVarDecl>,
}

pub(crate) struct ParseSynVarDecl {
    pub name: Ident,
    // TODO make this optional and infer if possible.
    pub kind: Kind,
    pub node: Option<TokenStream>,
}

pub(crate) struct ParseTransform {
    pub vars: Vec<ParseSynVarDecl>,
}

impl Transformation {
    pub(crate) fn from_path(path: &Path) -> Result<Self, ()> {
        let contents = std::fs::read_to_string(path).unwrap();
        let tokens = TokenStream::from_str(&contents).unwrap();
        let result: Result<Transformation, syn::Error> =
            syn::parse2(tokens).and_then(|res| resolve_parsed_transform(res));
        match result {
            Ok(res) => Ok(res),
            Err(err) => {
                let file = SimpleFile::new(format!("{:?}", path), contents);
                emit_error(&file, err);
                return Err(());
            }
        }
    }

    pub(crate) fn top_level_node(&self) -> &Node {
        self.vars.last().unwrap().node.as_ref().unwrap()
    }
}

#[derive(Debug, Default)]
pub(crate) struct Dependencies {
    pub vars: HashSet<Ident>,
}

fn resolve_parsed_transform(tf: ParseTransform) -> Result<Transformation, syn::Error> {
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
    Ok(Transformation {
        vars: sorted
            .into_iter()
            .map(|var| rewrite_fully_qualified(var, &kind_map))
            .collect::<Result<_, syn::Error>>()?,
    })
}

fn rewrite_fully_qualified(
    var: ParseSynVarDecl,
    kind_map: &HashMap<Ident, Kind>,
) -> Result<SynVarDecl, syn::Error> {
    let node = var
        .node
        .map(|node| {
            let stream = annotate(node, kind_map);
            Ok::<_, syn::Error>(match var.kind {
                Kind::Const => Node::Const(syn::parse2::<syn::ItemConst>(stream)?.convert()),
                Kind::Expr => Node::Expr(syn::parse2::<syn::Expr>(stream)?.convert()),
                Kind::Ident => Node::Ident(syn::parse2::<syn::Ident>(stream)?),
            })
        })
        .transpose()?;
    Ok(SynVarDecl {
        name: var.name,
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
