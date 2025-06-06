use std::{cell::RefCell, collections::HashMap, rc::Rc};

use molt_lib::{Id, Span, Var, VarDecl};
use rust_grammar::{Node, TokenStream, TokenTree};

use crate::{
    Command, Error, FileId, MoltFile, PatCtx,
    molt_grammar::{UnresolvedMoltFile, UnresolvedVarDecl, parse_node_with_kind},
};

type ResolvedCommand = Command<Id>;
type UnresolvedCommand = Command<String>;

#[derive(Debug, thiserror::Error)]
pub enum ResolveError {
    #[error("Multiple commands given.")]
    MultipleCommandGiven,
    #[error("No commands given.")]
    NoCommandGiven,
    #[error("Unresolvable pattern.")]
    Unresolvable,
    #[error("Reference to undefined variable.")]
    UndefinedVar,
    #[error("No command given and there was no candidate for the inferred match variable.")]
    NoUnreferencedVariables,
    #[error("No command given and there were multiple candidates for the inferred match variable.")]
    MultipleUnreferencedVariables,
}

pub(crate) struct TokenVar {
    pub span: Span,
    pub name: String,
}

pub fn get_vars_in_token_stream(deps: &mut Vec<TokenVar>, tokens: TokenStream) {
    let mut token_iter = tokens.into_iter();
    while let Some(token) = token_iter.next() {
        match token {
            TokenTree::Group(group) => {
                get_vars_in_token_stream(deps, group.stream());
            }
            TokenTree::Punct(punct) => {
                if punct.as_char() == '$' {
                    if let Some(TokenTree::Ident(ident)) = token_iter.next() {
                        let dollar_span: Span = punct.span().byte_range().into();
                        let ident_span: Span = ident.span().byte_range().into();
                        let span = dollar_span.join(ident_span);
                        deps.push(TokenVar {
                            span,
                            name: ident.to_string(),
                        });
                    } else {
                        // This is most likely invalid syntax, but
                        // we can defer reporting the error to the
                        // actual parsing.
                    }
                }
            }
            _ => {}
        }
    }
}

impl UnresolvedMoltFile {
    pub fn resolve(self, file_id: FileId) -> Result<(MoltFile, PatCtx), Error> {
        let vars = self.vars;
        let mut map = HashMap::default();
        for var in vars.iter() {
            let mut deps = vec![];
            if let Some(ref tokens) = var.tokens {
                get_vars_in_token_stream(&mut deps, tokens.clone());
            }
            map.insert(&var.name, deps);
        }
        // Check every referenced var exists
        for deps in map.values() {
            if deps
                .iter()
                .any(|referenced| !map.contains_key(&referenced.name))
            {
                return Err(ResolveError::UndefinedVar.into());
            }
        }
        let command = get_command(self.commands, &vars, &map)?;
        // TODO: Warn unused
        // Populate the context with all variables.
        let mut pat_ctx = PatCtx::default();
        let vars: Vec<_> = vars
            .into_iter()
            .map(|var| {
                (
                    pat_ctx
                        .add_var::<Node>(Var::new(var.name, var.kind.into()))
                        .into(),
                    var.kind,
                    var.tokens,
                )
            })
            .collect();
        // Translate the command to use the newly
        // assigned ids by a lookup.
        let command = command.map(|name| pat_ctx.get_id_by_name(&name));
        // Now that we know that the referenced variables
        // exist, we can safely parse the TokenStream and
        // assume that we can resolve every variable in the
        // ctx.
        let pat_ctx = Rc::new(RefCell::new(pat_ctx));
        let vars: Result<Vec<_>, Error> = vars
            .into_iter()
            .map(|(id, kind, tokens)| {
                let node = tokens
                    .map(|tokens| {
                        rust_grammar::parse_with_ctx(
                            pat_ctx.clone(),
                            |stream| parse_node_with_kind(stream, kind),
                            tokens,
                        )
                    })
                    .transpose()
                    .map_err(|e| Error::parse(e, file_id))?;
                Ok(VarDecl { id, node })
            })
            .collect();
        Ok((
            MoltFile {
                vars: vars?,
                command,
            },
            pat_ctx.take(),
        ))
    }
}

fn get_command(
    mut commands: Vec<UnresolvedCommand>,
    vars: &[UnresolvedVarDecl],
    map: &HashMap<&String, Vec<TokenVar>>,
) -> Result<UnresolvedCommand, Error> {
    if commands.is_empty() {
        // infer the command
        let unreferenced_vars: Vec<_> = vars
            .iter()
            .filter(|var| {
                !map.iter()
                    .any(|(_, deps)| deps.iter().any(|v| v.name == var.name))
            })
            .collect();
        if unreferenced_vars.is_empty() {
            Err(ResolveError::NoUnreferencedVariables.into())
        } else if unreferenced_vars.len() > 1 {
            Err(ResolveError::MultipleUnreferencedVariables.into())
        } else {
            Ok(Command::Match(unreferenced_vars[0].name.clone()))
        }
    } else if commands.len() > 1 {
        Err(ResolveError::MultipleCommandGiven.into())
    } else {
        Ok(commands.remove(0))
    }
}

impl MoltFile {
    pub(crate) fn command(&mut self) -> &ResolvedCommand {
        &self.command
    }
}
