use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::rust_grammar::{Node, TokenStream, TokenTree, Type, parse_node_with_kind};
use crate::{Id, ParsingMode, Span, Var, VarDecl};

use crate::molt_grammar::{
    MatchCommand, TokenVar, TransformCommand, TypeAnnotation, UnresolvedMoltFile, UnresolvedVarDecl,
};
use crate::{Command, Error, FileId, MoltFile, PatCtx};

#[derive(Debug, thiserror::Error)]
pub enum ResolveError {
    #[error("Multiple commands given.")]
    MultipleCommandGiven,
    #[error("No commands given.")]
    NoCommandGiven,
    #[error("Unresolvable pattern.")]
    Unresolvable,
    #[error("Reference to undefined variable.")]
    UndefinedVar(Span, String),
    #[error("There was no candidate for the inferred match variable.")]
    NoUnreferencedVariables,
    #[error("There were multiple candidates for the inferred match variable.")]
    MultipleUnreferencedVariables,
}

impl ResolveError {
    fn in_file(self, file_id: FileId) -> Error {
        Error::Resolve(self, file_id)
    }
}

type ResolvedCommand = Command<Id>;
type UnresolvedCommand = Command<TokenVar>;

pub fn get_vars_in_token_stream(deps: &mut Vec<TokenVar>, tokens: TokenStream) {
    let mut token_iter = tokens.into_iter();
    while let Some(token) = token_iter.next() {
        match token {
            TokenTree::Group(group) => {
                get_vars_in_token_stream(deps, group.stream());
            }
            TokenTree::Punct(punct) => {
                if punct.as_char() == '$' {
                    let next_token = token_iter.next();
                    if let Some(TokenTree::Ident(ident)) = next_token {
                        let dollar_span: Span = punct.span().byte_range().into();
                        let ident_span: Span = ident.span().byte_range().into();
                        let span = dollar_span.join(ident_span);
                        deps.push(TokenVar {
                            span,
                            name: ident.to_string(),
                        });
                    } else if let Some(TokenTree::Group(group)) = next_token {
                        get_vars_in_token_stream(deps, group.stream())
                    } else {
                        // This is most likely invalid syntax, but
                        // we can defer reporting the error to the
                        // actual parsing.
                        // However, for now it is useful if we panic here
                        // immediately, to recognize if I made changes
                        // to the molt syntax.
                        panic!()
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
            map.insert(&var.var.name, deps);
        }
        // Check every referenced var exists
        for deps in map.values() {
            if let Some(dep) = deps
                .iter()
                .find(|referenced| !map.contains_key(&referenced.name))
            {
                return Err(
                    ResolveError::UndefinedVar(dep.span, dep.name.to_string()).in_file(file_id)
                );
            }
        }
        let command = get_command(self.commands, &vars, &map, file_id)?;
        // TODO: Warn unused
        // Populate the context with all variables.
        let mut pat_ctx = PatCtx::default();
        let vars: Vec<_> = vars
            .into_iter()
            .map(|var| {
                (
                    pat_ctx
                        .add_var::<Node>(Var::new(var.var.name, var.kind))
                        .into(),
                    var.kind,
                    var.tokens,
                )
            })
            .collect();
        // Translate the command to use the newly
        // assigned ids by a lookup.
        let command = command.map(|var| pat_ctx.get_id_by_name(&var.name));
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
                        crate::rust_grammar::parse_with_ctx(
                            pat_ctx.clone(),
                            |stream| parse_node_with_kind(stream, kind),
                            tokens,
                            ParsingMode::Pat,
                        )
                    })
                    .transpose()
                    .map_err(|e| Error::parse(e, file_id))?;
                Ok(VarDecl { id, node })
            })
            .collect();
        let type_annotations: Result<Vec<_>, Error> = self
            .type_annotations
            .into_iter()
            .map(|ann| {
                Ok(TypeAnnotation {
                    var_name: ann.var_name,
                    type_: crate::rust_grammar::parse_with_ctx(
                        pat_ctx.clone(),
                        |input| input.parse_id::<Type>(),
                        ann.type_,
                        ParsingMode::Pat,
                    )
                    .map_err(|e| Error::parse(e, file_id))?,
                })
            })
            .collect();
        Ok((
            MoltFile {
                vars: vars?,
                command,
                type_annotations: type_annotations?,
                rulesets: self.rules,
            },
            pat_ctx.take(),
        ))
    }
}

fn get_command(
    mut commands: Vec<UnresolvedCommand>,
    vars: &[UnresolvedVarDecl],
    map: &HashMap<&String, Vec<TokenVar>>,
    file_id: FileId,
) -> Result<UnresolvedCommand, Error> {
    if commands.is_empty() {
        Ok(Command::Match(MatchCommand {
            match_: Some(infer_var(vars, map, vec![], file_id)?),
            print: None,
        }))
    } else if commands.len() > 1
        && commands
            .iter()
            .any(|command| matches!(command, Command::Match(_)))
    {
        Err(ResolveError::MultipleCommandGiven.in_file(file_id))
    } else {
        let command = commands.remove(0);
        for var in command.iter_var_names() {
            if !vars.iter().any(|v| v.var.name == *var.name) {
                return Err(
                    ResolveError::UndefinedVar(var.span, var.name.to_string()).in_file(file_id)
                );
            }
        }
        Ok(match command {
            Command::Match(MatchCommand {
                match_: None,
                print,
            }) => Command::Match(MatchCommand {
                match_: Some(infer_var(vars, map, vec![], file_id)?),
                print,
            }),
            Command::Transform(TransformCommand {
                transforms,
                match_: None,
            }) => Command::Transform(TransformCommand {
                match_: Some(infer_var(
                    vars,
                    map,
                    transforms.iter().map(|(_, output)| output).collect(),
                    file_id,
                )?),
                transforms,
            }),
            _ => unreachable!(),
        })
    }
}

fn infer_var(
    vars: &[UnresolvedVarDecl],
    map: &HashMap<&String, Vec<TokenVar>>,
    exclude: Vec<&TokenVar>,
    file_id: FileId,
) -> Result<TokenVar, Error> {
    // infer the command
    let unreferenced_vars: Vec<_> = vars
        .iter()
        .filter(|var| {
            if exclude.iter().any(|exclude| var.var.name == exclude.name) {
                return false;
            }
            !map.iter()
                .any(|(_, deps)| deps.iter().any(|v| v.name == var.var.name))
        })
        .collect();
    if unreferenced_vars.is_empty() {
        Err(ResolveError::NoUnreferencedVariables.in_file(file_id))
    } else if unreferenced_vars.len() > 1 {
        Err(ResolveError::MultipleUnreferencedVariables.in_file(file_id))
    } else {
        Ok(unreferenced_vars[0].var.clone())
    }
}

impl MoltFile {
    pub(crate) fn command(&mut self) -> ResolvedCommand {
        self.command.clone()
    }
}
