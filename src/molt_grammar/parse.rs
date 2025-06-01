use syn::Token;

use crate::{
    ctx::NodeId,
    node::{Node, ToNode, UserKind},
    parser::{Parse, ParseStream, Result, braced},
    rust_grammar::Ident,
};

use super::{Command, Decl, MoltFile, UntypedVar, Var, VarDecl, VarId};

mod kws {
    syn::custom_keyword!(transform);
}

impl Parse for MoltFile {
    fn parse(parser: ParseStream) -> Result<Self> {
        let mut commands = vec![];
        let mut vars = vec![];
        while !parser.is_empty() {
            match parser.parse()? {
                Decl::VarDecl(var) => vars.push(var),
                Decl::Command(command) => commands.push(command),
            }
        }
        Ok(MoltFile {
            vars,
            commands,
            sorted: false,
        })
    }
}

impl Parse for Decl {
    fn parse(parser: ParseStream) -> Result<Self> {
        if parser.peek(Token![match]) || parser.peek(kws::transform) {
            Ok(Self::Command(parser.parse()?))
        } else {
            Ok(Self::VarDecl(parser.parse()?))
        }
    }
}

impl Parse for VarDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        let token: Token![let] = input.parse()?;
        let var: Ident = input.parse()?;
        let _: Token![:] = input.parse()?;
        let kind: UserKind = input.parse()?;
        let var: Var = Var::new(var, kind);
        let name: VarId = input.add_var(var);
        let node = if input.peek(Token![=]) {
            let _: Token![=] = input.parse()?;
            let content;
            let syn_content;
            braced!(syn_content in content in input);
            let node = Node::parse_with_kind(&content, kind)?;
            let id = input.add_node(node);
            Some(id)
        } else {
            None
        };
        let _: Token![;] = input.parse()?;
        Ok(Self { id: name, node })
    }
}

impl<T: Parse + ToNode> Parse for NodeId<T> {
    fn parse(parser: ParseStream) -> Result<Self> {
        let t = parser.parse_spanned()?;
        let id = parser.add_item(t);
        Ok(id)
    }
}

impl Parse for UntypedVar {
    fn parse(parser: ParseStream) -> Result<Self> {
        let _: Token![$] = parser.parse()?;
        Ok(UntypedVar(parser.parse()?))
    }
}

impl Parse for Command {
    fn parse(parser: ParseStream) -> Result<Self> {
        todo!()
        // let command = if parser.consume_if_matches(Keyword(Transform)) {
        //     todo!("enable variant")
        //     // let input_var: Var = parser.parse()?;
        //     // parser.consume(Minus);
        //     // parser.consume(Gt);
        //     // let output_var: Var = parser.parse()?;
        //     // Command::Transform(input_var, output_var)
        // } else {
        //     parser.consume(Keyword(Match))?;
        //     let match_var: Var = parser.parse()?;
        //     Command::Match(parser.ctx.add_var(match_var))
        // };
        // parser.consume(Semi)?;
        // Ok(command)
    }
}
