use syn::{GenericArgument, PathArguments, Type};

pub fn type_path_matches(ty: &Type, name: &str) -> bool {
    if let Type::Path(type_path) = &ty {
        let seg = &type_path.path.segments.last().unwrap().ident;
        seg == name
    } else {
        false
    }
}

fn is_nested_type(ty: &Type, outer: &str, inner: &str) -> bool {
    if let Type::Path(type_path) = &ty {
        let seg = &type_path.path.segments.last().unwrap();
        if seg.ident != outer {
            return false;
        }
        if let PathArguments::AngleBracketed(generics) = &seg.arguments {
            if generics.args.len() != 1 {
                return false;
            };
            if let GenericArgument::Type(generic_ty) = &generics.args[0] {
                return type_path_matches(&generic_ty, inner);
            }
        }
    }
    false
}

pub(crate) fn is_node_id(ty: &Type) -> bool {
    type_path_matches(ty, "NodeId")
}

pub(crate) fn is_node_list(ty: &Type) -> bool {
    type_path_matches(ty, "NodeList")
}

pub(crate) fn is_vec_attribute(ty: &Type) -> bool {
    is_nested_type(ty, "Vec", "Attribute")
}

pub(crate) fn is_box(ty: &Type) -> bool {
    type_path_matches(ty, "Box")
}

pub(crate) fn is_token(ty: &Type) -> bool {
    match ty {
        Type::Macro(_) => true,
        _ => false,
    }
}
