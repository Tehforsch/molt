macro_rules! ast_enum { ($($tt: tt)*) => { $( $tt )* } }
macro_rules! ast_struct { ($($tt: tt)*) => { $( $tt )* } }
macro_rules! ast_enum_of_structs { ($($tt: tt)*) => { $( $tt )* } }

pub(crate) use ast_enum;
pub(crate) use ast_struct;
pub(crate) use ast_enum_of_structs;
