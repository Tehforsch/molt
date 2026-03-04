mod private_module {
}

pub mod public_module {
}

pub(crate) mod crate_module {
    pub(crate) fn crate_function() {}
}

pub(super) mod super_module {
    pub(super) fn super_function() {}
}

pub(in crate::visibility) mod restricted_module {
    pub(in crate::visibility) fn restricted_function() {}
}

mod private_module {
    fn private_function() {}
}

pub fn public_fn() {}
pub(crate) fn crate_fn() {}
pub(super) fn super_fn() {}
pub(in crate::visibility) fn restricted_fn() {}
fn private_fn() {}

pub struct PublicStruct {
    pub public_field: i32,
    pub(crate) crate_field: i32,
    pub(super) super_field: i32,
    pub(in crate::visibility) restricted_field: i32,
    private_field: i32,
}

pub(crate) struct CrateStruct;
pub(super) struct SuperStruct;
pub(in crate::visibility) struct RestrictedStruct;
struct PrivateStruct;

pub enum PublicEnum {
    Variant,
}

pub(crate) enum CrateEnum {
    Variant,
}

pub trait PublicTrait {
}

pub(crate) trait CrateTrait {
}

pub type PublicType = i32;
pub(crate) type CrateType = i32;
pub(super) type SuperType = i32;
pub(in crate::visibility) type RestrictedType = i32;
type PrivateType = i32;

pub const PUBLIC_CONST: i32 = 42;
pub(crate) const CRATE_CONST: i32 = 42;
pub(super) const SUPER_CONST: i32 = 42;
pub(in crate::visibility) const RESTRICTED_CONST: i32 = 42;
const PRIVATE_CONST: i32 = 42;

pub static PUBLIC_STATIC: i32 = 42;
pub(crate) static CRATE_STATIC: i32 = 42;
pub(super) static SUPER_STATIC: i32 = 42;
pub(in crate::visibility) static RESTRICTED_STATIC: i32 = 42;
static PRIVATE_STATIC: i32 = 42;
