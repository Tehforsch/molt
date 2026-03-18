use crate::{
    molt_lang::{index_types::VarId, interpreter::value::Value},
    storage::Storage,
};

#[derive(Default)]
pub(super) struct VarStack {
    values: Vec<Value>,
}

pub(super) struct VarHandle(VarId);

impl VarHandle {
    pub(crate) fn destroy(self) -> VarId {
        let id = self.0;
        std::mem::forget(self);
        id
    }
}

impl Drop for VarHandle {
    fn drop(&mut self) {
        // I want linear types, drop bombs are ugly
        panic!("Unused var handle, call .pop(). This is an error in the interpreter.")
    }
}

impl VarStack {
    // TODO document why this is push/pop and not set/get (see recursion)
    fn push(&mut self, val: Value) {
        self.values.push(val)
    }

    fn pop(&mut self) -> Value {
        self.values.pop().unwrap()
    }

    fn get(&self) -> Value {
        // We unwrap here since the resolver will have reported
        // an undefined variable if the stack is empty at run time
        self.values.last().unwrap().clone()
    }

    fn try_get(&self) -> Option<Value> {
        self.values.last().cloned()
    }
}

pub(super) struct Vars {
    storage: Storage<VarId, VarStack>,
}

impl Vars {
    pub(crate) fn new(inner: Storage<VarId, VarStack>) -> Self {
        Self { storage: inner }
    }

    #[must_use]
    pub(crate) fn set(&mut self, id: VarId, val: Value) -> VarHandle {
        self.storage[id].push(val);
        VarHandle(id)
    }

    pub(crate) fn get(&self, id: VarId) -> Value {
        self.storage[id].get()
    }

    pub(crate) fn pop(&mut self, handle: VarHandle) -> Value {
        let id = handle.destroy();
        self.storage[id].pop()
    }

    pub(crate) fn try_get(&self, id: VarId) -> Option<Value> {
        self.storage[id].try_get()
    }

    pub(crate) fn replace(&mut self, id: VarId, new_val: Value) {
        let _ = self.pop(VarHandle(id));
        // Intentionally destroy the handle here,
        // there is another handle out there that will
        // take care of the var.
        self.set(id, new_val).destroy();
    }
}
