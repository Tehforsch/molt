//! Extensions to the parsing API with niche applicability.

use std::cell::Cell;
use std::mem;
use std::rc::Rc;

use crate::parser::buffer::Cursor;
use crate::parser::parse::{ParseBuffer, Unexpected, inner_unexpected};

/// Extensions to the `ParseStream` API to support speculative parsing.
pub trait Speculative {
    /// Advance this parse stream to the position of a forked parse stream.
    fn advance_to(&self, fork: &Self);
}

impl<'a> Speculative for ParseBuffer<'a> {
    fn advance_to(&self, fork: &Self) {
        if !crate::parser::buffer::same_scope(self.cursor(), fork.cursor()) {
            panic!("fork was not derived from the advancing parse stream");
        }

        let (self_unexp, self_sp) = inner_unexpected(self);
        let (fork_unexp, fork_sp) = inner_unexpected(fork);
        if !Rc::ptr_eq(&self_unexp, &fork_unexp) {
            match (fork_sp, self_sp) {
                // Unexpected set on the fork, but not on `self`, copy it over.
                (Some((span, delimiter)), None) => {
                    self_unexp.set(Unexpected::Some(span, delimiter));
                }
                // Unexpected unset. Use chain to propagate errors from fork.
                (None, None) => {
                    fork_unexp.set(Unexpected::Chain(self_unexp));

                    // Ensure toplevel 'unexpected' tokens from the fork don't
                    // propagate up the chain by replacing the root `unexpected`
                    // pointer, only 'unexpected' tokens from existing group
                    // parsers should propagate.
                    fork.unexpected
                        .set(Some(Rc::new(Cell::new(Unexpected::None))));
                }
                // Unexpected has been set on `self`. No changes needed.
                (_, Some(_)) => {}
            }
        }

        // See comment on `cell` in the struct definition.
        self.cell
            .set(unsafe { mem::transmute::<Cursor, Cursor<'static>>(fork.cursor()) });
    }
}
