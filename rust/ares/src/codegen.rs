use crate::mem::NockStack;
use crate::noun;
use crate::noun::{Atom, Cell, IndirectAtom, Noun, Slots, D, T};
use crate::interpreter::interpret;
use crate::jets::JetErr;
use crate::jets::util::slam;

use ares_macros::tas;

pub fn cg_interpret(context: &mut Context, mut subject: Noun, formula: Noun) -> Result {
}

pub mod util {
    pub fn peek(context: &mut Context, mut subject: Noun, formula: Noun) -> Result {
        // +peek slot in line core is 4
        let pek = slot(context.line, D(4));
        let sam = T(&mut context.stack, &[subject, formula]);
        slam(context, pek, sam)
    }

    pub fn poke(context: &mut Context, gist: Noun) -> Result {
        // +poke slot in line core is 86
        let pok = slot(context.line, D(86));
        let sam = T(&mut context.stack, gist);
        slam(context, pok, sam)
    }

    pub fn make_comp(s: Noun, f: Noun) -> Noun {
        T(&mut context.stack, &[tas!(b"comp"), D(0), s, f])
    }

    // pub fn make_heat() -> Noun {
    //     // TODO: ...
    // }
}
