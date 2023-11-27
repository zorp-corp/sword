use crate::hamt::Hamt;
use crate::interpreter::Context;
use crate::jets::util::slot;
use crate::jets::JetErr;
use crate::noun::Noun;
use std::result::Result;

#[derive(Copy, Clone)]
pub struct Pile {
    long: Noun,
    want: Noun,
    wish: Noun,
    sire: usize,
    will: Hamt<Noun>,
    sans: usize,
}

impl Pile {
    fn from_noun(p: Noun) -> Result<Pile, JetErr> {
        Ok(Pile {
            long: slot(p, 2)?,
            want: slot(p, 6)?,
            wish: slot(p, 14)?,
            sire: slot(p, 30)?.as_direct()?.data() as usize,
            will: util::part_will(slot(p, 62)?)?,
            sans: slot(p, 63)?.as_direct()?.data() as usize,
        })
    }
}

pub fn cg_interpret(context: &mut Context, mut subject: Noun, formula: Noun) -> Result<(), JetErr> {
    // +peek returns (unit [=bell hall=_hill])
    let mut line = context.line.unwrap().expect("line must be set");
    let pek = util::peek(line, subject, formula)?.as_cell()?;
    if unsafe { pek.tail().as_cell()?.head().raw_equals(D(0)) } {
        line = util::poke(line, util::comp(subject, formula)).expect("poke failed");
        pek = util::peek(line, subject, formula)?.as_cell()?.tail();
    }
    context.peek = util::part_peek(context, pek)?;
}

pub mod util {
    use ares_macros::tas;

    use crate::{
        hamt::Hamt,
        interpreter::Context,
        jets::{
            util::{slam, slot},
            JetErr,
        },
        noun::{Cell, Noun, D, T},
    };

    use super::Pile;

    pub type NounResult = Result<Noun, JetErr>;

    pub fn peek(context: &mut Context, mut subject: Noun, formula: Noun) -> NounResult {
        // +peek slot in line core is 4
        let pek = slot(context.line, D(4));
        let sam = T(&mut context.stack, &[subject, formula]);
        slam(context, pek, sam)
    }

    pub fn poke(context: &mut Context, gist: Noun) -> NounResult {
        // +poke slot in line core is 86
        let pok = slot(context.line, D(86));
        let sam = T(&mut context.stack, gist);
        slam(context, pok, sam)
    }

    pub fn tap(context: &mut Context, map: Noun) -> NounResult {
        tap_in(context, slot(map, 30)?, D(0))
    }

    fn tap_in(context: &mut Context, a: Noun, b: Noun) -> NounResult {
        if unsafe { a.raw_equals(D(0)) } {
            Ok(b)
        } else {
            let mut node = slot(a, 2)?;
            let mut left = slot(a, 6)?;
            let mut right = slot(a, 7)?;
            Ok(tap_in(
                context,
                right,
                T(&mut context.stack, &[node, tap_in(context, left, b)?]),
            )?)
        }
    }

    pub fn part_hill(context: &mut Context, hill: Cell) -> Result<Hamt<Pile>, JetErr> {
        let kvs = tap(context, hill)?;
        let hamt = Hamt::new();
        let h = kvs.as_cell()?.head();
        while unsafe { !h.raw_equals(D(0)) } {
            let bell = h.head();
            let pile = Pile::from_noun(h.tail().head())?;
            hamt = hamt.insert(bell, pile);
            h = h.tail().tail();
        }
        Ok(hamt)
    }

    // TODO: convert to a util (hoon map) -> Hamt<Noun> ?
    pub fn part_will(will: Cell) -> Result<Hamt<Noun>, JetErr> {
        let kvs = tap(will)?;
        let hamt = Hamt::new();
        let h = kvs.as_cell()?.head();
        while unsafe { !h.raw_equals(D(0)) } {
            let bile = h.head();
            let blob = h.tail().head();
            hamt = hamt.insert(bile, blob);
            h = h.tail().tail();
        }
        Ok(hamt)
    }

    // pub fn part_peek(context: &mut Context, peek: Cell) -> Result<(Noun, Hamt<Pile>), JetErr> {
    //     let bell = peek.tail().head();
    //     let hall = part_hill(context, peek.tail().tail())?;
    //     (bell, hall)
    // }

    // pub fn comp(s: Noun, f: Noun) -> Noun {
    //     T(&mut context.stack, &[tas!(b"comp"), D(0), s, f])
    // }
}
