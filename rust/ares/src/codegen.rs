use std::ptr::copy_nonoverlapping;

use crate::hamt::Hamt;
use crate::interpreter::{Context, Error};
use crate::jets::util::slot;
use crate::mem::{NockStack, Preserve};
use crate::noun::{Noun, D};
use std::result::Result;

#[derive(Copy, Clone)]
pub struct PileMem {
    long: Noun,
    want: Noun,
    wish: Noun,
    sire: usize,
    will: Hamt<Noun>,
    sans: usize,
}

#[derive(Copy, Clone)]
pub struct Pile(*const PileMem);
impl Preserve for Pile {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        if stack.is_in_frame(self.0) {
            (*(self.0)).long.preserve(stack);
            (*(self.0)).want.preserve(stack);
            (*(self.0)).wish.preserve(stack);
            (*(self.0)).will.preserve(stack);
            let dest_mem: *mut PileMem = stack.struct_alloc_in_previous_frame(1);
            copy_nonoverlapping(self.0, dest_mem, 1);
        }
    }

    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        stack.assert_struct_is_in(self.0, 1);
        (*(self.0)).long.assert_in_stack(stack);
        (*(self.0)).want.assert_in_stack(stack);
        (*(self.0)).wish.assert_in_stack(stack);
        (*(self.0)).will.assert_in_stack(stack);
    }
}

impl Pile {
    fn from_noun(stack: &mut NockStack, p: Noun) -> Result<Pile, Error> {
        unsafe {
            let mem: *mut PileMem = stack.struct_alloc(1);
            *mem = PileMem {
                long: slot(p, 2)?,
                want: slot(p, 6)?,
                wish: slot(p, 14)?,
                sire: slot(p, 30)?.as_direct()?.data() as usize,
                will: util::part_will(stack, slot(p, 62)?)?,
                sans: slot(p, 63)?.as_direct()?.data() as usize,
            };
            Ok(Pile(mem))
        }
    }
}

pub fn cg_interpret(
    context: &mut Context,
    mut subject: Noun,
    formula: Noun,
) -> Result<Noun, Error> {
    // +peek returns (unit [=bell hall=_hill])
    // TODO: turn this into a helper to get or generate the codegen
    let mut line = context.line.ok_or(Error::Deterministic(D(0)))?;
    let pek = util::peek(context, subject, formula)?;
    if unsafe { pek.raw_equals(D(0)) } {
        line = util::poke(context, util::comp(context, subject, formula)).expect("poke failed");
        let good_peek = util::peek(context, subject, formula)?;
        context.peek = Some(util::part_peek(&mut context.stack, good_peek)?);
    } else {
        context.peek = Some(util::part_peek(&mut context.stack, pek)?);
    }

    // loop: pull the hill, pull the pile, pull the blob with a body=(list pole) and a bend=site
    let mut bell = context.peek.unwrap().0;
    let mut hill = context.peek.unwrap().1;
    let mut pile = unsafe {
        *(hill
            .lookup(&mut context.stack, &mut bell)
            .ok_or(Error::Deterministic(D(0)))?
            .0)
    };
    // first call is always indirect
    let mut blob = pile.will.lookup(&mut context.stack, &mut pile.wish);
    // load subject into register given by pile.sire
    // keep looking at body until it's empty, then look at site
    // when i hit %hop, load a new body from the will hamt to get a new blob which is
    // then split into [body bend], then keep going around the loop
    // only indirect calls are %lnk and %lnt
    Ok(D(0))
}

pub mod util {
    use ares_macros::tas;

    use crate::{
        hamt::Hamt,
        interpreter::{Context, Error},
        jets::util::{kick, slam, slot},
        mem::NockStack,
        noun::{Noun, D, T},
    };

    use super::Pile;

    pub type NounResult = Result<Noun, Error>;

    pub fn peek(context: &mut Context, mut subject: Noun, formula: Noun) -> NounResult {
        // +peek slot in line core is 4
        let line = context.line.ok_or(Error::Deterministic(D(0)))?;
        let pek = kick(context, line, D(4))?;
        let sam = T(&mut context.stack, &[subject, formula]);
        slam(context, pek, sam).map_err(|_| Error::Deterministic(D(0)))
    }

    pub fn poke(context: &mut Context, gist: Noun) -> NounResult {
        // +poke slot in line core is 86
        let line = context.line.ok_or(Error::Deterministic(D(0)))?;
        let pok = kick(context, line, D(86))?;
        let sam = T(&mut context.stack, &[gist]);
        slam(context, pok, sam).map_err(|_| Error::Deterministic(D(0)))
    }

    pub fn tap(stack: &mut NockStack, map: Noun) -> NounResult {
        tap_in(stack, slot(map, 30)?, D(0))
    }

    fn tap_in(stack: &mut NockStack, a: Noun, mut b: Noun) -> NounResult {
        unsafe {
            stack.frame_push(0);
            *(stack.push::<Noun>()) = a;
            loop {
                if stack.stack_is_empty() {
                    break;
                }
                let tree = *(stack.top::<Noun>());
                stack.pop::<Noun>();
                if tree.raw_equals(D(0)) {
                    continue;
                }
                // XX needs to pop frame even if it fails
                let node = slot(tree, 2)?;
                let lr = slot(tree, 3)?;
                let l = slot(lr, 2)?;
                let r = slot(lr, 3)?;
                b = T(stack, &[node, b]);
                *(stack.push::<Noun>()) = r;
                *(stack.push::<Noun>()) = l;
            }
            stack.preserve(&mut b);
            stack.frame_pop();
            Ok(b)
        }
    }

    pub fn part_hill(stack: &mut NockStack, hill: Noun) -> Result<Hamt<Pile>, Error> {
        let mut kvs = tap(stack, hill)?;
        let mut hamt = Hamt::new();
        while !unsafe { kvs.raw_equals(D(0)) } {
            let c = kvs.as_cell()?;
            let kv = c.head();
            let mut bell = slot(kv, 2)?;
            let pile = Pile::from_noun(stack, slot(kv, 3)?)?;
            hamt = hamt.insert(stack, &mut bell, pile);
            kvs = c.tail();
        }
        Ok(hamt)
    }

    pub fn part_will(stack: &mut NockStack, will: Noun) -> Result<Hamt<Noun>, Error> {
        let mut kvs = tap(stack, will)?;
        let mut hamt = Hamt::new();
        while !unsafe { kvs.raw_equals(D(0)) } {
            let c = kvs.as_cell()?;
            let kv = c.head();
            let mut bile = slot(kv, 2)?;
            let blob = slot(kv, 3)?;
            hamt = hamt.insert(stack, &mut bile, blob);
            kvs = c.tail();
        }
        Ok(hamt)
    }

    pub fn part_peek(stack: &mut NockStack, peek: Noun) -> Result<(Noun, Hamt<Pile>), Error> {
        let bell = slot(peek, 6)?;
        let hall = part_hill(stack, slot(peek, 7)?)?;
        Ok((bell, hall))
    }

    pub fn comp(context: &mut Context, s: Noun, f: Noun) -> Noun {
        T(&mut context.stack, &[D(tas!(b"comp")), D(0), s, f])
    }
}
