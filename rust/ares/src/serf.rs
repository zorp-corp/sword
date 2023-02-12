use crate::interpreter::{interpret, raw_slot};
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Cell, Noun, D};
use ares_macros::tas;
use std::io;

#[allow(dead_code)]
const LOAD_AXIS: u64 = 4;
const PEEK_AXIS: u64 = 22;
const POKE_AXIS: u64 = 23;
#[allow(dead_code)]
const WISH_AXIS: u64 = 10;

/**
 * This is suitable for talking to the king process.  To test, change the arg_c[0] line in
 * u3_lord_init in vere to point at this binary and start vere like normal.
 */
pub fn serf() -> io::Result<()> {
    let mut stack = NockStack::new(8 << 10 << 10, 0);
    let mut newt = Newt::new();
    newt.ripe(&mut stack, 0, 0);

    let mut eve = 0;
    let mut cor = D(0);

    // Can't use for loop because it borrows newt
    loop {
        let writ = if let Some(writ) = newt.next(&mut stack) {
            writ
        } else {
            break;
        };

        let tag = raw_slot(writ, 2).as_direct().unwrap();
        match tag.data() {
            tas!(b"live") => {
                newt.live(&mut stack);
            }
            tas!(b"peek") => {
                let sam = raw_slot(writ, 7);
                let res = slam(&mut stack, &mut newt, cor, PEEK_AXIS, sam);
                newt.peek_done(&mut stack, res);
            }
            tas!(b"play") => {
                if eve == 0 {
                    // apply lifecycle to first batch
                    let lit = raw_slot(writ, 7);
                    let sub = Cell::new_tuple(&mut stack, &[D(0), D(3)]).as_noun();
                    let lyf = Cell::new_tuple(&mut stack, &[D(2), sub, D(0), D(2)]).as_noun();
                    let gat = interpret(&mut stack, &mut Some(&mut newt), lit, lyf);
                    cor = raw_slot(gat, 7);
                } else {
                    panic!("partial replay not implemented");
                }
                eve = raw_slot(writ, 6).as_direct().unwrap().data();
                let mut lit = raw_slot(writ, 7);
                loop {
                    if let Ok(cell) = lit.as_cell() {
                        eve += 1;
                        lit = cell.tail();
                    } else {
                        break;
                    }
                }
                newt.play_done(&mut stack, 0);
            }
            tas!(b"work") => {
                let ovo = raw_slot(writ, 7);
                let res = slam(&mut stack, &mut newt, cor, POKE_AXIS, ovo)
                    .as_cell()
                    .unwrap();
                let fec = res.head();
                cor = res.tail();

                eve += 1;

                newt.work_done(&mut stack, eve - 1, 0, fec);
            }
            _ => panic!("got message with unknown tag {:?}", tag),
        };
    }

    Ok(())
}

pub fn slam(stack: &mut NockStack, newt: &mut Newt, cor: Noun, axis: u64, ovo: Noun) -> Noun {
    let pul = Cell::new_tuple(stack, &[D(9), D(axis), D(0), D(2)]).as_noun();
    let sam = Cell::new_tuple(stack, &[D(6), D(0), D(7)]).as_noun();
    let fol = Cell::new_tuple(stack, &[D(8), pul, D(9), D(2), D(10), sam, D(0), D(2)]).as_noun();
    let sub = Cell::new_tuple(stack, &[cor, ovo]).as_noun();
    interpret(stack, &mut Some(newt), sub, fol)
}
