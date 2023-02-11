use ares::interpreter::{interpret, raw_slot};
use ares::mem::NockStack;
use ares::newt::Newt;
use ares::noun::{IndirectAtom, D};
use ares::serialization::{cue, jam};
use ares_macros::tas;
use memmap::Mmap;
use memmap::MmapMut;
use std::env;
use std::fs::File;
use std::fs::OpenOptions;
use std::io;
use std::mem;
use std::ptr::copy_nonoverlapping;
use std::ptr::write_bytes;

fn main() -> io::Result<()> {
    let filename = env::args().nth(1).expect("Must provide input filename");
    if filename == "serf" {
        return serf();
    }

    let output_filename = format!("{}.out", filename.clone());
    let f = File::open(filename)?;
    let in_len = f.metadata()?.len();
    let mut stack = NockStack::new(8 << 10 << 10, 0);
    let jammed_input = unsafe {
        let in_map = Mmap::map(&f)?;
        let word_len = (in_len + 7) >> 3;
        let (mut atom, dest) = IndirectAtom::new_raw_mut(&mut stack, word_len as usize);
        write_bytes(dest.add(word_len as usize - 1), 0, 8);
        copy_nonoverlapping(in_map.as_ptr(), dest as *mut u8, in_len as usize);
        mem::drop(in_map);
        atom.normalize_as_atom()
    };
    let input = cue(&mut stack, jammed_input);
    let input_cell = input
        .as_cell()
        .expect("Input must be jam of subject/formula pair");
    let result = interpret(&mut stack, input_cell.head(), input_cell.tail());
    if let Ok(atom) = result.as_atom() {
        println!("Result: {:?}", atom);
    }
    let jammed_result = jam(&mut stack, result);
    let f_out = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(output_filename)?;
    f_out.set_len((jammed_result.size() << 3) as u64)?;
    unsafe {
        let mut out_map = MmapMut::map_mut(&f_out)?;
        copy_nonoverlapping(
            jammed_result.data_pointer() as *mut u8,
            out_map.as_mut_ptr(),
            jammed_result.size() << 3,
        );
        out_map.flush()?;
    };
    Ok(())
}

/**
 * This is suitable for talking to the king process.  To test, change the arg_c[0] line in
 * u3_lord_init in vere to point at this binary and start vere like normal.
 */
fn serf() -> io::Result<()> {
    let mut newt = Newt::new();
    newt.ripe(0, 0);

    let mut eve = 0;

    // Can't use for loop because it borrows newt
    loop {
        let writ = if let Some(writ) = newt.next() {
            writ
        } else {
            break;
        };

        let tag = raw_slot(writ, 2).as_direct().unwrap();
        match tag.data() {
            tas!(b"live") => {
                newt.live();
            }
            tas!(b"peek") => {
                newt.peek_done(D(0));
            }
            tas!(b"play") => {
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
                newt.play_done(0);
            }
            tas!(b"work") => {
                newt.flog(D(tas!(b"working")));
                newt.work_done(eve, 0, D(0));
                eve += 1;
            }
            _ => panic!("got message with unknown tag {:?}", tag),
        };
    }

    Ok(())
}
