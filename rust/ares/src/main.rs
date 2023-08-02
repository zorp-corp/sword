use ares::interpreter::interpret;
use ares::mem::NockStack;
use ares::noun::IndirectAtom;
use ares::serf::serf;
use ares::serialization::{cue, jam};
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

    if filename == "see gdb! definition in lib.rs about this" {
        ares::interpreter::use_gdb();
        ares::jets::use_gdb();
        ares::jets::math::use_gdb();
        ares::mem::use_gdb();
        ares::mug::use_gdb();
        ares::newt::use_gdb();
        ares::noun::use_gdb();
        ares::serf::use_gdb();
        ares::serialization::use_gdb();
        ares::snapshot::use_gdb();
        ares::snapshot::double_jam::use_gdb();
        ares::snapshot::pma::use_gdb();
    }

    if filename == "serf" {
        return serf();
    }

    let output_filename = format!("{}.out", filename);
    let f = File::open(filename)?;
    let in_len = f.metadata()?.len();
    let mut stack = NockStack::new(8 << 10 << 10, 0);
    let jammed_input = unsafe {
        let in_map = Mmap::map(&f)?;
        let word_len = (in_len + 7) >> 3;
        let (mut atom, dest) = IndirectAtom::new_raw_mut(&mut stack, word_len as usize);
        write_bytes(dest.add(word_len as usize - 1), 0, 1);
        copy_nonoverlapping(in_map.as_ptr(), dest as *mut u8, in_len as usize);
        mem::drop(in_map);
        atom.normalize_as_atom()
    };
    let input = cue(&mut stack, jammed_input);
    let input_cell = input
        .as_cell()
        .expect("Input must be jam of subject/formula pair");
    let result = interpret(&mut stack, &mut None, input_cell.head(), input_cell.tail());
    if let Ok(atom) = result.as_atom() {
        println!("Result: {}", atom);
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
