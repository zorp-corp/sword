use ares::interpreter::interpret;
use ares::mem::NockStack;
use ares::noun::IndirectAtom;
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
