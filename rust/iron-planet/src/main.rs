use std::env;
use std::fs::File;
use std::fs::OpenOptions;
use std::mem;
use memmap::Mmap;
use memmap::MmapMut;
use std::ptr::copy_nonoverlapping;
use std::ptr::write_bytes;
use iron_planet::mem::NockStack;
use iron_planet::serialization::{cue,jam};
use iron_planet::interpreter::interpret;
use iron_planet::noun::IndirectAtom;
use std::io;

fn main() -> io::Result<()> {
    let filename = env::args().nth(1).expect("Must provide input filename");
    let output_filename = format!("{}.out", filename.clone());
    let f = File::open(filename)?;
    let in_len = f.metadata()?.len();
    println!("in_len: {:?}", in_len);
    let mut stack = NockStack::new( 8 << 10 << 10, 0 );
    let jammed_input = unsafe {
        let in_map = Mmap::map(&f)?;
        let word_len = (in_len + 7) >> 3;
        println!("word_len: {:?}", word_len);
        let (mut atom, dest) = IndirectAtom::new_raw_mut(&mut stack, word_len as usize);
        write_bytes(dest.add(word_len as usize - 1), 0, 8);
        copy_nonoverlapping(in_map.as_ptr(), dest as *mut u8, in_len as usize);
        mem::drop(in_map);
        atom.normalize_as_atom()
    };
    let input = cue(&mut stack, jammed_input);
    let input_cell = input.as_cell().expect("Input must be jam of subject/formula pair");
    let result = interpret(&mut stack, input_cell.head(), input_cell.tail());
    let jammed_result = jam(&mut stack, result);
    let f_out = OpenOptions::new().read(true).write(true).create(true).open(output_filename)?;
    f_out.set_len((jammed_result.size() << 3) as u64)?;
    unsafe {
        let mut out_map = MmapMut::map_mut(&f_out)?;
        copy_nonoverlapping(jammed_result.data_pointer(), out_map.as_mut_ptr() as *mut u64, jammed_result.size());
        out_map.flush()?;
    };
    Ok(())
}
