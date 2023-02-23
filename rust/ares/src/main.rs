use ares::interpreter::interpret;
use ares::mem::NockStack;
use ares::noun::IndirectAtom;
use ares::serf::serf;
use ares::serialization::{cue, jam};
use libc::{c_char, c_int, c_void, size_t};
use memmap::Mmap;
use memmap::MmapMut;
use std::env;
use std::ffi::CString;
use std::fs::File;
use std::fs::OpenOptions;
use std::io;
use std::mem;
use std::ptr::copy_nonoverlapping;
use std::ptr::write_bytes;

#[link(name = "pma_malloc", kind = "static")]
extern {
    fn pma_init(path: *const c_char) -> c_int;
    fn pma_load(path: *const c_char) -> c_int;
    fn pma_close(epoch: u64, event: u64) -> c_int;
    fn pma_malloc(size: size_t) -> *mut c_void;
    fn pma_free(ptr: *mut c_void) -> c_int;
    fn pma_sync(epoch: u64, event: u64) -> c_int;
}

fn pma() -> io::Result<()> {
    unsafe {
        std::fs::remove_dir_all("/tmp/pma");
        eprintln!("\rpma_init");
        pma_init(CString::new("/tmp/pma").expect("init").as_ptr());
        eprintln!("\rpma_malloc 8");
        let eight = pma_malloc(8) as *mut u64;
        *eight = 0xdeadbeef;
        eprintln!("\rpma_close");
        assert!(0 == pma_close(10, 12));
        eprintln!("\rpma_load");
        pma_load(CString::new("/tmp/pma").expect("init").as_ptr());
        eprintln!("\rpma_sync 1 {}", pma_sync(13, 15));
        eprintln!("\rpma_malloc 20");
        let twenty = pma_malloc(8) as *mut u64;
        eprintln!("\rpma_sync 2 {}", pma_sync(14, 16));
        eprintln!("\rpma_malloc 20");
        let twenty = pma_malloc(8) as *mut u64;
        eprintln!("\rpma_sync 3 {}", pma_sync(15, 16));
        eprintln!("\rpma_malloc 20");
        let twenty = pma_malloc(8) as *mut u64;
        eprintln!("\rpma_free 20");
        pma_free(twenty as *mut c_void);
        eprintln!("\rpma_sync 4 {}", pma_sync(16, 15));
        eprintln!("\rpma_malloc 20");
        let twenty = pma_malloc(8) as *mut u64;
        eprintln!("\rpma_sync 5 {}", pma_sync(17, 15));
        eprintln!("\rpma_malloc 20");
        let twenty = pma_malloc(8) as *mut u64;
        eprintln!("\rpma_sync 6 {}", pma_sync(18, 15));
        eprintln!("\rpma_malloc 20");
        let twenty = pma_malloc(8) as *mut u64;
        eprintln!("\rpma_malloc 20");
        let twenty = pma_malloc(8) as *mut u64;
        *twenty = 0xcafebabe;
        eprintln!("\rpma_free 20");
        pma_free(twenty as *mut c_void);
        eprintln!("\rpma_close 20");
        pma_close(123, 124);
    }
    Ok(())
}

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
    }

    if filename == "serf" {
        return serf()
    }

    if filename == "pma" {
        return pma()
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
