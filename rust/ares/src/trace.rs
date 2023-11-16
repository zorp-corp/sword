use crate::jets::bits::util::rap;
use crate::jets::form::util::scow;
use crate::mem::NockStack;
use crate::mug::met3_usize;
use crate::noun::{Atom, DirectAtom, IndirectAtom, Noun};
use ares_macros::tas;
use either::Either::*;
use json::object;
use std::fs::{create_dir_all, File};
use std::io::{Error, Write};
use std::path::PathBuf;
use std::result::Result;
use std::time::Instant;

crate::gdb!();

pub struct TraceInfo {
    pub file: File,
    pub pid: u32,
    pub process_start: Instant,
}

pub struct TraceStack {
    pub start: Instant,
    pub path: Noun,
    pub next: *const TraceStack,
}

pub fn create_trace_file(pier_path: PathBuf) -> Result<TraceInfo, Error> {
    let mut trace_dir_path = pier_path.clone();
    trace_dir_path.push(".urb");
    trace_dir_path.push("put");
    trace_dir_path.push("trace");
    create_dir_all(&trace_dir_path)?;

    let trace_path: PathBuf;
    let mut trace_idx = 0u32;
    loop {
        let mut prospective_path = trace_dir_path.clone();
        prospective_path.push(format!("{}.json", trace_idx));

        if prospective_path.exists() {
            trace_idx += 1;
        } else {
            trace_path = prospective_path.clone();
            break;
        }
    }

    let file = File::create(trace_path)?;
    let process_start = Instant::now();
    let pid = std::process::id();

    Ok(TraceInfo {
        file,
        pid,
        process_start,
    })
}

/// Write metadata to trace file
pub fn write_metadata(info: &mut TraceInfo) {
    info.file.write_all("[ ".as_bytes());

    (object! {
        name: "process_name",
        ph: "M",
        pid: info.pid,
        args: object! { name: "urbit", },
    })
    .write(&mut info.file);
    info.file.write_all(",\n".as_bytes());

    (object! {
        name: "thread_name",
        ph: "M",
        pid: info.pid,
        tid: 1,
        args: object!{ name: "Event Processing", },
    })
    .write(&mut info.file);
    info.file.write_all(",\n".as_bytes());

    (object! {
        name: "thread_sort_index",
        ph: "M",
        pid: info.pid,
        tid: 1,
        args: object!{ sort_index: 1, },
    })
    .write(&mut info.file);
    info.file.write_all(",\n".as_bytes());

    (object! {
        name: "thread_name",
        ph: "M",
        pid: info.pid,
        tid: 2,
        args: object!{ name: "Nock", },
    })
    .write(&mut info.file);
    info.file.write_all(",\n".as_bytes());

    (object! {
        name: "thread_sort_index",
        ph: "M",
        pid: info.pid,
        tid: 2,
        args: object!{ sort_index: 2, },
    })
    .write(&mut info.file);
    info.file.write_all(",\n".as_bytes());

    info.file.sync_data();
}

pub fn write_nock_trace(
    stack: &mut NockStack,
    info: &mut TraceInfo,
    mut trace_stack: *const TraceStack,
) {
    let now = Instant::now();
    unsafe {
        assert_no_alloc::permit_alloc(|| {
            while !trace_stack.is_null() {
                let ts = (*trace_stack)
                    .start
                    .saturating_duration_since(info.process_start)
                    .as_micros() as f64;
                let dur = now
                    .saturating_duration_since((*trace_stack).start)
                    .as_micros() as f64;

                // Don't write out traces less than 33us
                // (same threshhold used in vere)
                if dur < 33.0 {
                    trace_stack = (*trace_stack).next;
                    continue;
                }

                let pc = path_to_cord(stack, (*trace_stack).path);
                let pclen = met3_usize(pc);
                let pc_str = &pc.as_bytes()[0..pclen];

                let obj = object! {
                    cat: "nock",
                    name: pc_str,
                    ph: "X",
                    pid: info.pid,
                    tid: 2,
                    ts: ts,
                    dur: dur,
                };
                if let Err(e) = obj.write(&mut info.file) {
                    eprintln!("\rError writing trace to file: {:?}", e);
                    break;
                };
                //  XX: success above but failure here permanently malforms the trace file
                if let Err(e) = info.file.write(",\n".as_bytes()) {
                    eprintln!("\rError writing trace to file: {:?}", e);
                    break;
                };

                trace_stack = (*trace_stack).next;
            }
        });
    }
    if let Err(e) = info.file.sync_data() {
        eprintln!("\rError syncing trace file: {:?}", e);
    };
}

//  XX: Need Rust string interpolation helper that doesn't allocate
fn path_to_cord(stack: &mut NockStack, path: Noun) -> Atom {
    let mut cursor = path;
    let mut length = 0usize;

    // count how much size we need
    while let Ok(c) = cursor.as_cell() {
        unsafe {
            match c.head().as_either_atom_cell() {
                Left(a) => {
                    length += 1;
                    length += met3_usize(a);
                }
                Right(ch) => {
                    if let Ok(nm) = ch.head().as_atom() {
                        if let Ok(kv) = ch.tail().as_atom() {
                            let kvt = scow(stack, DirectAtom::new_unchecked(tas!(b"ud")), kv)
                                .expect("scow should succeed in path_to_cord");
                            let kvc =
                                rap(stack, 3, kvt).expect("rap should succeed in path_to_cord");
                            length += 1;
                            length += met3_usize(nm);
                            length += met3_usize(kvc);
                        }
                    }
                }
            }
        }
        cursor = c.tail();
    }

    // reset cursor, then actually write the path
    cursor = path;
    let mut idx = 0;
    let (mut deres, buffer) = unsafe { IndirectAtom::new_raw_mut_bytes(stack, length) };
    let slash = (b"/")[0];

    while let Ok(c) = cursor.as_cell() {
        unsafe {
            match c.head().as_either_atom_cell() {
                Left(a) => {
                    buffer[idx] = slash;
                    idx += 1;
                    let bytelen = met3_usize(a);
                    buffer[idx..idx + bytelen].copy_from_slice(&a.as_bytes()[0..bytelen]);
                    idx += bytelen;
                }
                Right(ch) => {
                    if let Ok(nm) = ch.head().as_atom() {
                        if let Ok(kv) = ch.tail().as_atom() {
                            let kvt = scow(stack, DirectAtom::new_unchecked(tas!(b"ud")), kv)
                                .expect("scow should succeed in path_to_cord");
                            let kvc =
                                rap(stack, 3, kvt).expect("rap should succeed in path_to_cord");
                            buffer[idx] = slash;
                            idx += 1;
                            let nmlen = met3_usize(nm);
                            buffer[idx..idx + nmlen].copy_from_slice(&nm.as_bytes()[0..nmlen]);
                            idx += nmlen;
                            let kvclen = met3_usize(kvc);
                            buffer[idx..idx + kvclen].copy_from_slice(&kvc.as_bytes()[0..kvclen]);
                            idx += kvclen;
                        }
                    }
                }
            }
        }
        cursor = c.tail();
    }

    unsafe { deres.normalize_as_atom() }
}
