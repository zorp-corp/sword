use crate::flog;
use crate::interpreter::Context;
use crate::jets::bits::util::rap;
use crate::jets::form::util::scow;
use crate::mem::NockStack;
use crate::mug::met3_usize;
use crate::noun::{Atom, DirectAtom, IndirectAtom, Noun};
use either::Either::*;
use json::object;
use std::fs::{create_dir_all, File};
use std::io::{Error, Write};
use std::path::PathBuf;
use std::result::Result;
use std::time::Instant;
use sword_macros::tas;

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
pub fn write_metadata(info: &mut TraceInfo) -> Result<(), Error> {
    info.file.write_all("[ ".as_bytes())?;

    (object! {
        name: "process_name",
        ph: "M",
        pid: info.pid,
        args: object! { name: "urbit", },
    })
    .write(&mut info.file)?;
    info.file.write_all(",\n".as_bytes())?;

    (object! {
        name: "thread_name",
        ph: "M",
        pid: info.pid,
        tid: 1,
        args: object!{ name: "Event Processing", },
    })
    .write(&mut info.file)?;
    info.file.write_all(",\n".as_bytes())?;

    (object! {
        name: "thread_sort_index",
        ph: "M",
        pid: info.pid,
        tid: 1,
        args: object!{ sort_index: 1, },
    })
    .write(&mut info.file)?;
    info.file.write_all(",\n".as_bytes())?;

    Ok(())
}

/// Abort writing to trace file if an error is encountered.
///
/// This should result in a well-formed partial trace file.
pub fn write_serf_trace_safe(context: &mut Context, name: &str, start: Instant) {
    if let Err(e) = write_serf_trace(context.trace_info.as_mut().unwrap(), name, start) {
        flog!(context, "\rserf: error writing event trace to file: {:?}", e);
        let info = &mut context.trace_info;
        *info = None;
    }
}

pub fn write_serf_trace(info: &mut TraceInfo, name: &str, start: Instant) -> Result<(), Error> {
    let ts = start
        .saturating_duration_since(info.process_start)
        .as_micros() as f64;
    let dur = Instant::now().saturating_duration_since(start).as_micros() as f64;

    assert_no_alloc::permit_alloc(|| {
        let obj = object! {
            cat: "event",
            name: name,
            ph: "X",
            pid: info.pid,
            tid: 1,
            ts: ts,
            dur: dur,
        };
        obj.write(&mut info.file)
    })?;
    info.file.write_all(",\n".as_bytes())?;

    Ok(())
}

pub unsafe fn write_nock_trace(
    stack: &mut NockStack,
    info: &mut TraceInfo,
    mut trace_stack: *const TraceStack,
) -> Result<(), Error> {
    let now = Instant::now();

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
        let pc_len = met3_usize(pc);
        let pc_bytes = &pc.as_bytes()[0..pc_len];
        let pc_str = match std::str::from_utf8(pc_bytes) {
            Ok(valid) => valid,
            Err(error) => {
                let (valid, _) = pc_bytes.split_at(error.valid_up_to());
                unsafe { std::str::from_utf8_unchecked(valid) }
            }
        };

        assert_no_alloc::permit_alloc(|| {
            let obj = object! {
                cat: "nock",
                name: pc_str,
                ph: "X",
                pid: info.pid,
                tid: 1,
                ts: ts,
                dur: dur,
            };
            obj.write(&mut info.file)
        })?;
        info.file.write_all(",\n".as_bytes())?;

        trace_stack = (*trace_stack).next;
    }

    Ok(())
}

//  XX: Need Rust string interpolation helper that doesn't allocate
pub fn path_to_cord(stack: &mut NockStack, path: Noun) -> Atom {
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
