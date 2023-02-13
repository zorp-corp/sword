/** Newt: IPC to the king
 *
 * This manages an IPC connection to the king over stdin and stdout.  The protocol is jammed nouns,
 * with the following schema:
 *
 * |%
 * ::  +writ: from king to serf
 * ::
 * +$  writ
 *   $%  $:  %live
 *           $%  [%cram eve=@]
 *               [%exit cod=@]
 *               [%save eve=@]
 *               [%meld ~]
 *               [%pack ~]
 *       ==  ==
 *       [%peek mil=@ sam=*]  :: gang (each path $%([%once @tas @tas path] [%beam @tas beam]))
 *       [%play eve=@ lit=(list ?((pair @da ovum) *))]
 *       [%work mil=@ job=(pair @da ovum)]
 *   ==
 * ::  +plea: from serf to king
 * ::
 * +$  plea
 *   $%  [%live ~]
 *       [%ripe [pro=%1 hon=@ nok=@] eve=@ mug=@]
 *       [%slog pri=@ tank]
 *       [%flog cord]
 *       $:  %peek
 *           $%  [%done dat=(unit (cask))]
 *               [%bail dud=goof]
 *       ==  ==
 *       $:  %play
 *           $%  [%done mug=@]
 *               [%bail eve=@ mug=@ dud=goof]
 *       ==  ==
 *       $:  %work
 *           $%  [%done eve=@ mug=@ fec=(list ovum)]
 *               [%swap eve=@ mug=@ job=(pair @da ovum) fec=(list ovum)]
 *               [%bail lud=(list goof)]
 *       ==  ==
 *   ==
 * --
 *
 * NB: stdin and stdout are generally buffered, and there's no officially supported way to work
 * around that: https://github.com/rust-lang/rust/issues/58326.
 *
 * We use stdin and stdout with File::from_raw_fd(0) and File::from_raw_fd(1), which seems to get
 * around this.  We tested that using io::Stdout requires flushing while this doesn't, but we
 * haven't tested the same for stdin.
 *
 * It's important to not use io::Stdin and io::Stdout directly.  All printfs should use stderr.
 */
use crate::mem::NockStack;
use crate::noun::{IndirectAtom, Noun, D, T};
use crate::serialization::{cue, jam};
use ares_macros::tas;
use either::Either;
use std::io::{Read, Write};
use std::os::unix::prelude::FromRawFd;
use std::ptr::{copy_nonoverlapping, write_bytes};

pub struct Newt {
    input: std::fs::File,
    output: std::fs::File,
}

impl Newt {
    pub fn new() -> Newt {
        Newt {
            input: unsafe { std::fs::File::from_raw_fd(0) },
            output: unsafe { std::fs::File::from_raw_fd(1) },
        }
    }

    /** Write a noun to the newt.
     *
     * NB: we write 64-bit words, while vere writes bytes.  The extra zero bytes shouldn't be a
     * problem.
     */
    fn write_noun(&mut self, stack: &mut NockStack, noun: Noun) {
        let atom = jam(stack, noun);
        let size = atom.size() << 3;
        let mut buf = vec![0 as u8; size + 5];
        buf[1] = size as u8;
        buf[2] = (size >> 8) as u8;
        buf[3] = (size >> 16) as u8;
        buf[4] = (size >> 24) as u8;
        match atom.as_either() {
            Either::Left(direct) => unsafe {
                copy_nonoverlapping(
                    &direct.data() as *const u64 as *const u8,
                    buf.as_mut_ptr().add(5) as *mut u8,
                    size,
                );
            },
            Either::Right(indirect) => unsafe {
                // REVIEW: is this safe/the right way to do this?
                copy_nonoverlapping(
                    indirect.data_pointer() as *const u8,
                    buf.as_mut_ptr().add(5) as *mut u8,
                    size,
                );
            },
        };
        self.output.write_all(&buf).unwrap();
    }

    /** Send %ripe, the first event. */
    pub fn ripe(&mut self, stack: &mut NockStack, eve: u64, mug: u64) {
        let version = T(
            stack,
            &[
                D(1),   // newt protocol
                D(139), // hoon kelvin
                D(4),   // nock kelvin
            ],
        );
        let ripe = T(stack, &[D(tas!(b"ripe")), version, D(eve), D(mug)]);
        self.write_noun(stack, ripe);
    }

    /** Send %live, acknowledging. */
    pub fn live(&mut self, stack: &mut NockStack) {
        let live = T(stack, &[D(tas!(b"live")), D(0)]);
        self.write_noun(stack, live);
    }

    /** Send %slog, pretty-printed debug output. */
    pub fn slog(&mut self, stack: &mut NockStack, pri: u64, tank: Noun) {
        let slog = T(stack, &[D(tas!(b"slog")), D(pri), tank]);
        self.write_noun(stack, slog);
    }

    /** Send %flog, raw debug output. */
    pub fn flog(&mut self, stack: &mut NockStack, cord: Noun) {
        let flog = T(stack, &[D(tas!(b"flog")), cord]);
        self.write_noun(stack, flog);
    }

    /** Send %peek %done, successfully scried. */
    pub fn peek_done(&mut self, stack: &mut NockStack, dat: Noun) {
        let peek = T(stack, &[D(tas!(b"peek")), D(tas!(b"done")), dat]);
        self.write_noun(stack, peek);
    }

    /** Send %peek %bail, unsuccessfully scried. */
    pub fn peek_bail(&mut self, stack: &mut NockStack, dud: Noun) {
        let peek = T(stack, &[D(tas!(b"peek")), D(tas!(b"bail")), dud]);
        self.write_noun(stack, peek);
    }

    /** Send %play %done, successfully replayed events. */
    pub fn play_done(&mut self, stack: &mut NockStack, mug: u64) {
        let play = T(stack, &[D(tas!(b"play")), D(tas!(b"done")), D(mug)]);
        self.write_noun(stack, play);
    }

    /** Send %play %bail, failed to replay events. */
    pub fn play_bail(&mut self, stack: &mut NockStack, eve: u64, mug: u64, dud: Noun) {
        let play = T(
            stack,
            &[D(tas!(b"play")), D(tas!(b"bail")), D(eve), D(mug), dud],
        );
        self.write_noun(stack, play);
    }

    /** Send %work %done, successfully ran event. */
    pub fn work_done(&mut self, stack: &mut NockStack, eve: u64, mug: u64, fec: Noun) {
        let work = T(
            stack,
            &[D(tas!(b"work")), D(tas!(b"done")), D(eve), D(mug), fec],
        );
        self.write_noun(stack, work);
    }

    /** Send %work %swap, successfully replaced failed event. */
    pub fn work_swap(&mut self, stack: &mut NockStack, eve: u64, mug: u64, job: Noun, fec: Noun) {
        let work = T(
            stack,
            &[D(tas!(b"work")), D(tas!(b"swap")), D(eve), D(mug), job, fec],
        );
        self.write_noun(stack, work);
    }

    /** Send %work %bail, failed to run event. */
    pub fn work_bail(&mut self, stack: &mut NockStack, lud: Noun) {
        let work = T(stack, &[D(tas!(b"work")), D(tas!(b"bail")), lud]);
        self.write_noun(stack, work);
    }

    /** Fetch next message. */
    pub fn next(&mut self, stack: &mut NockStack) -> Option<Noun> {
        let mut header: Vec<u8> = Vec::with_capacity(5);
        header.resize(5, 0);
        if let Err(err) = self.input.read_exact(&mut header) {
            if err.kind() == std::io::ErrorKind::UnexpectedEof {
                return None;
            } else {
                panic!("Error reading header: {}", err);
            }
        }

        let byte_len = u32::from_le_bytes([header[1], header[2], header[3], header[4]]) as usize;

        // Would be nice to copy directly into an indirect atom, but I don't know how to do that
        // when these are aligned to bytes
        let mut body: Vec<u8> = Vec::with_capacity(byte_len);
        body.resize(byte_len, 0);
        if let Err(err) = self.input.read_exact(&mut body) {
            if err.kind() == std::io::ErrorKind::UnexpectedEof {
                return None;
            } else {
                panic!("Error reading body: {}", err);
            }
        }

        let atom = unsafe {
            let word_len = (byte_len + 7) >> 3;
            let (mut atom, dest) = IndirectAtom::new_raw_mut(stack, word_len as usize);
            write_bytes(dest.add(word_len as usize - 1), 0, 8);
            copy_nonoverlapping(body.as_ptr(), dest as *mut u8, byte_len);
            atom.normalize_as_atom()
        };

        Some(cue(stack, atom))
    }
}
