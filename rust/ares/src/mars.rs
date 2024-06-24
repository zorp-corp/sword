use std::fmt::{Display, Formatter, Result as FmtResult};
use std::result::Result as StdResult;
use std::{cmp::min, path::PathBuf};

use crate::disk::*;
use crate::hamt::Hamt;
use crate::jets::list::util::lent;
use crate::lmdb::lmdb_gulf;
use crate::noun::{Noun, D};
use crate::persist::pma_close;
use crate::serf::{clear_interrupt, play_life, work, Context};

#[derive(Debug)]
pub enum Error {
    PlayOOM,
    PlayInterrupted,
    PlayEventLogFailure,
    PlayMugMismatch,
    PlayFailure,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Error::PlayOOM => write!(f, "play: out of memory"),
            Error::PlayInterrupted => write!(f, "play: interrupted"),
            Error::PlayEventLogFailure => write!(f, "play: event log failure"),
            Error::PlayMugMismatch => write!(f, "play: mug mismatch"),
            Error::PlayFailure => write!(f, "play: failure"),
        }
    }
}

pub type Result<T> = StdResult<T, Error>;

pub struct Mars {
    /// Serf context.
    pub ctx: Context,

    /// Execution directory (pier).
    pub dir: PathBuf,

    /// Last event requested.
    pub sent: u64,

    /// Last event processed.
    pub done: u64,
}

/// Do a boot.
fn mars_boot(mars: &mut Mars, eve: u64) -> Result<()> {
    let ctx = &mut mars.ctx;
    let seq = disk_read_list(ctx, 1, eve).unwrap(); // boot sequence
    eprintln!("--------------- bootstrap starting ----------------\r");
    eprintln!("boot: 1-{}\r", lent(seq).unwrap());
    play_life(ctx, seq);
    eprintln!("--------------- bootstrap complete ----------------\r");
    Ok(())
}

/// Replay up to `eve`, snapshot every `sap` events.
/// XX save every `sap` events, show time, produce more errors.
pub fn mars_play(mut mars: Mars, mut eve: u64, _sap: u64) -> u64 {
    let mut played = 0u64;

    if eve == 0 {
        eve = mars.ctx.log.done;
    } else if eve <= mars.ctx.log.done {
        eprintln!("mars: already computed {}\r", eve);
        eprintln!(
            "      state={}, &mut mars.log={}\r",
            mars.done, mars.ctx.log.done
        );
        return played;
    } else {
        eve = min(eve, mars.ctx.log.done);
    }

    if mars.done == mars.ctx.log.done {
        return played;
    }

    if mars.done < eve {
        played = eve - mars.done;
    }

    if mars.done == 0 {
        let life = disk_read_meta(&mars.ctx.log.env, "life").unwrap();

        mars_boot(&mut mars, life).unwrap();

        mars.sent = life;
        mars.done = life;
    }

    eprintln!("---------------- playback starting ----------------\r");

    if (eve + 1) == mars.ctx.log.done {
        eprintln!("play: event {}\r", mars.ctx.log.done);
    } else if eve != mars.ctx.log.done {
        eprintln!(
            "play: events {}-{} of {}\r",
            (mars.done + 1),
            eve,
            mars.ctx.log.done
        );
    } else {
        eprintln!("play: events {}-{}\r", (mars.done + 1), eve);
    }

    let mut _past = mars.done; // XX last snapshot
    let mut _meme = 0; // XX last event to bail:meme
    let mut _shot = 0; // XX retry counter

    while mars.done < eve {
        match mars_play_batch(&mut mars, false, 1024) {
            Ok(_) => {
                // XX show the time
                // XX only snapshot per `sap` events, not after each one
                _past = mars.done;
            }
            Err(Error::PlayOOM) => {
                eprintln!("play: out of memory\r");
                break;
            }
            Err(Error::PlayInterrupted) => {
                eprintln!("play: interrupted\r");
                break;
            }
            Err(Error::PlayEventLogFailure) => {
                eprintln!("play: event log failure\r");
                break;
            }
            Err(Error::PlayMugMismatch) => {
                eprintln!("play: mug mismatch\r");
                break;
            }
            Err(Error::PlayFailure) => {
                eprintln!("play: failure\r");
                break;
            }
        }
    }

    let _ = pma_close();

    eprintln!("---------------- playback complete ----------------\r");

    played
}

/// Play a batch of events.
/// XX use `mug`, track time, and produce more errors.
fn mars_play_batch(mars: &mut Mars, _mug: bool, bat: u64) -> Result<()> {
    let ctx = &mut mars.ctx;
    let (_, high) = lmdb_gulf(&ctx.log.env);
    let mut i = mars.done + 1;
    let start = i;
    while i < (start + bat) && i <= high {
        let e = disk_read_one(ctx, i);
        match e {
            Some(e) => {
                let stack = &mut ctx.nock_context.stack;
                ctx.nock_context.cache = Hamt::<Noun>::new(stack);
                ctx.nock_context.scry_stack = D(0);
                work(ctx, e);
                mars.done = i;
                clear_interrupt();
                i += 1;
            }
            None => {
                return Err(Error::PlayEventLogFailure);
            }
        }
    }

    Ok(())
}
