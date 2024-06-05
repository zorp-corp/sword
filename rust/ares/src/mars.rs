use std::{cmp::min, path::PathBuf};
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::result::Result as StdResult;

use crate::disk::*;
use crate::jets::list::util::lent;
use crate::mem::Preserve;
use crate::persist::{pma_close, pma_sync};
use crate::serf::{Context, play_life, play_list};

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
    let seq = disk_read_list(ctx, 1, eve).unwrap();  // boot sequence
    eprintln!("--------------- bootstrap starting ----------------\r");
    eprintln!("boot: 1-{}\r", lent(seq).unwrap());
    play_life(ctx, seq);
    eprintln!("--------------- bootstrap complete ----------------\r");
    Ok(())
}

/// Replay up to `eve`, snapshot every `sap` events.
pub fn mars_play(mut mars: Mars, mut eve: u64, _sap: u64) -> u64 {
    let mut played = 0u64;

    if eve == 0 {
        eve = mars.ctx.log.done;
    } else if eve <= mars.ctx.log.done {
        eprintln!("mars: already computed {}\r", eve);
        eprintln!("      state={}, &mut mars.log={}\r", mars.done, mars.ctx.log.done);
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

        mars_boot(&mut mars, life + 1).unwrap();

        mars.sent = life;
        mars.done = life;
    }

    eprintln!("---------------- playback starting ----------------\r");

    if (eve + 1) == mars.ctx.log.done {
        eprintln!("play: event {}\r", mars.ctx.log.done);
    } else if eve != mars.ctx.log.done {
        eprintln!("play: events {}-{} of {}\r", (mars.done + 1), eve, mars.ctx.log.done);
    } else {
        eprintln!("play: events {}-{}\r", (mars.done + 1), eve);
    }

    let past = mars.done; // last snapshot
    
    let events = disk_read_list(&mut mars.ctx, past + 1, eve - mars.done).unwrap();
    eprintln!("events len: {}\r", lent(events).unwrap());
    play_list(&mut mars.ctx, events);
    eprintln!("play: list\r");
    pma_sync();
    eprintln!("play: pma sync\r");
    let _ = pma_close();

    eprintln!("---------------- playback complete ----------------\r");

    played
}

/// Play a batch of events. XX add a `when` parameter.
fn _mars_play_batch(mars: &mut Mars, _mug: bool, batch: u64) -> Result<()> {
    let start = mars.done + 1;
    if let Some(eve) = disk_read_list(&mut mars.ctx, start, batch) {
        play_list(&mut mars.ctx, eve);
        Ok(())
    } else {
        Err(Error::PlayEventLogFailure)
    }
}
