use std::{cmp::min, path::PathBuf};
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::result::Result as StdResult;

use crate::disk::*;
use crate::jets::list::util::lent;
use crate::noun::Noun;
use crate::persist::pma_sync;
use crate::serf::{Context, play_life};

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
    let log = &mut mars.ctx.log;
    let ctx = &mut mars.ctx;
    let seq = disk_read_list(ctx, 1, eve).unwrap();  // boot sequence
    println!("--------------- bootstrap starting ----------------");
    println!("boot: 1-{}", lent(seq).unwrap());
    play_life(ctx, seq);
    println!("--------------- bootstrap complete ----------------");
    Ok(())
}

/// Replay up to `eve`, snapshot every `sap` events.
pub fn mars_play(mut mars: Mars, mut eve: u64, sap: u64) -> u64 {
    println!("mars_play: mars.sent={} mars.done={} eve={}, sap={}", mars.sent, mars.done, eve, sap);

    let mut played = 0u64;

    if eve == 0 {
        eve = mars.ctx.log.done;
    } else if eve <= mars.ctx.log.done {
        println!("mars: already computed {}", eve);
        println!("      state={}, &mut mars.log={}", mars.done, mars.ctx.log.done);
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

        pma_sync();
    }

    println!("---------------- playback starting ----------------");

    if (eve + 1) == mars.ctx.log.done {
        println!("play: event {}", mars.ctx.log.done);
    } else if eve != mars.ctx.log.done {
        println!("play: events {}-{} of {}", (mars.done + 1), eve, mars.ctx.log.done);
    } else {
        println!("play: events {}-{}", (mars.done + 1), eve);
    }

    {
        let mut past = mars.done; // last snapshot
        // let meme = 0;         // last event to bail:meme
        // let shot = 0;         // meme retry count

        while mars.done < eve {
            match mars_play_batch(&mut mars, true, 1024) {
                Ok(_) => {
                    if sap > 0 && (mars.done - past) >= sap {
                        pma_sync();
                        past = mars.done;
                        println!("play ({}): save", mars.done);
                    } else {
                        println!("play: ({}): done", mars.done);
                    }
                    break;
                }
                Err(e) => { // XX implement retries and other error handling
                    println!("play: error {}", e);
                    break;
                }
            }
        }
    }

    println!("---------------- playback complete ----------------");

    pma_sync();

    played
}

/// Play a batch of events. XX add a `when` parameter.
fn mars_play_batch(mars: &mut Mars, mug: bool, batch: u32) -> Result<()> {
    let log: &mut Disk = &mut mars.ctx.log;
    Ok(())
}
