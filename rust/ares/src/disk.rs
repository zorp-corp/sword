/** Disk storage for events. */
use crate::jets::list::util::{flop, lent};
use crate::lmdb::{lmdb_gulf, lmdb_read_meta};
use crate::mug::mug_u32;
use crate::noun::{IndirectAtom, Noun, D, T};
use crate::serf::Context;
use crate::serialization::cue;
use lmdb::{Cursor, Environment, Error as LmdbError, Transaction};
use lmdb_sys as ffi;
use std::convert::TryInto;
use std::fs;
use std::path::PathBuf;
use std::result::Result as StdResult;

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidPath,
    EpochNotFound,
    Lmdb(LmdbError),
}

pub type Result<T> = StdResult<T, Error>;

pub struct Disk {
    /// Full path to the pier's log directory.
    pub dir: PathBuf,

    /// Current epoch number.
    pub epoch: u64,

    /// Current epoch's LMDB environment.
    pub env: Environment,

    /// Last event number in the log.
    pub done: u64,
}

impl Disk {
    pub fn new(log_dir: PathBuf) -> Self {
        let epoch = epoch_last(&log_dir).expect("Failed to get last epoch");
        let epoch_dir = log_dir.join(format!("0i{}", epoch));
        let mut env_builder = Environment::new();
        env_builder.set_map_size(0x10000000000);
        env_builder.set_max_dbs(2);
        let env = env_builder
            .open(epoch_dir.as_path())
            .expect("Failed to open LMDB environment");
        let (_, high) = lmdb_gulf(&env);
        eprintln!("disk: new\r");
        Disk {
            dir: log_dir,
            epoch: epoch,
            env: env,
            done: high,
        }
    }
}

/// Get the number of the latest epoch in the given directory, or return
/// an error if there are no epochs or the path specified isn't a directory.
pub fn epoch_last(log_dir: &PathBuf) -> Result<u64> {
    if !log_dir.is_dir() {
        return Err(Error::InvalidPath);
    }

    let mut some = false;
    let mut last = 0;

    if let Ok(entries) = fs::read_dir(log_dir.clone()) {
        for entry in entries {
            if let Ok(entry) = entry {
                if let Some(name) = entry.file_name().to_str() {
                    if let Some(epoch) = name.strip_prefix("0i") {
                        if let Ok(n) = epoch.parse::<u64>() {
                            some = true;
                            if n > last {
                                last = n;
                            }
                        }
                    }
                }
            }
        }
    }

    if some {
        return Ok(last);
    }

    Err(Error::EpochNotFound)
}

/// Open the specified epoch's LMDB environment.
fn _epoch_load(log: Disk, epoch: u64) -> Result<()> {
    let epoch_dir = log.dir.join(format!("0i{}", epoch));
    let env_builder = Environment::new();
    let env_res = env_builder.open(epoch_dir.as_path());
    match env_res {
        Ok(_) => Ok(()),
        Err(err) => Err(Error::Lmdb(err)),
    }
}

/// Read a value from the metadata database.
pub fn disk_read_meta(env: &Environment, key: &str) -> Result<u64> {
    lmdb_read_meta(env, key).map_err(|e| Error::Lmdb(e))
}

/// Read a single event `eve` from the database.
pub fn disk_read_one(ctx: &mut Context, eve: u64) -> Option<Noun> {
    let stack = &mut ctx.nock_context.stack;
    let db_name = "EVENTS";
    let log = &ctx.log;
    let env = &log.env;
    let txn = env.begin_ro_txn().unwrap();
    let db = unsafe { txn.open_db(Some(db_name)).unwrap() };
    let key = u64::to_le_bytes(eve);
    if let Ok(value) = txn.get(db, &key) {
        let mug_bytes = &value[0..4];
        let mug = u32::from_ne_bytes(mug_bytes.try_into().unwrap());
        let jam = unsafe { IndirectAtom::new_raw_bytes_ref(stack, &value[4..]) };
        let e = cue(stack, jam.as_atom());
        eprintln!("disk: {} mug: {:x}", eve, mug);
        txn.abort();
        Some(e)
    } else {
        txn.abort();
        None
    }
}

/// Read `len` events from the database, starting at `eve`.
pub fn disk_read_list(ctx: &mut Context, eve: u64, len: u64) -> Option<Noun> {
    let stack = &mut ctx.nock_context.stack;
    let mut eves: Noun = D(0);
    let db_name = "EVENTS";
    let log = &ctx.log;
    let env = &log.env;
    let txn = env.begin_ro_txn().unwrap();
    let db = unsafe { txn.open_db(Some(db_name)).unwrap() };
    {
        let cursor = txn.open_ro_cursor(db).unwrap();
        let mut i = eve;
        while i < eve + len {
            let key = u64::to_le_bytes(i);
            let value = cursor.get(Some(&key), None, ffi::MDB_SET_KEY).unwrap().1;
            let mug_bytes = &value[0..4];
            let mug = u32::from_ne_bytes(mug_bytes.try_into().unwrap());
            let jam = unsafe { IndirectAtom::new_raw_bytes_ref(stack, &value[4..]) };
            let e = cue(stack, jam.as_atom());
            eprintln!("disk: {} mug: {:x}", i, mug);
            eves = T(stack, &[e, eves]);
            i += 1;
        }
    }
    txn.abort();
    Some(flop(stack, eves).unwrap())
}
