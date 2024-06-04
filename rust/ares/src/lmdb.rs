use lmdb::{Cursor, Environment, Transaction};
use lmdb_sys as ffi;
use std::convert::TryInto;

pub type Result<T> = std::result::Result<T, lmdb::Error>;

pub fn lmdb_read_meta(env: &Environment, key: &str) -> Result<u64> {
    let db_name = "META";
    let txn = env.begin_ro_txn()?;
    let db = unsafe { txn.open_db(Some(db_name))? };
    let bytes: &[u8] = txn.get(db, &key.as_bytes())?;
    if bytes.len() > 8 {
        panic!("lmdb_read_meta: value too large for u64");
    }
    let mut value: u64 = 0;
    for &byte in bytes.iter() {
        value = (value << 8) | u64::from(byte);
    }
    Ok(value)
}

pub fn lmdb_gulf(env: &Environment) -> (u64, u64) {
    let db_name = "EVENTS";
    let txn = env.begin_ro_txn().unwrap();
    let db = unsafe { txn.open_db(Some(db_name)).unwrap() };
    let cursor = txn.open_ro_cursor(db).unwrap();
    if let Some(first) = cursor.get(None, None, ffi::MDB_FIRST).unwrap().0 {
        let low = u64::from_le_bytes(first.try_into().unwrap());
        if let Some(last) = cursor.get(None, None, ffi::MDB_LAST).unwrap().0 {
            let high = u64::from_le_bytes(last.try_into().unwrap());
            return (low, high);
        } else {
            panic!("Couldn't get last event from the database");
        }
    } else {
        panic!("Couldn't get first event from the database");
    }
}

/// Read `len` events from the database, starting at `eve`, and calling `read_f` for each
/// event.
pub fn lmdb_read<F>(env: &Environment, eve: u64, len: usize, mut read_f: F)
where
    F: FnMut(u64, &[u8]),
{
    let db_name = "EVENTS";
    let txn = env.begin_ro_txn().unwrap();
    let db = unsafe { txn.open_db(Some(db_name)).unwrap() };
    let mut cursor = txn.open_ro_cursor(db).unwrap();
    for (key, value) in cursor.iter_from(&u64::to_le_bytes(eve)) {
        println!("key: {:?}", key);
        if key > u64::to_le_bytes(eve + len as u64).as_ref() {
            break;
        }
        let key = u64::from_le_bytes(key.try_into().unwrap());
        read_f(key, value);
    }
}
