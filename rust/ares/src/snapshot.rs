use ares_pma::*;
use crate::noun::Noun;
use crate::jets::cold::Cold;

crate::gdb!();

/** A handle to an open PMA */
struct PMA(*mut BT_state);

/** An error for the PMA */
enum Error {
    IOError(std::io::Error),
    CopyError,
}

/** Result specialized for the PMA */
type Result = std::result::Result<(), Error>;

impl PMA {
    pub fn open(path: &std::path::Path) -> std::result::Result<Self, Error> {
        // XX TODO this should use a OnceLock or similar to make sure we only open the PMA once
        todo!()
    }

    pub fn close(self) -> Result {
        todo!()
    }

    /** Sync returns no result because failure will intentionally abort the entire process */
    pub fn sync(self) {
        if unsafe { bt_sync(self.0) } != 0 {
            panic!("PMA sync failed.");
        }
    }

    pub fn malloc<T>(count: usize) {
        let one_chunk = max(size_of::<T>(), align_of::<T>());
        let bytes = one_chunk * count;

    }

    pub fn save_noun(self, noun: Noun) -> Result {
        todo!()
    }

    pub fn save_arvo(self, arvo: Noun) -> Result {
        todo!()
    }

    pub fn saved_arvo(self) -> Option<Noun> {
        todo!()
    }

    pub fn save_cold_state(self, cold: Cold) -> Result {
        todo!()
    }

    pub fn saved_cold_state(self) -> Option<Cold> {
        todo!()
    }
}
