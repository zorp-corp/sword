use ares_pma::*;
use crate::noun::Noun;
use crate::jets::cold::ColdState;

crate::gdb!();

struct PMA(*mut BT_state);

impl PMA {
    pub fn open(path: std::path::Path) -> Result<PMA, Error> {
        todo!()
    }

    pub fn close(self) -> Result<(), Error> {
        todo!()
    }

    /** Sync returns no result because failure will intentionally abort the entire process */
    pub fn sync(self) {
        if unsafe { bt_sync(self.0) } != 0 {
            panic!("PMA sync failed.");
        }
    }


    pub fn save_noun(self, Noun) -> Result<(), Error> {
        todo!()
    }

    pub fn save_arvo(self, Noun) -> Result<(), Error> {
        todo!()
    }

    pub fn save_cold_state(self, Noun) -> Result<(), Error> {
        todo!()
    }
}
