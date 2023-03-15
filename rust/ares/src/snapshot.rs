use crate::mem::NockStack;
use crate::noun::Noun;

pub mod double_jam;

crate::gdb!();

pub trait Snapshot {
    fn load(&mut self, stack: &mut NockStack) -> std::io::Result<(u64, u64, Noun)>;
    fn save(&mut self, stack: &mut NockStack, noun: &mut Noun);
    fn sync(&mut self, stack: &mut NockStack, epoch: u64, event: u64);
}
