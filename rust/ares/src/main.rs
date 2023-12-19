use ares::jets::hot::URBIT_HOT_STATE;
use ares::serf::serf;
use std::env;
use std::io;

fn main() -> io::Result<()> {
    //  debug
    #[cfg(feature = "stop_for_debug")]
    {
        eprintln!("serf: pid {}", std::process::id());
        if unsafe { libc::kill(std::process::id() as i32, libc::SIGSTOP) } != 0 {
            panic!("Could not stop ourselves.");
        };
    }

    let filename = env::args().nth(1).expect("Must provide input filename");

    if filename == "see gdb! definition in lib.rs about this" {
        ares::interpreter::use_gdb();
        ares::jets::use_gdb();
        ares::jets::bits::use_gdb();
        ares::jets::hash::use_gdb();
        ares::jets::math::use_gdb();
        ares::jets::nock::use_gdb();
        ares::jets::tree::use_gdb();
        ares::mem::use_gdb();
        ares::mug::use_gdb();
        ares::newt::use_gdb();
        ares::noun::use_gdb();
        ares::serf::use_gdb();
        ares::serialization::use_gdb();
    }

    if filename == "serf" {
        return serf(URBIT_HOT_STATE);
    }

    panic!("Ares can only run as a serf!");
}
