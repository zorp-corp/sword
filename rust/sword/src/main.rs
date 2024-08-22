use sword::jets::hot::URBIT_HOT_STATE;
use sword::serf::serf;
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
        sword::interpreter::use_gdb();
        sword::jets::use_gdb();
        sword::jets::bits::use_gdb();
        sword::jets::hash::use_gdb();
        sword::jets::math::use_gdb();
        sword::jets::nock::use_gdb();
        sword::jets::tree::use_gdb();
        sword::mem::use_gdb();
        sword::mug::use_gdb();
        sword::newt::use_gdb();
        sword::noun::use_gdb();
        sword::serf::use_gdb();
        sword::serialization::use_gdb();
    }

    if filename == "serf" {
        return serf(URBIT_HOT_STATE);
    }

    panic!("Ares can only run as a serf!");
}
