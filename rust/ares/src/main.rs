use ares::jets::hot::URBIT_HOT_STATE;
use ares::mars::{mars_play, Mars};
use ares::serf::{Context, serf};
use ares::trace::create_trace_file;
use std::env;
use std::io;
use std::path::PathBuf;

fn main() -> io::Result<()> {
    //  debug
    #[cfg(feature = "stop_for_debug")]
    {
        eprintln!("serf: pid {}", std::process::id());
        if unsafe { libc::kill(std::process::id() as i32, libc::SIGSTOP) } != 0 {
            panic!("Could not stop ourselves.");
        };
    }

    let cmd = env::args().nth(1).expect("Must provide input filename");

    if cmd == "see gdb! definition in lib.rs about this" {
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

    if cmd == "serf" {
        // eprintln!("ares: serf\r");
        return serf(URBIT_HOT_STATE);
    } else if cmd == "play" {
        let pier_path = PathBuf::from(
            env::args()
                .nth(2)
                .expect("Must provide path to log directory"),
        );

        // eprintln!("\rares: play: loading context from {:?}\r", pier_path);
        let trace_path = pier_path.clone();
        let trace_info = create_trace_file(trace_path).ok();
        let mut ctx = Context::load(pier_path.clone(), trace_info, URBIT_HOT_STATE);
        ctx.ripe();
        
        let sent = ctx.event_num;
        let done = sent;

        let mars = Mars {
            ctx: ctx,
            dir: pier_path,
            sent: sent,
            done: done,
        };

        let eve = env::args()
            .nth(4)
            .expect("Must provide event number to play up to")
            .parse::<u64>()
            .expect("Failed to parse event number");

        let sap = env::args()
            .nth(6)
            .expect("Must provide snapshot interval")
            .parse::<u64>()
            .expect("Failed to parse snapshot interval");

        mars_play(mars, eve, sap);
        // eprintln!("play: done\r");
    }

    Ok(())
}
