extern crate bindgen;

use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let out_dir = env::var("OUT_DIR").expect("No out dir");
    println!("cargo:rustc-link-lib=murmur3");
    println!("cargo:rustc-link-search=native={}", &out_dir);

    let bindings = bindgen::Builder::default()
        .header("murmur3.h")
        .generate()
        .expect("Unable to generate bindings");

    assert!(Command::new("make")
        .arg("shared")
        .status()
        .expect("Building C lib failed")
        .success());

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(out_dir);
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
