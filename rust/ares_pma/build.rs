extern crate bindgen;

use std::env;
use std::fs::create_dir_all;
use std::path::PathBuf;

use bindgen::CargoCallbacks;

fn main() {
    let profile = env::var("PROFILE").unwrap();
    let opt_level = match profile.as_ref() {
        "debug" => 0,
        "release" => 3,
        _ => panic!("Unknown profile: {}", profile),
    };

    // This is the directory where the `c` library is located.
    let libdir_path = PathBuf::from("c-src")
        // Canonicalize the path as `rustc-link-search` requires an absolute
        // path.
        .canonicalize()
        .expect("cannot canonicalize path");
    let libdir_path_str = libdir_path.to_str().expect("Path is not a valid string");

    // This is the path to the `c` headers file.
    let headers_path = libdir_path.join("wrapper.h");
    let headers_path_str = headers_path.to_str().expect("Path is not a valid string");

    println!("cargo:rerun-if-changed={}", libdir_path_str);

    let res = cc::Build::new()
        .file(
            libdir_path
                .join("btree.c")
                .to_str()
                .expect("Path is not a valid string"),
        )
        .file(
            libdir_path
                .join("lib")
                .join("checksum.c")
                .to_str()
                .expect("Path is not a valid string"),
        )
        .flag("-g3")
        .flag("-Wall")
        .flag("-Wextra")
        .flag("-Wpedantic")
        .flag("-Wformat=2")
        .flag("-Wno-unused-parameter")
        .flag("-Wshadow")
        .flag("-Wwrite-strings")
        .flag("-Wstrict-prototypes")
        .flag("-Wold-style-definition")
        .flag("-Wredundant-decls")
        .flag("-Wnested-externs")
        .flag("-Wmissing-include-dirs")
        .try_compile("btree");

    if let Err(err) = res {
        panic!("{}", err);
    }

    // The bindgen::Builder is the main entry point
    // to bindgen, and lets you build up options for
    // the resulting bindings.
    let bindings = bindgen::Builder::default()
        // The input header we would like to generate
        // bindings for.
        .header(headers_path_str)
        // Tell cargo to invalidate the built crate whenever any of the
        // included header files changed.
        .parse_callbacks(Box::new(CargoCallbacks))
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("bindings.rs");
    bindings
        .write_to_file(out_path)
        .expect("Couldn't write bindings!");
}
