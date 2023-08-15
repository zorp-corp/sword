fn main() {
    use std::env;
    let profile = env::var("PROFILE").unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=./src/pma");

    match profile.as_ref() {
        "debug" => debug(),
        "release" => release(),
        _ => {
            println!("unknown profile: {}", profile);
            std::process::exit(-1);
        }
    }

    cc::Build::new()
        .file("./src/pma/test/malloc.c")
        .opt_level(0)
        .flag("-g3")
        .flag("-Wno-int-conversion")
        .flag("-w")
        .compile("test_pma_malloc_unit");
}

fn debug() {
    cc::Build::new()
        .file("./src/pma/malloc.c")
        .file("./src/pma/includes/checksum.c")
        .opt_level(0)
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
        .compile("pma_malloc");
}

fn release() {
    cc::Build::new()
        .file("./src/pma/malloc.c")
        .file("./src/pma/includes/checksum.c")
        .warnings_into_errors(true)
        .opt_level(3)
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
        .compile("pma_malloc");
}
