fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=./src/pma");
    cc::Build::new()
        .file("./src/pma/malloc.c")
        .file("./src/pma/includes/checksum.c")
        .opt_level(3)
        .compile("pma_malloc");
}
