fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=../pma_malloc/");
    cc::Build::new()
        .file("../pma_malloc/src/malloc.c")
        .file("../pma_malloc/src/includes/checksum.c")
        .opt_level(3)
        .compile("pma_malloc");
}
