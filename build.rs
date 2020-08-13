use std::path::PathBuf;

fn main() {
    let dir: PathBuf = ["tree-sitter-elm", "src"].iter().collect();
    cc::Build::new()
        .include(&dir)
        .cpp(true)
        .file(dir.join("parser.c"))
        .file(dir.join("scanner.cc"))
        .compile("tree-sitter-elm");
}
