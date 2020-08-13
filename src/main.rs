use tree_sitter::{Language, Parser};

extern "C" {
    fn tree_sitter_elm() -> Language;
}

fn main() {
    let mut parser = Parser::new();

    let language = unsafe { tree_sitter_elm() };
    parser.set_language(language).unwrap();

    let source_code = "module Main exposing ()";
    let tree = parser.parse(source_code, None).unwrap();

    println!("{}", tree.root_node().to_sexp());
}
