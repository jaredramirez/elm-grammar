use std::fs;
use std::vec::Vec;
use tree_sitter;

extern "C" {
    fn tree_sitter_elm() -> tree_sitter::Language;
}

fn main() {
    let mut parser = tree_sitter::Parser::new();
    let language = unsafe { tree_sitter_elm() };
    parser.set_language(language).unwrap();

    let basic_source_code =
        fs::read_to_string("./samples/Basic.elm").expect("Failed to read Basic.elm");

    let tree = parser.parse(&basic_source_code, None).unwrap();
    let mut cursor = tree.walk();
    cursor.goto_first_child();
    let opt_declaration = node_to_declaration(&basic_source_code, &mut cursor);
    print!("{:?}", opt_declaration);
}

fn node_to_declaration(
    source: &String,
    mut cursor: &mut tree_sitter::TreeCursor,
) -> Option<Declaration> {
    let root_node = cursor.node();
    match root_node.kind() {
        "module_declaration" => {
            let mut module_name = None;
            let mut exposing = None;
            let mut has_child = cursor.goto_first_child();
            while has_child {
                let child_node = cursor.node();
                match child_node.kind() {
                    "upper_case_qid" => {
                        module_name = Some(ModuleName(get_node_text(&source, child_node)));
                    }
                    "exposing_list" => exposing = Some(cursor_to_exposing(&source, &mut cursor)),
                    _ => (),
                }
                has_child = cursor.goto_next_sibling();
            }
            Some(Declaration::Module(Module {
                module_name,
                exposing,
            }))
        }
        _ => None,
    }
}

fn get_node_text(source: &String, node: tree_sitter::Node) -> String {
    source[node.start_byte()..node.end_byte()].to_string()
}

fn cursor_to_exposing(source: &String, cursor: &mut tree_sitter::TreeCursor) -> Exposing {
    let mut exposing_values = Vec::new();

    let parent_node = cursor.node();
    let mut has_child = cursor.goto_first_child();
    while has_child {
        let exposing_child_node = cursor.node();
        match exposing_child_node.kind() {
            "exposed_value" => {
                let has_expsoing_value = cursor.goto_first_child();
                if has_expsoing_value {
                    let exposing_value_node = cursor.node();
                    match exposing_value_node.kind() {
                        "lower_case_identifier" => exposing_values.push(ExposedItem::ExposedValue(
                            VarName(get_node_text(&source, exposing_value_node)),
                        )),
                        _ => (),
                    }
                    cursor.goto_parent();
                }
            }
            "exposed_type" => {
                println!("exposed_type");
                println!("{}", exposing_child_node.to_sexp());
                let has_expsoing_value = cursor.goto_first_child();
                if has_expsoing_value {
                    let exposing_value_node = cursor.node();
                    match exposing_value_node.kind() {
                        "upper_case_identifier" => exposing_values.push(ExposedItem::ExposedType(
                            TypeName(get_node_text(&source, exposing_value_node)),
                        )),
                        _ => (),
                    }
                    cursor.goto_parent();
                }
            }
            _ => (),
        }
        has_child = cursor.goto_next_sibling();
    }
    if parent_node.id() != cursor.node().id() {
        cursor.reset(parent_node);
    }

    Exposing::ExposingSome(exposing_values)
}

// DATA

#[derive(Debug)]
struct Position {
    row: i32,
    col: i32,
}

#[derive(Debug)]
struct Region {
    start: Position,
    end: Position,
}

#[derive(Debug)]
struct Located<Expr> {
    region: Region,
    expr: Expr,
}

#[derive(Debug)]
struct ModuleName(String);

#[derive(Debug)]
struct TypeName(String);

#[derive(Debug)]
struct VarName(String);

#[derive(Debug)]
enum Exposing {
    ExposingAll,
    ExposingSome(Vec<ExposedItem>),
}

#[derive(Debug)]
enum ExposedItem {
    ExposedValue(VarName),
    ExposedType(TypeName),
    ExposedCustomType(VarName, Vec<VarName>),
}

#[derive(Debug)]
struct IfThenElse<Expr> {
    test: Expr,
    then: Expr,
    else_: Expr,
}

#[derive(Debug)]
struct CasePattern<Pattern, Expr> {
    pattern: Pattern,
    expr: Expr,
}

// Canonical AST

// type LocatedExpr = Located<Expr>;

// #[derive(Debug)]
// enum Expr {
// Int(i32),
// Float(f32),
// Char(char),
// String(String),
// Bool(bool),
// Var(ModuleName, VarName),
// Argument(VarName),
// Plus(Box<LocatedExpr>, Box<LocatedExpr>),
// Cons(Box<LocatedExpr>, Box<LocatedExpr>),
// Lambda(VarName, Box<LocatedExpr>),
// Call(Box<LocatedExpr>, Box<LocatedExpr>),
// If(IfThenElse<Box<LocatedExpr>>),
// Let(HashMap<VarName, Box<LocatedExpr>>),
// List(Vec<Box<LocatedExpr>>),
// Unit,
// Tuple(Box<LocatedExpr>, Box<LocatedExpr>),
// Triple(Box<LocatedExpr>, Box<LocatedExpr>, Box<LocatedExpr>),
// Record(HashMap<VarName, Box<LocatedExpr>>),
// Case(
// Box<LocatedExpr>,
// Vec<CasePattern<LocatedPattern, Box<LocatedExpr>>>,
// ),
// }

// type LocatedPattern = Located<Pattern>;

// #[derive(Debug)]
// enum Pattern {
// PAnything,
// PVar(VarName),
// PRecord(Vec<VarName>),
// PAlias(Box<LocatedPattern>),
// PUnit,
// PTuple(Box<LocatedPattern>, Box<LocatedPattern>),
// PTriple(
// Box<LocatedPattern>,
// Box<LocatedPattern>,
// Box<LocatedPattern>,
// ),
// PList(Vec<Box<LocatedPattern>>),
// PInt(i32),
// PFloat(f32),
// PChar(char),
// PString(String),
// PBool(bool),
// }

// Elems

// #[derive(Debug)]
// struct Import {
// module_name: ModuleName,
// as_: Option<ModuleName>,
// exposing: Option<Exposing>,
// }

#[derive(Debug)]
struct Module {
    module_name: Option<ModuleName>,
    exposing: Option<Exposing>,
}

#[derive(Debug)]
enum Declaration {
    // Value(VarName, LocatedExpr),
    // Import(Import),
    Module(Module),
}
