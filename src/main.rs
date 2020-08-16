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

fn cursor_to_exposing(source: &String, mut cursor: &mut tree_sitter::TreeCursor) -> ExposingValues {
    let mut exposing_values = Vec::new();
    let has_children = cursor.goto_first_child();
    let mut has_child = has_children;
    while has_child {
        let exposing_child_node = cursor.node();
        match exposing_child_node.kind() {
            "double_dot" => {
                cursor.goto_parent();
                return ExposingValues::ExposingAll;
            }
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
            "exposed_type" => match cursor_to_exposing_type(&source, &mut cursor) {
                Some(exposed_type) => exposing_values.push(exposed_type),
                None => (),
            },
            _ => (),
        }
        has_child = cursor.goto_next_sibling();
    }
    if has_children {
        cursor.goto_parent();
    }
    ExposingValues::ExposingSome(exposing_values)
}

fn cursor_to_exposing_type(
    source: &String,
    mut cursor: &mut tree_sitter::TreeCursor,
) -> Option<ExposedItem> {
    let has_children = cursor.goto_first_child();
    let mut has_child = has_children;

    let mut opt_exposed_type = None;
    let mut opt_custom_type_constructors = None;
    while has_child {
        let exposing_child_node = cursor.node();
        match exposing_child_node.kind() {
            "upper_case_identifier" => {
                opt_exposed_type = Some(TypeName(get_node_text(&source, exposing_child_node)));
            }
            "exposed_union_constructors" => {
                opt_custom_type_constructors =
                    cursor_to_custom_type_constructors(&source, &mut cursor);
            }
            _ => (),
        }
        has_child = cursor.goto_next_sibling();
    }
    if has_children {
        cursor.goto_parent();
    }

    match (opt_exposed_type, opt_custom_type_constructors) {
        (Some(type_name), Some(constructors)) => {
            Some(ExposedItem::ExposedCustomType(type_name, constructors))
        }
        (Some(type_name), None) => Some(ExposedItem::ExposedType(type_name)),
        _ => None,
    }
}

fn cursor_to_custom_type_constructors(
    source: &String,
    cursor: &mut tree_sitter::TreeCursor,
) -> Option<ExposingConstructors> {
    let has_children = cursor.goto_first_child();
    let mut has_child = has_children;

    let mut opt_constructors: Option<Vec<TypeName>> = None;
    while has_child {
        let child_node = cursor.node();
        match child_node.kind() {
            "double_dot" => {
                cursor.goto_parent();
                return Some(ExposingConstructors::ExposingAll);
            }
            "upper_case_identifier" => {
                let mut constructors = opt_constructors.unwrap_or(Vec::new());
                constructors.push(TypeName(get_node_text(&source, child_node)));
                opt_constructors = Some(constructors);
            }
            "lower_case_identifier" => {
                let mut constructors = opt_constructors.unwrap_or(Vec::new());
                constructors.push(TypeName(get_node_text(&source, child_node)));
                opt_constructors = Some(constructors);
            }
            _ => (),
        }
        has_child = cursor.goto_next_sibling();
    }
    if has_children {
        cursor.goto_parent();
    }
    opt_constructors.map(|constructors| ExposingConstructors::ExposingSome(constructors))
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
enum ExposingValues {
    ExposingAll,
    ExposingSome(Vec<ExposedItem>),
}

#[derive(Debug)]
enum ExposedItem {
    ExposedValue(VarName),
    ExposedType(TypeName),
    ExposedCustomType(TypeName, ExposingConstructors),
}

#[derive(Debug)]
enum ExposingConstructors {
    ExposingAll,
    ExposingSome(Vec<TypeName>),
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
    exposing: Option<ExposingValues>,
}

#[derive(Debug)]
enum Declaration {
    // Value(VarName, LocatedExpr),
    // Import(Import),
    Module(Module),
}
