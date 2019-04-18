type parser;
type language;
type tree;
[@bs.new] [@bs.module] external makeParser: unit => parser = "tree-sitter";
[@bs.send] external setLanguage: (parser, language) => unit = "";
[@bs.send] external parse: (parser, string) => tree = "";
[@bs.module] external elm: language = "tree-sitter-elm";

type elmImport;
[@bs.module] external elmWorker: elmImport = "./Main.elm";

let parser = makeParser();
setLanguage(parser, elm);

let source = "
import World exposing (Hello)
";

let parsed = parse(parser, source);

[%raw "console.log(parsed.rootNode.child(0).child(2).namedChildren)"];
