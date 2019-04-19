module ParserTest exposing (test)

import Elm.AST as Elm
import Elm.Parser as Elm
import Expect
import Parser
import Test


test : Test.Test
test =
    Test.describe "Parser Tests"
        [ tokenTests
        , moduleNameTests
        , operatorTests
        , exposedItemTests
        , exposingListTests
        , moduleDeclarationTests
        , moduleImportTests
        , sourceTests
        , patternTests
        ]


tokenTests : Test.Test
tokenTests =
    Test.describe "Tokens Tests"
        [ Test.test "Uppercase Identifier" <|
            \_ ->
                let
                    source =
                        "Hello"
                in
                Expect.equal (Ok <| Elm.UppercaseIdentifier "Hello")
                    (Parser.run Elm.uppercaseIdentifier source)
        , Test.test "Lowercase Identifier" <|
            \_ ->
                let
                    source =
                        "hello"
                in
                Expect.equal (Ok <| Elm.LowercaseIdentifier "hello")
                    (Parser.run Elm.lowercaseIdentifier source)
        ]


moduleNameTests : Test.Test
moduleNameTests =
    Test.describe "Module Name Tests"
        [ Test.test "Single ModuleName" <|
            \_ ->
                let
                    source =
                        "Hello"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleName
                            (Elm.UppercaseIdentifier "Hello")
                            []
                    )
                    (Parser.run Elm.moduleName source)
        , Test.test "Many ModuleNames" <|
            \_ ->
                let
                    source =
                        "Hello.World.HowYaDoing"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleName
                            (Elm.UppercaseIdentifier "Hello")
                            [ Elm.UppercaseIdentifier "World"
                            , Elm.UppercaseIdentifier "HowYaDoing"
                            ]
                    )
                    (Parser.run Elm.moduleName source)
        , Test.test "Single ModuleName trailing '.'" <|
            \_ ->
                let
                    source =
                        "Hello."
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleName
                            (Elm.UppercaseIdentifier "Hello")
                            []
                    )
                    (Parser.run Elm.moduleName source)
        , Test.test "Many ModuleName trailing '.'" <|
            \_ ->
                let
                    source =
                        "Hello.World."
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleName (Elm.UppercaseIdentifier "Hello")
                            [ Elm.UppercaseIdentifier "World" ]
                    )
                    (Parser.run Elm.moduleName source)
        ]


exposedItemTests : Test.Test
exposedItemTests =
    Test.describe "Exposed Item Tests"
        [ Test.test "ExposedValue" <|
            \_ ->
                let
                    source =
                        "hello"
                in
                Expect.equal (Ok <| Elm.ExposedValue (Elm.LowercaseIdentifier "hello"))
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedUnion Opaque" <|
            \_ ->
                let
                    source =
                        "String"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposedType (Elm.UppercaseIdentifier "String")
                            Elm.NoExposedConstructors
                    )
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedUnion with DoubleDot" <|
            \_ ->
                let
                    source =
                        "String(..)"
                in
                Expect.equal
                    (Ok <| Elm.ExposedType (Elm.UppercaseIdentifier "String") Elm.ExposedConstructorsDotDot)
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedUnion with Constructors" <|
            \_ ->
                let
                    source =
                        "String(Str)"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposedType (Elm.UppercaseIdentifier "String")
                            (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ])
                    )
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedUnion with Constructors trailing" <|
            \_ ->
                let
                    source =
                        "String(Str, )"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposedType (Elm.UppercaseIdentifier "String")
                            (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ])
                    )
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedUnion with Constructors missing parenthesis" <|
            \_ ->
                let
                    source =
                        "String(Str"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposedType (Elm.UppercaseIdentifier "String")
                            (Elm.ExposedConstructors
                                [ Elm.UppercaseIdentifier "Str" ]
                            )
                    )
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedUnion with Constructors trailing and missing parenthesis" <|
            \_ ->
                let
                    source =
                        "String(Str,"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposedType (Elm.UppercaseIdentifier "String")
                            (Elm.ExposedConstructors
                                [ Elm.UppercaseIdentifier "Str" ]
                            )
                    )
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedOperator" <|
            \_ ->
                let
                    source =
                        "(++)"
                in
                Expect.equal (Ok <| Elm.ExposedOperator Elm.PlusPlus)
                    (Parser.run Elm.exposedItem source)
        ]


exposingListTests : Test.Test
exposingListTests =
    Test.describe "Exposing List Tests"
        [ Test.test "Exposing value" <|
            \_ ->
                let
                    source =
                        "(hello)"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposingList
                            [ Elm.ExposedValue (Elm.LowercaseIdentifier "hello")
                            ]
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing type opaque" <|
            \_ ->
                let
                    source =
                        "(Hello)"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposingList
                            [ Elm.ExposedType (Elm.UppercaseIdentifier "Hello")
                                Elm.NoExposedConstructors
                            ]
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing type all constructors" <|
            \_ ->
                let
                    source =
                        "(Hello(..))"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposingList
                            [ Elm.ExposedType (Elm.UppercaseIdentifier "Hello")
                                Elm.ExposedConstructorsDotDot
                            ]
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing type some constructors" <|
            \_ ->
                let
                    source =
                        "(Hello(World))"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposingList
                            [ Elm.ExposedType (Elm.UppercaseIdentifier "Hello")
                                (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "World" ])
                            ]
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing operator" <|
            \_ ->
                let
                    source =
                        "((++))"
                in
                Expect.equal
                    (Ok <| Elm.ExposingList [ Elm.ExposedOperator Elm.PlusPlus ])
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing many" <|
            \_ ->
                let
                    source =
                        "((++), myValue, String(Str))"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposingList
                            [ Elm.ExposedOperator Elm.PlusPlus
                            , Elm.ExposedValue (Elm.LowercaseIdentifier "myValue")
                            , Elm.ExposedType
                                (Elm.UppercaseIdentifier "String")
                                (Elm.ExposedConstructors
                                    [ Elm.UppercaseIdentifier "Str" ]
                                )
                            ]
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing many trailing" <|
            \_ ->
                let
                    source =
                        "((++), myValue, String(Str),)"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposingList
                            [ Elm.ExposedOperator Elm.PlusPlus
                            , Elm.ExposedValue (Elm.LowercaseIdentifier "myValue")
                            , Elm.ExposedType
                                (Elm.UppercaseIdentifier "String")
                                (Elm.ExposedConstructors
                                    [ Elm.UppercaseIdentifier "Str" ]
                                )
                            ]
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing many trailing in the middle" <|
            \_ ->
                let
                    source =
                        "((++), myValue, , String(Str))"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposingList
                            [ Elm.ExposedOperator Elm.PlusPlus
                            , Elm.ExposedValue (Elm.LowercaseIdentifier "myValue")
                            , Elm.ExposedType
                                (Elm.UppercaseIdentifier "String")
                                (Elm.ExposedConstructors
                                    [ Elm.UppercaseIdentifier "Str" ]
                                )
                            ]
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing many trailing trailing" <|
            \_ ->
                let
                    source =
                        "((++), myValue, String(Str,),)"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposingList
                            [ Elm.ExposedOperator Elm.PlusPlus
                            , Elm.ExposedValue (Elm.LowercaseIdentifier "myValue")
                            , Elm.ExposedType
                                (Elm.UppercaseIdentifier "String")
                                (Elm.ExposedConstructors
                                    [ Elm.UppercaseIdentifier "Str" ]
                                )
                            ]
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing many missing parenthesis" <|
            \_ ->
                let
                    source =
                        "((++)"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposingList [ Elm.ExposedOperator Elm.PlusPlus ]
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing many missing parenthesis and trailing" <|
            \_ ->
                let
                    source =
                        "((++),"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposingList [ Elm.ExposedOperator Elm.PlusPlus ]
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing multiline" <|
            \_ ->
                let
                    source =
                        """( (++)
                           , hello
                           )
                        """
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposingList
                            [ Elm.ExposedOperator Elm.PlusPlus
                            , Elm.ExposedValue (Elm.LowercaseIdentifier "hello")
                            ]
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing multiline in the middle" <|
            \_ ->
                let
                    source =
                        """( (++),
                           , hello
                           )
                        """
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposingList
                            [ Elm.ExposedOperator Elm.PlusPlus
                            , Elm.ExposedValue (Elm.LowercaseIdentifier "hello")
                            ]
                    )
                    (Parser.run Elm.exposingList source)
        ]


operatorTests : Test.Test
operatorTests =
    Test.describe "Operator Tests"
        [ Test.test "++" <|
            \_ ->
                let
                    source =
                        "++"
                in
                Expect.equal
                    (Ok Elm.PlusPlus)
                    (Parser.run Elm.operator source)
        , Test.test "+" <|
            \_ ->
                let
                    source =
                        "+"
                in
                Expect.equal
                    (Ok Elm.Plus)
                    (Parser.run Elm.operator source)
        , Test.test "-" <|
            \_ ->
                let
                    source =
                        "-"
                in
                Expect.equal
                    (Ok Elm.Minus)
                    (Parser.run Elm.operator source)
        , Test.test "*" <|
            \_ ->
                let
                    source =
                        "*"
                in
                Expect.equal
                    (Ok Elm.Multiply)
                    (Parser.run Elm.operator source)
        , Test.test "/" <|
            \_ ->
                let
                    source =
                        "/"
                in
                Expect.equal
                    (Ok Elm.DivideFloat)
                    (Parser.run Elm.operator source)
        , Test.test "//" <|
            \_ ->
                let
                    source =
                        "//"
                in
                Expect.equal
                    (Ok Elm.DivideInt)
                    (Parser.run Elm.operator source)
        , Test.test "|>" <|
            \_ ->
                let
                    source =
                        "|>"
                in
                Expect.equal
                    (Ok Elm.RightPipe)
                    (Parser.run Elm.operator source)
        , Test.test "<|" <|
            \_ ->
                let
                    source =
                        "<|"
                in
                Expect.equal
                    (Ok Elm.LeftPipe)
                    (Parser.run Elm.operator source)
        , Test.test "|=" <|
            \_ ->
                let
                    source =
                        "|="
                in
                Expect.equal
                    (Ok Elm.ParseKeep)
                    (Parser.run Elm.operator source)
        , Test.test "|." <|
            \_ ->
                let
                    source =
                        "|."
                in
                Expect.equal
                    (Ok Elm.ParseIgnore)
                    (Parser.run Elm.operator source)
        , Test.test ">=" <|
            \_ ->
                let
                    source =
                        ">="
                in
                Expect.equal
                    (Ok Elm.GreaterThan)
                    (Parser.run Elm.operator source)
        , Test.test "<=" <|
            \_ ->
                let
                    source =
                        "<="
                in
                Expect.equal
                    (Ok Elm.LessThan)
                    (Parser.run Elm.operator source)
        ]


moduleDeclarationTests : Test.Test
moduleDeclarationTests =
    Test.describe "Module Declaration Testz"
        [ Test.test "Module name declaration" <|
            \_ ->
                let
                    source =
                        "module Hello"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclarationPartial
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module name declaration with capital" <|
            \_ ->
                let
                    source =
                        "Module Hello"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclarationPartial
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module declaration unfinished exposing" <|
            \_ ->
                let
                    source =
                        "module Hello exposing"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclarationPartial
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module declaration with open parenthesis" <|
            \_ ->
                let
                    source =
                        "module Hello exposing ("
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclaration
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [])
                            (Elm.ExposingList [])
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module declaration with exposing nothing" <|
            \_ ->
                let
                    source =
                        "module Hello exposing ()"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclaration
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.ExposingList [])
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module declaration with exposing something" <|
            \_ ->
                let
                    source =
                        "module Hello exposing (value, String(Str), Int(..), (|>))"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclaration
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ])
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator Elm.RightPipe
                                ]
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module declaration with exposing something multiline" <|
            \_ ->
                let
                    source =
                        """module Hello exposing
                         ( value
                         , String(Str)
                         , Int(..)
                         , (|>)
                         )
                        """
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclaration
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ])
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator Elm.RightPipe
                                ]
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module declaration with exposing something trailing" <|
            \_ ->
                let
                    source =
                        "module Hello exposing (value, String(Str),)"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclaration
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ])
                                ]
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module declaration with exposing something trailing multiline" <|
            \_ ->
                let
                    source =
                        """module Hello exposing
                         ( value
                         , String(Str)
                         , Int(..)
                         , (|>)
                         ,
                        """
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclaration
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ])
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator Elm.RightPipe
                                ]
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module declaration with exposing something trailing with no closing parenthesis" <|
            \_ ->
                let
                    source =
                        "module Hello exposing (value, String(Str),"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclaration
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ])
                                ]
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module declaration with exposing something trailing with no closing parenthesis multiline" <|
            \_ ->
                let
                    source =
                        """module Hello exposing
                         ( value
                         , String(Str)
                         , Int(..)
                         , (|>)
                         ,
                        """
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclaration
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ])
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator Elm.RightPipe
                                ]
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module declaration with exposing something with no closing parenthesis" <|
            \_ ->
                let
                    source =
                        "module Hello exposing (value, String(Str), "
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclaration
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ])
                                ]
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module declaration with exposing something with no closing parenthesis multiline" <|
            \_ ->
                let
                    source =
                        """module Hello exposing
                         ( value
                         , String(Str)
                         , Int(..)
                         , (|>)
                        """
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclaration
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ])
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator Elm.RightPipe
                                ]
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        ]


moduleImportTests : Test.Test
moduleImportTests =
    Test.describe "Module Import Tests"
        [ Test.test "Module import " <|
            \_ ->
                let
                    source =
                        "import "
                in
                Expect.equal
                    (Ok Elm.ModuleImportIncomplete)
                    (Parser.run Elm.moduleImport source)
        , Test.test "Module import name declaration" <|
            \_ ->
                let
                    source =
                        "import Hello"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleImport
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            Elm.AliasNone
                            Nothing
                    )
                    (Parser.run Elm.moduleImport source)
        , Test.test "Module import name declaration trailing" <|
            \_ ->
                let
                    source =
                        "import Hello."
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleImport
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            Elm.AliasNone
                            Nothing
                    )
                    (Parser.run Elm.moduleImport source)
        , Test.test "Module import declaration alias partial" <|
            \_ ->
                let
                    source =
                        "import Hello as"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleImport
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            Elm.AliasPartial
                            Nothing
                    )
                    (Parser.run Elm.moduleImport source)
        , Test.test "Module import declaration alias" <|
            \_ ->
                let
                    source =
                        "import Hello as H"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleImport
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.Alias (Elm.UppercaseIdentifier "H"))
                            Nothing
                    )
                    (Parser.run Elm.moduleImport source)
        , Test.test "Module import declaration exposing" <|
            \_ ->
                let
                    source =
                        "import Hello exposing (world)"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleImport
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            Elm.AliasNone
                            (Just <|
                                Elm.ExposingList
                                    [ Elm.ExposedValue (Elm.LowercaseIdentifier "world") ]
                            )
                    )
                    (Parser.run Elm.moduleImport source)
        , Test.test "Module import declaration with alias and exposing" <|
            \_ ->
                let
                    source =
                        "import Hello as H exposing (world)"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleImport
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.Alias (Elm.UppercaseIdentifier "H"))
                            (Just <|
                                Elm.ExposingList [ Elm.ExposedValue (Elm.LowercaseIdentifier "world") ]
                            )
                    )
                    (Parser.run Elm.moduleImport source)
        , Test.test "Module import declaration with alias and exposing partial" <|
            \_ ->
                let
                    source =
                        "import Hello as H exposing"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleImport
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.Alias (Elm.UppercaseIdentifier "H"))
                            Nothing
                    )
                    (Parser.run Elm.moduleImport source)
        , Test.test "Module import declaration with alias and exposing trailing" <|
            \_ ->
                let
                    source =
                        "import Hello as H exposing ("
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleImport
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.Alias (Elm.UppercaseIdentifier "H"))
                            (Just <| Elm.ExposingList [])
                    )
                    (Parser.run Elm.moduleImport source)
        , Test.test "Module import declaration with alias and exposing multiline" <|
            \_ ->
                let
                    source =
                        """import Hello as H exposing
                             ( world,
                             , myGoodness
                             )
                        """
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleImport
                            (Elm.ModuleName
                                (Elm.UppercaseIdentifier "Hello")
                                []
                            )
                            (Elm.Alias (Elm.UppercaseIdentifier "H"))
                            (Just <|
                                Elm.ExposingList
                                    [ Elm.ExposedValue (Elm.LowercaseIdentifier "world")
                                    , Elm.ExposedValue (Elm.LowercaseIdentifier "myGoodness")
                                    ]
                            )
                    )
                    (Parser.run Elm.moduleImport source)
        ]


sourceTests : Test.Test
sourceTests =
    Test.describe "Source tests"
        [ Test.test "Simple Module import " <|
            \_ ->
                let
                    source =
                        """
                        module Ya exposing ()
                        import Hello
                        """
                in
                Expect.equal
                    (Ok
                        (Elm.Elm
                            (Just
                                (Elm.ModuleDeclaration
                                    (Elm.ModuleName
                                        (Elm.UppercaseIdentifier "Ya")
                                        []
                                    )
                                    (Elm.ExposingList [])
                                )
                            )
                            [ Elm.ModuleImport
                                (Elm.ModuleName
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                )
                                Elm.AliasNone
                                Nothing
                            ]
                        )
                    )
                    (Parser.run Elm.elm source)
        , Test.test "Module import declaration trailing" <|
            \_ ->
                let
                    source =
                        """
                                module Ya exposing
                                    ( myValue
                                    ,
                                    )
                                import Hello as H
                                import World exposing (World(..), world)
                                import Parser as P exposing ((|.))
                                """
                in
                Expect.equal
                    (Ok
                        (Elm.Elm
                            (Just
                                (Elm.ModuleDeclaration
                                    (Elm.ModuleName
                                        (Elm.UppercaseIdentifier "Ya")
                                        []
                                    )
                                    (Elm.ExposingList
                                        [ Elm.ExposedValue (Elm.LowercaseIdentifier "myValue")
                                        ]
                                    )
                                )
                            )
                            [ Elm.ModuleImport
                                (Elm.ModuleName
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                )
                                (Elm.Alias (Elm.UppercaseIdentifier "H"))
                                Nothing
                            , Elm.ModuleImport
                                (Elm.ModuleName
                                    (Elm.UppercaseIdentifier "World")
                                    []
                                )
                                Elm.AliasNone
                                (Just <|
                                    Elm.ExposingList
                                        [ Elm.ExposedType (Elm.UppercaseIdentifier "World")
                                            Elm.ExposedConstructorsDotDot
                                        , Elm.ExposedValue (Elm.LowercaseIdentifier "world")
                                        ]
                                )
                            , Elm.ModuleImport
                                (Elm.ModuleName
                                    (Elm.UppercaseIdentifier "Parser")
                                    []
                                )
                                (Elm.Alias (Elm.UppercaseIdentifier "P"))
                                (Just <|
                                    Elm.ExposingList [ Elm.ExposedOperator Elm.ParseIgnore ]
                                )
                            ]
                        )
                    )
                    (Parser.run Elm.elm source)
        , Test.test "Module without declaration" <|
            \_ ->
                let
                    source =
                        """
                        import Hello
                        """
                in
                Expect.equal
                    (Ok
                        (Elm.Elm Nothing
                            [ Elm.ModuleImport
                                (Elm.ModuleName
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                )
                                Elm.AliasNone
                                Nothing
                            ]
                        )
                    )
                    (Parser.run Elm.elm source)
        ]


patternTests : Test.Test
patternTests =
    Test.describe "Pattern Tests"
        [ Test.test "Anything " <|
            \_ ->
                let
                    source =
                        "_"
                in
                Expect.equal
                    (Ok Elm.AnythingPattern)
                    (Parser.run Elm.pattern source)
        , Test.test "Lower" <|
            \_ ->
                let
                    source =
                        "variable"
                in
                Expect.equal
                    (Ok (Elm.LowerPattern <| Elm.LowercaseIdentifier "variable"))
                    (Parser.run Elm.pattern source)
        , Test.test "Tuple" <|
            \_ ->
                let
                    source =
                        """(  h, _ )"""
                in
                Expect.equal
                    (Ok
                        (Elm.TuplePattern
                            (Elm.LowerPattern (Elm.LowercaseIdentifier "h"))
                            Elm.AnythingPattern
                            Nothing
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Triple" <|
            \_ ->
                let
                    source =
                        """(  h, _  , yikes)"""
                in
                Expect.equal
                    (Ok
                        (Elm.TuplePattern
                            (Elm.LowerPattern (Elm.LowercaseIdentifier "h"))
                            Elm.AnythingPattern
                            (Just (Elm.LowerPattern (Elm.LowercaseIdentifier "yikes")))
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Tuple nested" <|
            \_ ->
                let
                    source =
                        """(  (_, myThing), _ )"""
                in
                Expect.equal
                    (Ok
                        (Elm.TuplePattern
                            (Elm.TuplePattern
                                Elm.AnythingPattern
                                (Elm.LowerPattern (Elm.LowercaseIdentifier "myThing"))
                                Nothing
                            )
                            Elm.AnythingPattern
                            Nothing
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Unit" <|
            \_ ->
                let
                    source =
                        """()"""
                in
                Expect.equal
                    (Ok Elm.UnitPattern)
                    (Parser.run Elm.pattern source)
        , Test.test "Record" <|
            \_ ->
                let
                    source =
                        """{ hello, world}"""
                in
                Expect.equal
                    (Ok (Elm.RecordPattern [ Elm.LowercaseIdentifier "hello", Elm.LowercaseIdentifier "world" ]))
                    (Parser.run Elm.pattern source)
        , Test.test "Parenthesis (Record)" <|
            \_ ->
                let
                    source =
                        """({ hello, world})"""
                in
                Expect.equal
                    (Ok
                        (Elm.ParenthesisPattern
                            (Elm.RecordPattern [ Elm.LowercaseIdentifier "hello", Elm.LowercaseIdentifier "world" ])
                            Nothing
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Parenthesis (Record) with alias" <|
            \_ ->
                let
                    source =
                        """({ hello, world} as oi)"""
                in
                Expect.equal
                    (Ok
                        (Elm.ParenthesisPattern
                            (Elm.RecordPattern [ Elm.LowercaseIdentifier "hello", Elm.LowercaseIdentifier "world" ])
                            (Just (Elm.LowercaseIdentifier "oi"))
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Parenthesis (Tuple) with alias" <|
            \_ ->
                let
                    source =
                        """((myMan, _) as aaaabbbb )"""
                in
                Expect.equal
                    (Ok
                        (Elm.ParenthesisPattern
                            (Elm.TuplePattern
                                (Elm.LowerPattern (Elm.LowercaseIdentifier "myMan"))
                                Elm.AnythingPattern
                                Nothing
                            )
                            (Just (Elm.LowercaseIdentifier "aaaabbbb"))
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "List (empty)" <|
            \_ ->
                let
                    source =
                        """[]"""
                in
                Expect.equal (Ok (Elm.ListPattern []))
                    (Parser.run Elm.pattern source)
        , Test.test "List (one)" <|
            \_ ->
                let
                    source =
                        """[element]"""
                in
                Expect.equal (Ok (Elm.ListPattern [ Elm.LowerPattern (Elm.LowercaseIdentifier "element") ]))
                    (Parser.run Elm.pattern source)
        , Test.test "List (two)" <|
            \_ ->
                let
                    source =
                        """[element, _]"""
                in
                Expect.equal
                    (Ok
                        (Elm.ListPattern
                            [ Elm.LowerPattern (Elm.LowercaseIdentifier "element")
                            , Elm.AnythingPattern
                            ]
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "List  trailing(two)" <|
            \_ ->
                let
                    source =
                        """[element, _, ]"""
                in
                Expect.equal
                    (Ok
                        (Elm.ListPattern
                            [ Elm.LowerPattern (Elm.LowercaseIdentifier "element")
                            , Elm.AnythingPattern
                            ]
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Cons empty list" <|
            \_ ->
                let
                    source =
                        """element :: []"""
                in
                Expect.equal
                    (Ok
                        (Elm.ConsPattern
                            (Elm.LowerPattern (Elm.LowercaseIdentifier "element"))
                            (Elm.ListPattern [])
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Cons cons empty list" <|
            \_ ->
                let
                    source =
                        """element :: _ :: []"""
                in
                Expect.equal
                    (Ok
                        (Elm.ConsPattern
                            (Elm.LowerPattern (Elm.LowercaseIdentifier "element"))
                            (Elm.ConsPattern
                                Elm.AnythingPattern
                                (Elm.ListPattern [])
                            )
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Char" <|
            \_ ->
                let
                    source =
                        """'a'"""
                in
                Expect.equal
                    (Ok (Elm.CharPattern "a"))
                    (Parser.run Elm.pattern source)
        , Test.test "String" <|
            \_ ->
                let
                    source =
                        "\"Hello\""
                in
                Expect.equal
                    (Ok (Elm.StringPattern "Hello"))
                    (Parser.run Elm.pattern source)
        , Test.test "Int" <|
            \_ ->
                let
                    source =
                        "123"
                in
                Expect.equal
                    (Ok (Elm.IntPattern 123))
                    (Parser.run Elm.pattern source)
        , Test.test "Float" <|
            \_ ->
                let
                    source =
                        "1.2"
                in
                Expect.equal
                    (Ok (Elm.FloatPattern 1.2))
                    (Parser.run Elm.pattern source)
        , Test.test "Ctor" <|
            \_ ->
                let
                    source =
                        "Hello _ world"
                in
                Expect.equal
                    (Ok
                        (Elm.CtorPattern (Elm.UppercaseIdentifier "Hello")
                            [ Elm.AnythingPattern
                            , Elm.LowerPattern (Elm.LowercaseIdentifier "world")
                            ]
                        )
                    )
                    (Parser.run Elm.pattern source)
        ]