module ParserTest exposing (test)

import Elm.AST as Elm
import Elm.Parser as Elm
import Expect
import Parser.Advanced as Parser exposing ((|.), (|=))
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
        , expressionTests
        , declarationTests
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
                Expect.equal (Ok <| "Hello")
                    (Parser.run Elm.uppercaseIdentifier source)
        , Test.test "Lowercase Identifier" <|
            \_ ->
                let
                    source =
                        "hello"
                in
                Expect.equal (Ok <| "hello")
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
                            "Hello"
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
                            "Hello"
                            [ "World"
                            , "HowYaDoing"
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
                            "Hello"
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
                        Elm.ModuleName "Hello"
                            [ "World" ]
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
                Expect.equal (Ok <| Elm.ExposedValue "hello")
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedUnion Opaque" <|
            \_ ->
                let
                    source =
                        "String"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposedType "String"
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
                    (Ok <| Elm.ExposedType "String" Elm.ExposedConstructorsDotDot)
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedUnion with Constructors" <|
            \_ ->
                let
                    source =
                        "String(Str)"
                in
                Expect.equal
                    (Ok <|
                        Elm.ExposedType "String"
                            (Elm.ExposedConstructors [ "Str" ])
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
                        Elm.ExposedType "String"
                            (Elm.ExposedConstructors [ "Str" ])
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
                        Elm.ExposedType "String"
                            (Elm.ExposedConstructors
                                [ "Str" ]
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
                        Elm.ExposedType "String"
                            (Elm.ExposedConstructors
                                [ "Str" ]
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
                            [ Elm.ExposedValue "hello"
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
                            [ Elm.ExposedType "Hello"
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
                            [ Elm.ExposedType "Hello"
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
                            [ Elm.ExposedType "Hello"
                                (Elm.ExposedConstructors [ "World" ])
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
                            , Elm.ExposedValue "myValue"
                            , Elm.ExposedType
                                "String"
                                (Elm.ExposedConstructors
                                    [ "Str" ]
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
                            , Elm.ExposedValue "myValue"
                            , Elm.ExposedType
                                "String"
                                (Elm.ExposedConstructors
                                    [ "Str" ]
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
                            , Elm.ExposedValue "myValue"
                            , Elm.ExposedType
                                "String"
                                (Elm.ExposedConstructors
                                    [ "Str" ]
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
                            , Elm.ExposedValue "myValue"
                            , Elm.ExposedType
                                "String"
                                (Elm.ExposedConstructors
                                    [ "Str" ]
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
                            , Elm.ExposedValue "hello"
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
                            , Elm.ExposedValue "hello"
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
                    (Ok Elm.GreaterThanOrEqual)
                    (Parser.run Elm.operator source)
        , Test.test ">" <|
            \_ ->
                let
                    source =
                        ">"
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
                    (Ok Elm.LessThanOrEqual)
                    (Parser.run Elm.operator source)
        , Test.test "<" <|
            \_ ->
                let
                    source =
                        "<"
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
                                "Hello"
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
                                "Hello"
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
                            (Elm.ModuleName "Hello" [])
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
                                "Hello"
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
                                "Hello"
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue "value"
                                , Elm.ExposedType "String"
                                    (Elm.ExposedConstructors [ "Str" ])
                                , Elm.ExposedType "Int" Elm.ExposedConstructorsDotDot
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
                                "Hello"
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue "value"
                                , Elm.ExposedType "String"
                                    (Elm.ExposedConstructors [ "Str" ])
                                , Elm.ExposedType "Int" Elm.ExposedConstructorsDotDot
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
                                "Hello"
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue "value"
                                , Elm.ExposedType "String"
                                    (Elm.ExposedConstructors [ "Str" ])
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
                                "Hello"
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue "value"
                                , Elm.ExposedType "String"
                                    (Elm.ExposedConstructors [ "Str" ])
                                , Elm.ExposedType "Int" Elm.ExposedConstructorsDotDot
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
                                "Hello"
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue "value"
                                , Elm.ExposedType "String"
                                    (Elm.ExposedConstructors [ "Str" ])
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
                                "Hello"
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue "value"
                                , Elm.ExposedType "String"
                                    (Elm.ExposedConstructors [ "Str" ])
                                , Elm.ExposedType "Int" Elm.ExposedConstructorsDotDot
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
                                "Hello"
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue "value"
                                , Elm.ExposedType "String"
                                    (Elm.ExposedConstructors [ "Str" ])
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
                                "Hello"
                                []
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue "value"
                                , Elm.ExposedType "String"
                                    (Elm.ExposedConstructors [ "Str" ])
                                , Elm.ExposedType "Int" Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator Elm.RightPipe
                                ]
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        ]


moduleImportTests : Test.Test
moduleImportTests =
    Test.describe "Module Import Tests"
        [ Test.test "Module import name declaration" <|
            \_ ->
                let
                    source =
                        "import Hello"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleImport
                            (Elm.ModuleName "Hello" [])
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
                                "Hello"
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
                                "Hello"
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
                                "Hello"
                                []
                            )
                            (Elm.Alias "H")
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
                                "Hello"
                                []
                            )
                            Elm.AliasNone
                            (Just <|
                                Elm.ExposingList
                                    [ Elm.ExposedValue "world" ]
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
                                "Hello"
                                []
                            )
                            (Elm.Alias "H")
                            (Just <|
                                Elm.ExposingList [ Elm.ExposedValue "world" ]
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
                                "Hello"
                                []
                            )
                            (Elm.Alias "H")
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
                                "Hello"
                                []
                            )
                            (Elm.Alias "H")
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
                                "Hello"
                                []
                            )
                            (Elm.Alias "H")
                            (Just <|
                                Elm.ExposingList
                                    [ Elm.ExposedValue "world"
                                    , Elm.ExposedValue "myGoodness"
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
                                        "Ya"
                                        []
                                    )
                                    (Elm.ExposingList [])
                                )
                            )
                            [ Elm.ModuleImport
                                (Elm.ModuleName
                                    "Hello"
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
                                        "Ya"
                                        []
                                    )
                                    (Elm.ExposingList
                                        [ Elm.ExposedValue "myValue"
                                        ]
                                    )
                                )
                            )
                            [ Elm.ModuleImport
                                (Elm.ModuleName
                                    "Hello"
                                    []
                                )
                                (Elm.Alias "H")
                                Nothing
                            , Elm.ModuleImport
                                (Elm.ModuleName
                                    "World"
                                    []
                                )
                                Elm.AliasNone
                                (Just <|
                                    Elm.ExposingList
                                        [ Elm.ExposedType "World"
                                            Elm.ExposedConstructorsDotDot
                                        , Elm.ExposedValue "world"
                                        ]
                                )
                            , Elm.ModuleImport
                                (Elm.ModuleName
                                    "Parser"
                                    []
                                )
                                (Elm.Alias "P")
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
                                    "Hello"
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
                    (Ok (Elm.LowerPattern <| "variable"))
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
                            (Elm.LowerPattern "h")
                            Elm.AnythingPattern
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
                        (Elm.TriplePattern
                            (Elm.LowerPattern "h")
                            Elm.AnythingPattern
                            (Elm.LowerPattern "yikes")
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
                                (Elm.LowerPattern "myThing")
                            )
                            Elm.AnythingPattern
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
                    (Ok
                        (Elm.RecordPattern
                            [ "hello"
                            , "world"
                            ]
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Parenthesis (Record)" <|
            \_ ->
                let
                    source =
                        """({ hello, world})"""
                in
                Expect.equal
                    (Ok
                        (Elm.RecordPattern
                            [ "hello"
                            , "world"
                            ]
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
                        (Elm.AliasPattern
                            (Elm.RecordPattern
                                [ "hello"
                                , "world"
                                ]
                            )
                            "oi"
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Tuple with alias" <|
            \_ ->
                let
                    source =
                        """(myMan, _) as aaaabbbb"""
                in
                Expect.equal
                    (Ok
                        (Elm.AliasPattern
                            (Elm.TuplePattern
                                (Elm.LowerPattern "myMan")
                                Elm.AnythingPattern
                            )
                            "aaaabbbb"
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
                Expect.equal (Ok (Elm.ListPattern [ Elm.LowerPattern "element" ]))
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
                            [ Elm.LowerPattern "element"
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
                            [ Elm.LowerPattern "element"
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
                            (Elm.LowerPattern "element")
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
                            (Elm.LowerPattern "element")
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
                        (Elm.CtorPattern "Hello"
                            [ Elm.AnythingPattern
                            , Elm.LowerPattern "world"
                            ]
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Ctor with sub ctor" <|
            \_ ->
                let
                    source =
                        "Hello (World world) whatUp"
                in
                Expect.equal
                    (Ok
                        (Elm.CtorPattern "Hello"
                            [ Elm.CtorPattern "World"
                                [ Elm.LowerPattern "world"
                                ]
                            , Elm.LowerPattern "whatUp"
                            ]
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Ctor with sub pattern alias" <|
            \_ ->
                let
                    source =
                        "World ({world} as w)"
                in
                Expect.equal
                    (Ok
                        (Elm.CtorPattern "World"
                            [ Elm.AliasPattern
                                (Elm.RecordPattern [ "world" ])
                                "w"
                            ]
                        )
                    )
                    (Parser.run Elm.pattern source)
        , Test.test "Ctor with alias" <|
            \_ ->
                let
                    source =
                        "(Hello world) as h"
                in
                Expect.equal
                    (Ok
                        (Elm.AliasPattern
                            (Elm.CtorPattern "Hello"
                                [ Elm.LowerPattern "world"
                                ]
                            )
                            "h"
                        )
                    )
                    (Parser.run Elm.pattern source)
        ]


expressionTests : Test.Test
expressionTests =
    Test.describe "Expression Tests"
        [ Test.describe "Literals"
            [ Test.test "Char" <|
                \_ ->
                    let
                        source =
                            "'c'"
                    in
                    Expect.equal
                        (Ok (Elm.CharExpression "c"))
                        (Parser.run Elm.expression source)
            , Test.test "String" <|
                \_ ->
                    let
                        source =
                            "\"hello\""
                    in
                    Expect.equal
                        (Ok (Elm.StringExpression "hello"))
                        (Parser.run Elm.expression source)
            , Test.test "Int" <|
                \_ ->
                    let
                        source =
                            "12"
                    in
                    Expect.equal
                        (Ok (Elm.IntExpression 12))
                        (Parser.run Elm.expression source)
            , Test.test "Float" <|
                \_ ->
                    let
                        source =
                            "1.2"
                    in
                    Expect.equal
                        (Ok (Elm.FloatExpression 1.2))
                        (Parser.run Elm.expression source)
            ]
        , Test.test "Variable" <|
            \_ ->
                let
                    source =
                        "world"
                in
                Expect.equal
                    (Ok (Elm.VarExpression "world"))
                    (Parser.run Elm.expression source)
        , Test.test "Record" <|
            \_ ->
                let
                    source =
                        "{ hello = world }"
                in
                Expect.equal
                    (Ok (Elm.RecordExpression [ ( "hello", Elm.VarExpression "world" ) ]))
                    (Parser.run Elm.expression source)
        , Test.test "Record no spacing" <|
            \_ ->
                let
                    source =
                        "{hello=world}"
                in
                Expect.equal
                    (Ok (Elm.RecordExpression [ ( "hello", Elm.VarExpression "world" ) ]))
                    (Parser.run Elm.expression source)
        , Test.test "Record empty" <|
            \_ ->
                let
                    source =
                        "{}"
                in
                Expect.equal
                    (Ok (Elm.RecordExpression []))
                    (Parser.run Elm.expression source)
        , Test.test "List empty" <|
            \_ ->
                let
                    source =
                        "[]"
                in
                Expect.equal
                    (Ok (Elm.ListExpression []))
                    (Parser.run Elm.expression source)
        , Test.test "List mixed" <|
            \_ ->
                let
                    source =
                        "['c', 12, 1.2]"
                in
                Expect.equal
                    (Ok
                        (Elm.ListExpression
                            [ Elm.CharExpression "c"
                            , Elm.IntExpression 12
                            , Elm.FloatExpression 1.2
                            ]
                        )
                    )
                    (Parser.run Elm.expression source)
        , Test.test "unit" <|
            \_ ->
                let
                    source =
                        "()"
                in
                Expect.equal (Ok Elm.UnitExpression)
                    (Parser.run Elm.expression source)
        , Test.test "tuple" <|
            \_ ->
                let
                    source =
                        "(1, 2)"
                in
                Expect.equal
                    (Ok
                        (Elm.TupleExpression
                            (Elm.IntExpression 1)
                            (Elm.IntExpression 2)
                        )
                    )
                    (Parser.run Elm.expression source)
        , Test.test "triple" <|
            \_ ->
                let
                    source =
                        "(1, 2, 3)"
                in
                Expect.equal
                    (Ok
                        (Elm.TripleExpression
                            (Elm.IntExpression 1)
                            (Elm.IntExpression 2)
                            (Elm.IntExpression 3)
                        )
                    )
                    (Parser.run Elm.expression source)
        , Test.test "negated (invalid)" <|
            \_ ->
                let
                    source =
                        "!'c'"
                in
                Expect.equal
                    (Ok (Elm.NegateExpression (Elm.CharExpression "c")))
                    (Parser.run Elm.expression source)
        , Test.test "lambda" <|
            \_ ->
                let
                    source =
                        "\\hello world -> world"
                in
                Expect.equal
                    (Ok
                        (Elm.LambdaExpression
                            (Elm.LowerPattern "hello")
                            [ Elm.LowerPattern "world" ]
                            (Elm.VarExpression "world")
                        )
                    )
                    (Parser.run Elm.expression source)
        , Test.test "access" <|
            \_ ->
                let
                    source =
                        "hello.world"
                in
                Expect.equal
                    (Ok
                        (Elm.AccessExpression
                            (Elm.VarExpression "hello")
                            "world"
                        )
                    )
                    (Parser.run Elm.expression source)
        , Test.test "accessor" <|
            \_ ->
                let
                    source =
                        ".world"
                in
                Expect.equal
                    (Ok (Elm.AccessorExpression "world"))
                    (Parser.run Elm.expression source)
        , Test.test "let" <|
            \_ ->
                let
                    source =
                        """
                        let
                            hello =
                                 "world"

                            my i =
                                 1234

                            (a, b) =
                                 ("hello", "world")
                        in
                        hello
                        """
                in
                Expect.equal
                    (Ok
                        (Elm.LetExpression
                            [ Elm.ValueDeclaration "hello"
                                (Elm.StringExpression "world")
                            , Elm.FunctionDeclaration "my"
                                (Elm.LowerPattern "i")
                                []
                                (Elm.IntExpression 1234)
                            , Elm.ValuePatternMatchDeclaration
                                (Elm.TuplePattern (Elm.LowerPattern "a") (Elm.LowerPattern "b"))
                                (Elm.TupleExpression
                                    (Elm.StringExpression "hello")
                                    (Elm.StringExpression "world")
                                )
                            ]
                            (Elm.VarExpression "hello")
                        )
                    )
                    (Parser.run
                        (Parser.succeed identity
                            |. Parser.spaces
                            |= Elm.expression
                        )
                        source
                    )
        , Test.test "case" <|
            \_ ->
                let
                    source =
                        """
                        case var of
                            [hello, world] ->
                                 "world"

                            var ->
                                'c'

                            _ ->
                                 1234
                        """
                in
                Expect.equal
                    (Ok
                        (Elm.CaseExpression
                            (Elm.LowerPattern "var")
                            [ ( Elm.ListPattern
                                    [ Elm.LowerPattern "hello"
                                    , Elm.LowerPattern "world"
                                    ]
                              , Elm.StringExpression "world"
                              )
                            , ( Elm.LowerPattern "var", Elm.CharExpression "c" )
                            , ( Elm.AnythingPattern, Elm.IntExpression 1234 )
                            ]
                        )
                    )
                    (Parser.run
                        (Parser.succeed identity
                            |. Parser.spaces
                            |= Elm.expression
                        )
                        source
                    )
        , Test.test "call" <|
            \_ ->
                let
                    source =
                        """hello world ya bitch"""
                in
                Expect.equal
                    (Ok
                        (Elm.CallExpression (Elm.VarExpression "hello")
                            (Elm.VarExpression "world")
                            [ Elm.VarExpression "ya"
                            , Elm.VarExpression "bitch"
                            ]
                        )
                    )
                    (Parser.run
                        (Parser.succeed identity
                            |. Parser.spaces
                            |= Elm.expression
                        )
                        source
                    )
        , Test.test "call multiline" <|
            \_ ->
                let
                    source =
                        """
                        hello world
                            ya
                            bitch
                        """
                in
                Expect.equal
                    (Ok
                        (Elm.CallExpression (Elm.VarExpression "hello")
                            (Elm.VarExpression "world")
                            [ Elm.VarExpression "ya"
                            , Elm.VarExpression "bitch"
                            ]
                        )
                    )
                    (Parser.run
                        (Parser.succeed identity
                            |. Parser.spaces
                            |= Elm.expression
                        )
                        source
                    )
        , Test.test "binop call" <|
            \_ ->
                let
                    source =
                        """hello++ world"""
                in
                Expect.equal
                    (Ok
                        (Elm.BinOpCallExpression
                            (Elm.VarExpression "hello")
                            Elm.PlusPlus
                            (Elm.VarExpression "world")
                        )
                    )
                    (Parser.run
                        (Parser.succeed identity
                            |. Parser.spaces
                            |= Elm.expression
                        )
                        source
                    )
        , Test.test "if" <|
            \_ ->
                let
                    source =
                        """
                        if 1 > 2 then
                            "hello"
                        else
                            "world"

                        """
                in
                Expect.equal
                    (Ok
                        (Elm.IfExpression
                            (Elm.BinOpCallExpression
                                (Elm.IntExpression 1)
                                Elm.GreaterThan
                                (Elm.IntExpression 2)
                            )
                            (Elm.StringExpression "hello")
                            (Elm.StringExpression "world")
                        )
                    )
                    (Parser.run
                        (Parser.succeed identity
                            |. Parser.spaces
                            |= Elm.expression
                        )
                        source
                    )
        , Test.test "else if" <|
            \_ ->
                let
                    source =
                        """
                        if 1 > 2 then
                            "hello"
                        else if 2 > 3 then
                            "world"
                        else
                            "yas"

                        """
                in
                Expect.equal
                    (Ok
                        (Elm.IfExpression
                            (Elm.BinOpCallExpression
                                (Elm.IntExpression 1)
                                Elm.GreaterThan
                                (Elm.IntExpression 2)
                            )
                            (Elm.StringExpression "hello")
                            (Elm.IfExpression
                                (Elm.BinOpCallExpression
                                    (Elm.IntExpression 2)
                                    Elm.GreaterThan
                                    (Elm.IntExpression 3)
                                )
                                (Elm.StringExpression "world")
                                (Elm.StringExpression "yas")
                            )
                        )
                    )
                    (Parser.run
                        (Parser.succeed identity
                            |. Parser.spaces
                            |= Elm.expression
                        )
                        source
                    )
        , Test.test "qualified var" <|
            \_ ->
                let
                    source =
                        """Hello.World.hi"""
                in
                Expect.equal
                    (Ok (Elm.QualVarExpression (Elm.ModuleName "Hello" [ "World" ]) "hi"))
                    (Parser.run
                        (Parser.succeed identity
                            |. Parser.spaces
                            |= Elm.expression
                        )
                        source
                    )
        ]


declarationTests : Test.Test
declarationTests =
    Test.describe "Declaration Tests"
        [ Test.test "Value" <|
            \_ ->
                let
                    source =
                        "value = "
                in
                Expect.equal
                    (Ok (Elm.ValueDeclaration "value" Elm.ExpressionStub))
                    (Parser.run Elm.declaration source)
        , Test.test "Function" <|
            \_ ->
                let
                    source =
                        "value arg = "
                in
                Expect.equal
                    (Ok
                        (Elm.FunctionDeclaration
                            "value"
                            (Elm.LowerPattern "arg")
                            []
                            Elm.ExpressionStub
                        )
                    )
                    (Parser.run Elm.declaration source)
        , Test.test "Function with many args" <|
            \_ ->
                let
                    source =
                        "value arg _ = "
                in
                Expect.equal
                    (Ok
                        (Elm.FunctionDeclaration
                            "value"
                            (Elm.LowerPattern "arg")
                            [ Elm.AnythingPattern ]
                            Elm.ExpressionStub
                        )
                    )
                    (Parser.run Elm.declaration source)
        , Test.test "Function with many pattern matched" <|
            \_ ->
                let
                    source =
                        "value arg (World w) = "
                in
                Expect.equal
                    (Ok
                        (Elm.FunctionDeclaration "value"
                            (Elm.LowerPattern "arg")
                            [ Elm.CtorPattern "World" [ Elm.LowerPattern "w" ]
                            ]
                            Elm.ExpressionStub
                        )
                    )
                    (Parser.run Elm.declaration source)
        , Test.test "Function with many pattern matched with alias" <|
            \_ ->
                let
                    source =
                        "value arg ((World w) as abc) = "
                in
                Expect.equal
                    (Ok
                        (Elm.FunctionDeclaration "value"
                            (Elm.LowerPattern "arg")
                            [ Elm.AliasPattern (Elm.CtorPattern "World" [ Elm.LowerPattern "w" ]) "abc" ]
                            Elm.ExpressionStub
                        )
                    )
                    (Parser.run Elm.declaration source)
        , Test.test "Function with many pattern matched with alias top level" <|
            -- TODO: Should this even pass?
            \_ ->
                let
                    source =
                        "value arg (World w) as abc = "
                in
                Expect.equal
                    (Ok
                        (Elm.FunctionDeclaration "value"
                            (Elm.LowerPattern "arg")
                            [ Elm.AliasPattern (Elm.CtorPattern "World" [ Elm.LowerPattern "w" ]) "abc" ]
                            Elm.ExpressionStub
                        )
                    )
                    (Parser.run Elm.declaration source)
        ]
