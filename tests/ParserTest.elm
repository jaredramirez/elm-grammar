module ParserTest exposing (test)

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
                            Elm.NotTrailing
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
                            Elm.NotTrailing
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
                            Elm.NotTrailing
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
                                (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "World" ]
                                    Elm.NotTrailing
                                )
                            ]
                            Elm.NotTrailing
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing operator" <|
            \_ ->
                let
                    source =
                        "((++))"
                in
                Expect.equal
                    (Ok <| Elm.ExposingList [ Elm.ExposedOperator Elm.PlusPlus ] Elm.NotTrailing)
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
                                    Elm.NotTrailing
                                )
                            ]
                            Elm.NotTrailing
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
                                    Elm.NotTrailing
                                )
                            ]
                            Elm.Trailing
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
                                    Elm.NotTrailing
                                )
                            ]
                            Elm.TrailingInTheMiddle
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
                                    Elm.Trailing
                                )
                            ]
                            Elm.Trailing
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
                            Elm.Trailing
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
                            Elm.Trailing
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
                            Elm.NotTrailing
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
                            Elm.TrailingInTheMiddle
                    )
                    (Parser.run Elm.exposingList source)
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
                Expect.equal (Ok <| Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                    (Parser.run Elm.moduleName source)
        , Test.test "Many ModuleNames" <|
            \_ ->
                let
                    source =
                        "Hello.World.HowYaDoing"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleName (Elm.UppercaseIdentifier "Hello")
                            [ Elm.UppercaseIdentifier "World", Elm.UppercaseIdentifier "HowYaDoing" ]
                            Elm.NotTrailing
                    )
                    (Parser.run Elm.moduleName source)
        , Test.test "Single ModuleName Lowercase" <|
            \_ ->
                let
                    source =
                        "hello"
                in
                Expect.equal (Ok <| Elm.ModuleName (Elm.LowercaseIdentifier "hello") [] Elm.NotTrailing)
                    (Parser.run Elm.moduleName source)
        , Test.test "Many ModuleNames Lowercase" <|
            \_ ->
                let
                    source =
                        "hello.World.howYaDoing"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleName (Elm.LowercaseIdentifier "hello")
                            [ Elm.UppercaseIdentifier "World", Elm.LowercaseIdentifier "howYaDoing" ]
                            Elm.NotTrailing
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
                        Elm.ModuleName (Elm.UppercaseIdentifier "Hello")
                            []
                            Elm.Trailing
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
                            Elm.Trailing
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
                            (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
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
                            (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.Trailing)
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
                                Elm.Trailing
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
                                Elm.Trailing
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello")
                                []
                                Elm.NotTrailing
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello")
                                []
                                Elm.NotTrailing
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.ExposingList [] Elm.Trailing)
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.ExposingList [] Elm.NotTrailing)
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator Elm.RightPipe
                                ]
                                Elm.NotTrailing
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator Elm.RightPipe
                                ]
                                Elm.NotTrailing
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                ]
                                Elm.Trailing
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator Elm.RightPipe
                                ]
                                Elm.Trailing
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                ]
                                Elm.Trailing
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator Elm.RightPipe
                                ]
                                Elm.Trailing
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
        , Test.test "Module declaration with exposing something with no closing parenthesis multiline" <|
            \_ ->
                let
                    source =
                        "module Hello exposing (value, String(Str), "
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclaration
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                ]
                                Elm.Trailing
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator Elm.RightPipe
                                ]
                                Elm.Trailing
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
                        Elm.ModuleImportPartial
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            Elm.AliasNone
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
                        Elm.ModuleImportPartial
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            Elm.AliasPartial
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
                        Elm.ModuleImportPartial
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.Alias (Elm.UppercaseIdentifier "H"))
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            Elm.AliasNone
                            (Elm.ExposingList [ Elm.ExposedValue (Elm.LowercaseIdentifier "world") ] Elm.NotTrailing)
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.Alias (Elm.UppercaseIdentifier "H"))
                            (Elm.ExposingList [ Elm.ExposedValue (Elm.LowercaseIdentifier "world") ] Elm.NotTrailing)
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
                            (Elm.ModuleName (Elm.UppercaseIdentifier "Hello") [] Elm.NotTrailing)
                            (Elm.Alias (Elm.UppercaseIdentifier "H"))
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "world")
                                , Elm.ExposedValue (Elm.LowercaseIdentifier "myGoodness")
                                ]
                                Elm.TrailingInTheMiddle
                            )
                    )
                    (Parser.run Elm.moduleImport source)
        ]
