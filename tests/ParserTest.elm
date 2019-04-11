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
        , sourceTests
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
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ModuleName_
                                (Elm.UppercaseIdentifier "Hello")
                                []
                                Elm.NotTrailing
                            )
                            ( 1, 6 )
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
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ModuleName_
                                (Elm.UppercaseIdentifier "Hello")
                                [ Elm.UppercaseIdentifier "World"
                                , Elm.UppercaseIdentifier "HowYaDoing"
                                ]
                                Elm.NotTrailing
                            )
                            ( 1, 23 )
                    )
                    (Parser.run Elm.moduleName source)
        , Test.test "Single ModuleName Lowercase" <|
            \_ ->
                let
                    source =
                        "hello"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ModuleName_
                                (Elm.LowercaseIdentifier "hello")
                                []
                                Elm.NotTrailing
                            )
                            ( 1, 6 )
                    )
                    (Parser.run Elm.moduleName source)
        , Test.test "Many ModuleNames Lowercase" <|
            \_ ->
                let
                    source =
                        "hello.World.howYaDoing"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ModuleName_
                                (Elm.LowercaseIdentifier "hello")
                                [ Elm.UppercaseIdentifier "World"
                                , Elm.LowercaseIdentifier "howYaDoing"
                                ]
                                Elm.NotTrailing
                            )
                            ( 1, 23 )
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
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ModuleName_
                                (Elm.UppercaseIdentifier "Hello")
                                []
                                Elm.Trailing
                            )
                            ( 1, 7 )
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
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ModuleName_ (Elm.UppercaseIdentifier "Hello")
                                [ Elm.UppercaseIdentifier "World" ]
                                Elm.Trailing
                            )
                            ( 1, 13 )
                    )
                    (Parser.run Elm.moduleName source)
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
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            Elm.PlusPlus
                            ( 1, 3 )
                    )
                    (Parser.run Elm.operator source)
        , Test.test "+" <|
            \_ ->
                let
                    source =
                        "+"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            Elm.Plus
                            ( 1, 2 )
                    )
                    (Parser.run Elm.operator source)
        , Test.test "-" <|
            \_ ->
                let
                    source =
                        "-"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            Elm.Minus
                            ( 1, 2 )
                    )
                    (Parser.run Elm.operator source)
        , Test.test "*" <|
            \_ ->
                let
                    source =
                        "*"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            Elm.Multiply
                            ( 1, 2 )
                    )
                    (Parser.run Elm.operator source)
        , Test.test "/" <|
            \_ ->
                let
                    source =
                        "/"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            Elm.DivideFloat
                            ( 1, 2 )
                    )
                    (Parser.run Elm.operator source)
        , Test.test "//" <|
            \_ ->
                let
                    source =
                        "//"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            Elm.DivideInt
                            ( 1, 3 )
                    )
                    (Parser.run Elm.operator source)
        , Test.test "|>" <|
            \_ ->
                let
                    source =
                        "|>"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            Elm.RightPipe
                            ( 1, 3 )
                    )
                    (Parser.run Elm.operator source)
        , Test.test "<|" <|
            \_ ->
                let
                    source =
                        "<|"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            Elm.LeftPipe
                            ( 1, 3 )
                    )
                    (Parser.run Elm.operator source)
        , Test.test "|=" <|
            \_ ->
                let
                    source =
                        "|="
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            Elm.ParseKeep
                            ( 1, 3 )
                    )
                    (Parser.run Elm.operator source)
        , Test.test "|." <|
            \_ ->
                let
                    source =
                        "|."
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            Elm.ParseIgnore
                            ( 1, 3 )
                    )
                    (Parser.run Elm.operator source)
        , Test.test ">=" <|
            \_ ->
                let
                    source =
                        ">="
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            Elm.GreaterThan
                            ( 1, 3 )
                    )
                    (Parser.run Elm.operator source)
        , Test.test "<=" <|
            \_ ->
                let
                    source =
                        "<="
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            Elm.LessThan
                            ( 1, 3 )
                    )
                    (Parser.run Elm.operator source)
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
                Expect.equal
                    (Ok <| Elm.Located ( 1, 1 ) (Elm.ExposedValue (Elm.LowercaseIdentifier "hello")) ( 1, 6 ))
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedUnion Opaque" <|
            \_ ->
                let
                    source =
                        "String"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                (Elm.Located
                                    ( 1, 7 )
                                    Elm.NoExposedConstructors
                                    ( 1, 7 )
                                )
                            )
                            ( 1, 7 )
                    )
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedUnion with DoubleDot" <|
            \_ ->
                let
                    source =
                        "String(..)"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                (Elm.Located
                                    ( 1, 7 )
                                    Elm.ExposedConstructorsDotDot
                                    ( 1, 11 )
                                )
                            )
                            ( 1, 11 )
                    )
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedUnion with Constructors" <|
            \_ ->
                let
                    source =
                        "String(Str)"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ExposedType
                                (Elm.UppercaseIdentifier "String")
                                (Elm.Located
                                    ( 1, 7 )
                                    (Elm.ExposedConstructors
                                        [ Elm.UppercaseIdentifier "Str" ]
                                        Elm.NotTrailing
                                    )
                                    ( 1, 12 )
                                )
                            )
                            ( 1, 12 )
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
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                (Elm.Located
                                    ( 1, 7 )
                                    (Elm.ExposedConstructors
                                        [ Elm.UppercaseIdentifier "Str" ]
                                        Elm.Trailing
                                    )
                                    ( 1, 14 )
                                )
                            )
                            ( 1, 14 )
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
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                (Elm.Located
                                    ( 1, 7 )
                                    (Elm.ExposedConstructors
                                        [ Elm.UppercaseIdentifier "Str" ]
                                        Elm.Trailing
                                    )
                                    ( 1, 11 )
                                )
                            )
                            ( 1, 11 )
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
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                (Elm.Located
                                    ( 1, 7 )
                                    (Elm.ExposedConstructors
                                        [ Elm.UppercaseIdentifier "Str" ]
                                        Elm.Trailing
                                    )
                                    ( 1, 12 )
                                )
                            )
                            ( 1, 12 )
                    )
                    (Parser.run Elm.exposedItem source)
        , Test.test "ExposedOperator" <|
            \_ ->
                let
                    source =
                        "(++)"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located
                            ( 1, 1 )
                            (Elm.ExposedOperator
                                (Elm.Located
                                    ( 1, 2 )
                                    Elm.PlusPlus
                                    ( 1, 4 )
                                )
                            )
                            ( 1, 5 )
                    )
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
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 2 ) (Elm.ExposedValue (Elm.LowercaseIdentifier "hello")) ( 1, 7 )
                                ]
                                Elm.NotTrailing
                            )
                            ( 1, 8 )
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
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 2 )
                                    (Elm.ExposedType (Elm.UppercaseIdentifier "Hello")
                                        (Elm.Located
                                            ( 1, 7 )
                                            Elm.NoExposedConstructors
                                            ( 1, 7 )
                                        )
                                    )
                                    ( 1, 7 )
                                ]
                                Elm.NotTrailing
                            )
                            ( 1, 8 )
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
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 2 )
                                    (Elm.ExposedType (Elm.UppercaseIdentifier "Hello")
                                        (Elm.Located
                                            ( 1, 7 )
                                            Elm.ExposedConstructorsDotDot
                                            ( 1, 11 )
                                        )
                                    )
                                    ( 1, 11 )
                                ]
                                Elm.NotTrailing
                            )
                            ( 1, 12 )
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
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 2 )
                                    (Elm.ExposedType (Elm.UppercaseIdentifier "Hello")
                                        (Elm.Located
                                            ( 1, 7 )
                                            (Elm.ExposedConstructors
                                                [ Elm.UppercaseIdentifier "World" ]
                                                Elm.NotTrailing
                                            )
                                            ( 1, 14 )
                                        )
                                    )
                                    ( 1, 14 )
                                ]
                                Elm.NotTrailing
                            )
                            ( 1, 15 )
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing operator" <|
            \_ ->
                let
                    source =
                        "((++))"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 2 )
                                    (Elm.ExposedOperator
                                        (Elm.Located
                                            ( 1, 3 )
                                            Elm.PlusPlus
                                            ( 1, 5 )
                                        )
                                    )
                                    ( 1, 6 )
                                ]
                                Elm.NotTrailing
                            )
                            ( 1, 7 )
                    )
                    (Parser.run Elm.exposingList source)
        , Test.test "Exposing many" <|
            \_ ->
                let
                    source =
                        "((++), myValue, String(Str))"
                in
                Expect.equal
                    (Ok <|
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 2 )
                                    (Elm.ExposedOperator
                                        (Elm.Located
                                            ( 1, 3 )
                                            Elm.PlusPlus
                                            ( 1, 5 )
                                        )
                                    )
                                    ( 1, 6 )
                                , Elm.Located ( 1, 8 )
                                    (Elm.ExposedValue (Elm.LowercaseIdentifier "myValue"))
                                    ( 1, 15 )
                                , Elm.Located ( 1, 17 )
                                    (Elm.ExposedType
                                        (Elm.UppercaseIdentifier "String")
                                        (Elm.Located
                                            ( 1, 23 )
                                            (Elm.ExposedConstructors
                                                [ Elm.UppercaseIdentifier "Str" ]
                                                Elm.NotTrailing
                                            )
                                            ( 1, 28 )
                                        )
                                    )
                                    ( 1, 28 )
                                ]
                                Elm.NotTrailing
                            )
                            ( 1, 29 )
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
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 2 )
                                    (Elm.ExposedOperator
                                        (Elm.Located
                                            ( 1, 3 )
                                            Elm.PlusPlus
                                            ( 1, 5 )
                                        )
                                    )
                                    ( 1, 6 )
                                , Elm.Located ( 1, 8 )
                                    (Elm.ExposedValue (Elm.LowercaseIdentifier "myValue"))
                                    ( 1, 15 )
                                , Elm.Located ( 1, 17 )
                                    (Elm.ExposedType
                                        (Elm.UppercaseIdentifier "String")
                                        (Elm.Located
                                            ( 1, 23 )
                                            (Elm.ExposedConstructors
                                                [ Elm.UppercaseIdentifier "Str" ]
                                                Elm.NotTrailing
                                            )
                                            ( 1, 28 )
                                        )
                                    )
                                    ( 1, 28 )
                                ]
                                Elm.Trailing
                            )
                            ( 1, 30 )
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
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 2 )
                                    (Elm.ExposedOperator
                                        (Elm.Located
                                            ( 1, 3 )
                                            Elm.PlusPlus
                                            ( 1, 5 )
                                        )
                                    )
                                    ( 1, 6 )
                                , Elm.Located ( 1, 8 )
                                    (Elm.ExposedValue (Elm.LowercaseIdentifier "myValue"))
                                    ( 1, 15 )
                                , Elm.Located ( 1, 19 )
                                    (Elm.ExposedType
                                        (Elm.UppercaseIdentifier "String")
                                        (Elm.Located
                                            ( 1, 25 )
                                            (Elm.ExposedConstructors
                                                [ Elm.UppercaseIdentifier "Str" ]
                                                Elm.NotTrailing
                                            )
                                            ( 1, 30 )
                                        )
                                    )
                                    ( 1, 30 )
                                ]
                                Elm.TrailingInTheMiddle
                            )
                            ( 1, 31 )
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
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 2 )
                                    (Elm.ExposedOperator
                                        (Elm.Located
                                            ( 1, 3 )
                                            Elm.PlusPlus
                                            ( 1, 5 )
                                        )
                                    )
                                    ( 1, 6 )
                                , Elm.Located ( 1, 8 )
                                    (Elm.ExposedValue (Elm.LowercaseIdentifier "myValue"))
                                    ( 1, 15 )
                                , Elm.Located ( 1, 17 )
                                    (Elm.ExposedType
                                        (Elm.UppercaseIdentifier "String")
                                        (Elm.Located
                                            ( 1, 23 )
                                            (Elm.ExposedConstructors
                                                [ Elm.UppercaseIdentifier "Str" ]
                                                Elm.Trailing
                                            )
                                            ( 1, 29 )
                                        )
                                    )
                                    ( 1, 29 )
                                ]
                                Elm.Trailing
                            )
                            ( 1, 31 )
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
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 2 )
                                    (Elm.ExposedOperator
                                        (Elm.Located
                                            ( 1, 3 )
                                            Elm.PlusPlus
                                            ( 1, 5 )
                                        )
                                    )
                                    ( 1, 6 )
                                ]
                                Elm.Trailing
                            )
                            ( 1, 6 )
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
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 2 )
                                    (Elm.ExposedOperator
                                        (Elm.Located
                                            ( 1, 3 )
                                            Elm.PlusPlus
                                            ( 1, 5 )
                                        )
                                    )
                                    ( 1, 6 )
                                ]
                                Elm.Trailing
                            )
                            ( 1, 7 )
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
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 3 )
                                    (Elm.ExposedOperator
                                        (Elm.Located
                                            ( 1, 4 )
                                            Elm.PlusPlus
                                            ( 1, 6 )
                                        )
                                    )
                                    ( 1, 7 )
                                , Elm.Located ( 2, 30 )
                                    (Elm.ExposedValue (Elm.LowercaseIdentifier "hello"))
                                    ( 2, 35 )
                                ]
                                Elm.NotTrailing
                            )
                            ( 3, 29 )
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
                        Elm.Located ( 1, 1 )
                            (Elm.ExposingList
                                [ Elm.Located ( 1, 3 )
                                    (Elm.ExposedOperator
                                        (Elm.Located
                                            ( 1, 4 )
                                            Elm.PlusPlus
                                            ( 1, 6 )
                                        )
                                    )
                                    ( 1, 7 )
                                , Elm.Located ( 2, 30 )
                                    (Elm.ExposedValue (Elm.LowercaseIdentifier "hello"))
                                    ( 2, 35 )
                                ]
                                Elm.TrailingInTheMiddle
                            )
                            ( 3, 29 )
                    )
                    (Parser.run Elm.exposingList source)
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
                            (Elm.Located
                                ( 1, 8 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 1, 13 )
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
                            (Elm.Located
                                ( 1, 8 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.Trailing
                                )
                                ( 1, 14 )
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
                            (Elm.Located
                                ( 1, 8 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 1, 13 )
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
                            (Elm.Located
                                ( 1, 8 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 1, 13 )
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
                            (Elm.Located
                                ( 1, 8 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 1, 13 )
                            )
                            Elm.AliasNone
                            (Just <|
                                Elm.Located ( 1, 23 )
                                    (Elm.ExposingList
                                        [ Elm.Located ( 1, 24 )
                                            (Elm.ExposedValue (Elm.LowercaseIdentifier "world"))
                                            ( 1, 29 )
                                        ]
                                        Elm.NotTrailing
                                    )
                                    ( 1, 30 )
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
                            (Elm.Located
                                ( 1, 8 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 1, 13 )
                            )
                            (Elm.Alias (Elm.UppercaseIdentifier "H"))
                            (Just <|
                                Elm.Located ( 1, 28 )
                                    (Elm.ExposingList
                                        [ Elm.Located ( 1, 29 ) (Elm.ExposedValue (Elm.LowercaseIdentifier "world")) ( 1, 34 )
                                        ]
                                        Elm.NotTrailing
                                    )
                                    ( 1, 35 )
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
                            (Elm.Located
                                ( 1, 8 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 1, 13 )
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
                            (Elm.Located
                                ( 1, 8 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 1, 13 )
                            )
                            (Elm.Alias (Elm.UppercaseIdentifier "H"))
                            (Just <| Elm.Located ( 1, 28 ) (Elm.ExposingList [] Elm.Trailing) ( 1, 29 ))
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
                            (Elm.Located
                                ( 1, 8 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 1, 13 )
                            )
                            (Elm.Alias (Elm.UppercaseIdentifier "H"))
                            (Just <|
                                Elm.Located ( 2, 30 )
                                    (Elm.ExposingList
                                        [ Elm.Located ( 2, 32 )
                                            (Elm.ExposedValue (Elm.LowercaseIdentifier "world"))
                                            ( 2, 37 )
                                        , Elm.Located ( 3, 32 )
                                            (Elm.ExposedValue (Elm.LowercaseIdentifier "myGoodness"))
                                            ( 3, 42 )
                                        ]
                                        Elm.TrailingInTheMiddle
                                    )
                                    ( 4, 31 )
                            )
                    )
                    (Parser.run Elm.moduleImport source)
        ]


moduleDeclarationTests : Test.Test
moduleDeclarationTests =
    Test.describe "Module Declaration Tests"
        [ Test.test "Module name declaration" <|
            \_ ->
                let
                    source =
                        "module Hello"
                in
                Expect.equal
                    (Ok <|
                        Elm.ModuleDeclarationPartial
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.Located ( 1, 1 ) (Elm.ExposingList [] Elm.Trailing) ( 1, 1 ))
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.Located ( 1, 1 ) (Elm.ExposingList [] Elm.NotTrailing) ( 1, 1 ))
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
                            (Elm.Located ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.Located ( 1, 1 )
                                (Elm.ExposingList
                                    [ Elm.Located ( 0, 0 )
                                        (Elm.ExposedValue (Elm.LowercaseIdentifier "value"))
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                            (Elm.Located
                                                ( 1, 7 )
                                                (Elm.ExposedConstructors
                                                    [ Elm.UppercaseIdentifier "Str" ]
                                                    Elm.NotTrailing
                                                )
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "Int")
                                            (Elm.Located
                                                ( 1, 7 )
                                                Elm.ExposedConstructorsDotDot
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedOperator
                                            (Elm.Located
                                                ( 1, 1 )
                                                Elm.RightPipe
                                                ( 1, 3 )
                                            )
                                        )
                                        ( 1, 3 )
                                    ]
                                    Elm.NotTrailing
                                )
                                ( 1, 1 )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.Located ( 1, 1 )
                                (Elm.ExposingList
                                    [ Elm.Located ( 0, 0 )
                                        (Elm.ExposedValue (Elm.LowercaseIdentifier "value"))
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                            (Elm.Located
                                                ( 1, 7 )
                                                (Elm.ExposedConstructors
                                                    [ Elm.UppercaseIdentifier "Str" ]
                                                    Elm.NotTrailing
                                                )
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "Int")
                                            (Elm.Located
                                                ( 1, 7 )
                                                Elm.ExposedConstructorsDotDot
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedOperator
                                            (Elm.Located
                                                ( 1, 1 )
                                                Elm.RightPipe
                                                ( 1, 3 )
                                            )
                                        )
                                        ( 1, 3 )
                                    ]
                                    Elm.NotTrailing
                                )
                                ( 1, 1 )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.Located ( 1, 1 )
                                (Elm.ExposingList
                                    [ Elm.Located ( 0, 0 )
                                        (Elm.ExposedValue (Elm.LowercaseIdentifier "value"))
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                            (Elm.Located
                                                ( 1, 7 )
                                                (Elm.ExposedConstructors
                                                    [ Elm.UppercaseIdentifier "Str" ]
                                                    Elm.NotTrailing
                                                )
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    ]
                                    Elm.Trailing
                                )
                                ( 1, 1 )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.Located ( 1, 1 )
                                (Elm.ExposingList
                                    [ Elm.Located ( 0, 0 )
                                        (Elm.ExposedValue (Elm.LowercaseIdentifier "value"))
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                            (Elm.Located
                                                ( 1, 7 )
                                                (Elm.ExposedConstructors
                                                    [ Elm.UppercaseIdentifier "Str" ]
                                                    Elm.NotTrailing
                                                )
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "Int")
                                            (Elm.Located
                                                ( 1, 7 )
                                                Elm.ExposedConstructorsDotDot
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedOperator
                                            (Elm.Located
                                                ( 1, 1 )
                                                Elm.RightPipe
                                                ( 1, 3 )
                                            )
                                        )
                                        ( 1, 3 )
                                    ]
                                    Elm.Trailing
                                )
                                ( 1, 1 )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.Located ( 1, 1 )
                                (Elm.ExposingList
                                    [ Elm.Located ( 0, 0 )
                                        (Elm.ExposedValue (Elm.LowercaseIdentifier "value"))
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                            (Elm.Located
                                                ( 1, 7 )
                                                (Elm.ExposedConstructors
                                                    [ Elm.UppercaseIdentifier "Str" ]
                                                    Elm.NotTrailing
                                                )
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    ]
                                    Elm.Trailing
                                )
                                ( 1, 1 )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.Located ( 1, 1 )
                                (Elm.ExposingList
                                    [ Elm.Located ( 0, 0 )
                                        (Elm.ExposedValue (Elm.LowercaseIdentifier "value"))
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                            (Elm.Located
                                                ( 1, 7 )
                                                (Elm.ExposedConstructors
                                                    [ Elm.UppercaseIdentifier "Str" ]
                                                    Elm.NotTrailing
                                                )
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "Int")
                                            (Elm.Located
                                                ( 1, 7 )
                                                Elm.ExposedConstructorsDotDot
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedOperator
                                            (Elm.Located
                                                ( 1, 1 )
                                                Elm.RightPipe
                                                ( 1, 3 )
                                            )
                                        )
                                        ( 1, 3 )
                                    ]
                                    Elm.Trailing
                                )
                                ( 1, 1 )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.Located ( 1, 1 )
                                (Elm.ExposingList
                                    [ Elm.Located ( 0, 0 )
                                        (Elm.ExposedValue (Elm.LowercaseIdentifier "value"))
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                            (Elm.Located
                                                ( 1, 7 )
                                                (Elm.ExposedConstructors
                                                    [ Elm.UppercaseIdentifier "Str" ]
                                                    Elm.NotTrailing
                                                )
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    ]
                                    Elm.Trailing
                                )
                                ( 1, 1 )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.Located ( 1, 1 )
                                (Elm.ExposingList
                                    [ Elm.Located ( 0, 0 )
                                        (Elm.ExposedValue (Elm.LowercaseIdentifier "value"))
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                            (Elm.Located
                                                ( 1, 7 )
                                                (Elm.ExposedConstructors
                                                    [ Elm.UppercaseIdentifier "Str" ]
                                                    Elm.NotTrailing
                                                )
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedType (Elm.UppercaseIdentifier "Int")
                                            (Elm.Located
                                                ( 1, 7 )
                                                Elm.ExposedConstructorsDotDot
                                                ( 1, 7 )
                                            )
                                        )
                                        ( 1, 3 )
                                    , Elm.Located ( 0, 0 )
                                        (Elm.ExposedOperator
                                            (Elm.Located
                                                ( 1, 1 )
                                                Elm.RightPipe
                                                ( 1, 3 )
                                            )
                                        )
                                        ( 1, 3 )
                                    ]
                                    Elm.Trailing
                                )
                                ( 1, 1 )
                            )
                    )
                    (Parser.run Elm.moduleDeclaration source)
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
                                    (Elm.Located
                                        ( 2, 32 )
                                        (Elm.ModuleName_
                                            (Elm.UppercaseIdentifier "Ya")
                                            []
                                            Elm.NotTrailing
                                        )
                                        ( 2, 34 )
                                    )
                                    (Elm.Located ( 2, 44 ) (Elm.ExposingList [] Elm.NotTrailing) ( 2, 46 ))
                                )
                            )
                            [ Elm.ModuleImport
                                (Elm.Located
                                    ( 4, 32 )
                                    (Elm.ModuleName_
                                        (Elm.UppercaseIdentifier "Hello")
                                        []
                                        Elm.NotTrailing
                                    )
                                    ( 4, 37 )
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
                                    (Elm.Located
                                        ( 2, 32 )
                                        (Elm.ModuleName_
                                            (Elm.UppercaseIdentifier "Ya")
                                            []
                                            Elm.NotTrailing
                                        )
                                        ( 2, 34 )
                                    )
                                    (Elm.Located ( 3, 29 )
                                        (Elm.ExposingList
                                            [ Elm.Located ( 3, 31 )
                                                (Elm.ExposedValue (Elm.LowercaseIdentifier "myValue"))
                                                ( 3, 38 )
                                            ]
                                            Elm.Trailing
                                        )
                                        ( 5, 30 )
                                    )
                                )
                            )
                            [ Elm.ModuleImport
                                (Elm.Located
                                    ( 7, 32 )
                                    (Elm.ModuleName_
                                        (Elm.UppercaseIdentifier "Hello")
                                        []
                                        Elm.NotTrailing
                                    )
                                    ( 7, 37 )
                                )
                                (Elm.Alias (Elm.UppercaseIdentifier "H"))
                                Nothing
                            , Elm.ModuleImport
                                (Elm.Located
                                    ( 8, 32 )
                                    (Elm.ModuleName_
                                        (Elm.UppercaseIdentifier "World")
                                        []
                                        Elm.NotTrailing
                                    )
                                    ( 8, 37 )
                                )
                                Elm.AliasNone
                                (Just <|
                                    Elm.Located ( 8, 47 )
                                        (Elm.ExposingList
                                            [ Elm.Located
                                                ( 8, 48 )
                                                (Elm.ExposedType
                                                    (Elm.UppercaseIdentifier "World")
                                                    (Elm.Located
                                                        ( 8, 53 )
                                                        Elm.ExposedConstructorsDotDot
                                                        ( 8, 57 )
                                                    )
                                                )
                                                ( 8, 57 )
                                            , Elm.Located
                                                ( 8, 59 )
                                                (Elm.ExposedValue (Elm.LowercaseIdentifier "world"))
                                                ( 8, 64 )
                                            ]
                                            Elm.NotTrailing
                                        )
                                        ( 8, 65 )
                                )
                            , Elm.ModuleImport
                                (Elm.Located
                                    ( 9, 32 )
                                    (Elm.ModuleName_
                                        (Elm.UppercaseIdentifier "Parser")
                                        []
                                        Elm.NotTrailing
                                    )
                                    ( 9, 38 )
                                )
                                (Elm.Alias (Elm.UppercaseIdentifier "P"))
                                (Just <|
                                    Elm.Located ( 9, 53 )
                                        (Elm.ExposingList
                                            [ Elm.Located
                                                ( 9, 54 )
                                                (Elm.ExposedOperator
                                                    (Elm.Located
                                                        ( 9, 55 )
                                                        Elm.ParseIgnore
                                                        ( 9, 57 )
                                                    )
                                                )
                                                ( 9, 58 )
                                            ]
                                            Elm.NotTrailing
                                        )
                                        ( 9, 59 )
                                )
                            ]
                        )
                    )
                    (Parser.run Elm.elm source)
        ]
