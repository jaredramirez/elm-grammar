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
                    (Ok <|
                        Elm.ExposingList
                            [ Elm.ExposedOperator
                                (Elm.Located
                                    ( 1, 3 )
                                    Elm.PlusPlus
                                    ( 1, 5 )
                                )
                            ]
                            Elm.NotTrailing
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
                        Elm.ExposingList
                            [ Elm.ExposedOperator
                                (Elm.Located
                                    ( 1, 3 )
                                    Elm.PlusPlus
                                    ( 1, 5 )
                                )
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
                            [ Elm.ExposedOperator
                                (Elm.Located
                                    ( 1, 3 )
                                    Elm.PlusPlus
                                    ( 1, 5 )
                                )
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
                            [ Elm.ExposedOperator
                                (Elm.Located
                                    ( 1, 3 )
                                    Elm.PlusPlus
                                    ( 1, 5 )
                                )
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
                            [ Elm.ExposedOperator
                                (Elm.Located
                                    ( 1, 3 )
                                    Elm.PlusPlus
                                    ( 1, 5 )
                                )
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
                        Elm.ExposingList
                            [ Elm.ExposedOperator
                                (Elm.Located
                                    ( 1, 3 )
                                    Elm.PlusPlus
                                    ( 1, 5 )
                                )
                            ]
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
                        Elm.ExposingList
                            [ Elm.ExposedOperator
                                (Elm.Located
                                    ( 1, 3 )
                                    Elm.PlusPlus
                                    ( 1, 5 )
                                )
                            ]
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
                            [ Elm.ExposedOperator
                                (Elm.Located
                                    ( 1, 4 )
                                    Elm.PlusPlus
                                    ( 1, 6 )
                                )
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
                            [ Elm.ExposedOperator
                                (Elm.Located
                                    ( 1, 4 )
                                    Elm.PlusPlus
                                    ( 1, 6 )
                                )
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
                Expect.equal
                    (Ok <|
                        Elm.ExposedOperator
                            (Elm.Located
                                ( 1, 2 )
                                Elm.PlusPlus
                                ( 1, 4 )
                            )
                    )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator
                                    (Elm.Located
                                        ( 1, 1 )
                                        Elm.RightPipe
                                        ( 1, 3 )
                                    )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator
                                    (Elm.Located
                                        ( 1, 1 )
                                        Elm.RightPipe
                                        ( 1, 3 )
                                    )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator
                                    (Elm.Located
                                        ( 1, 1 )
                                        Elm.RightPipe
                                        ( 1, 3 )
                                    )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator
                                    (Elm.Located
                                        ( 1, 1 )
                                        Elm.RightPipe
                                        ( 1, 3 )
                                    )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
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
                            (Elm.Located
                                ( 0, 0 )
                                (Elm.ModuleName_
                                    (Elm.UppercaseIdentifier "Hello")
                                    []
                                    Elm.NotTrailing
                                )
                                ( 0, 0 )
                            )
                            (Elm.ExposingList
                                [ Elm.ExposedValue (Elm.LowercaseIdentifier "value")
                                , Elm.ExposedType (Elm.UppercaseIdentifier "String")
                                    (Elm.ExposedConstructors [ Elm.UppercaseIdentifier "Str" ] Elm.NotTrailing)
                                , Elm.ExposedType (Elm.UppercaseIdentifier "Int") Elm.ExposedConstructorsDotDot
                                , Elm.ExposedOperator
                                    (Elm.Located
                                        ( 1, 1 )
                                        Elm.RightPipe
                                        ( 1, 3 )
                                    )
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
                                Elm.ExposingList
                                    [ Elm.ExposedValue (Elm.LowercaseIdentifier "world") ]
                                    Elm.NotTrailing
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
                                Elm.ExposingList [ Elm.ExposedValue (Elm.LowercaseIdentifier "world") ] Elm.NotTrailing
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
                            (Just <| Elm.ExposingList [] Elm.Trailing)
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
                                Elm.ExposingList
                                    [ Elm.ExposedValue (Elm.LowercaseIdentifier "world")
                                    , Elm.ExposedValue (Elm.LowercaseIdentifier "myGoodness")
                                    ]
                                    Elm.TrailingInTheMiddle
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
                                    (Elm.Located
                                        ( 2, 32 )
                                        (Elm.ModuleName_
                                            (Elm.UppercaseIdentifier "Ya")
                                            []
                                            Elm.NotTrailing
                                        )
                                        ( 2, 34 )
                                    )
                                    (Elm.ExposingList [] Elm.NotTrailing)
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
                                    (Elm.ExposingList
                                        [ Elm.ExposedValue (Elm.LowercaseIdentifier "myValue")
                                        ]
                                        Elm.Trailing
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
                                    Elm.ExposingList
                                        [ Elm.ExposedType (Elm.UppercaseIdentifier "World")
                                            Elm.ExposedConstructorsDotDot
                                        , Elm.ExposedValue (Elm.LowercaseIdentifier "world")
                                        ]
                                        Elm.NotTrailing
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
                                    Elm.ExposingList
                                        [ Elm.ExposedOperator
                                            (Elm.Located
                                                ( 9, 55 )
                                                Elm.ParseIgnore
                                                ( 9, 57 )
                                            )
                                        ]
                                        Elm.NotTrailing
                                )
                            ]
                        )
                    )
                    (Parser.run Elm.elm source)
        ]
