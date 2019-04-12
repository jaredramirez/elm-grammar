module Elm.Parser exposing
    ( elm
    , exposedItem
    , exposingList
    , lowercaseIdentifier
    , moduleDeclaration
    , moduleImport
    , moduleName
    , operator
    , uppercaseIdentifier
    )

import Elm.AST exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra as PExtra



-- Elm Source --


elm : Parser Elm
elm =
    Parser.succeed Elm
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed identity
                |= moduleDeclaration
                |. Parser.spaces
                |> Parser.map Just
            , Parser.succeed Nothing
            ]
        |= Parser.oneOf
            [ Parser.succeed identity
                |= PExtra.sequence
                    { subParser = moduleImport
                    , separator = PExtra.spacesAtLeastOne
                    , spaces = Parser.succeed ()
                    }
            , Parser.succeed []
            ]



-- Module Declaration --


moduleDeclaration : Parser ModuleDeclaration
moduleDeclaration =
    Parser.oneOf
        [ Parser.succeed
            (\name maybeExposingList ->
                case maybeExposingList of
                    Nothing ->
                        ModuleDeclarationPartial name

                    Just exposedList ->
                        ModuleDeclaration name exposedList
            )
            |. Parser.spaces
            |. Parser.oneOf
                [ PExtra.chompStringInsensitive "module"
                , Parser.succeed ()
                    |. PExtra.chompStringInsensitive "port"
                    |. Parser.spaces
                    |. PExtra.chompStringInsensitive "module"
                ]
            |. Parser.spaces
            |= moduleName
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. PExtra.chompStringInsensitive "exposing"
                    |. Parser.spaces
                    |= Parser.oneOf
                        [ exposingList |> Parser.map Just
                        , Parser.succeed Nothing
                        ]
                , Parser.succeed Nothing
                ]
        ]



-- Module Imports --


moduleImport : Parser ModuleImport
moduleImport =
    Parser.succeed identity
        |. Parser.spaces
        |. PExtra.chompStringInsensitive "import"
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed ModuleImport
                |= moduleName
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed identity
                        |. PExtra.chompStringInsensitive "as"
                        |. Parser.spaces
                        |= Parser.oneOf
                            [ uppercaseIdentifier |> Parser.map Alias
                            , Parser.succeed AliasPartial
                            ]
                    , Parser.succeed AliasNone
                    ]
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed identity
                        |. PExtra.chompStringInsensitive "exposing"
                        |. Parser.spaces
                        |= Parser.oneOf
                            [ exposingList |> Parser.map Just
                            , Parser.succeed Nothing
                            ]
                    , Parser.succeed Nothing
                    ]
            , Parser.succeed ModuleImportIncomplete
            ]



-- Exposing List --


exposingList : Parser ExposingList
exposingList =
    Parser.oneOf
        [ Parser.succeed identity
            |. PExtra.chompChar '('
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed ExposingListDoubleDot
                    |. Parser.token ".."
                , PExtra.sequence
                    { subParser = exposedItem
                    , separator = PExtra.chompChar ','
                    , spaces = Parser.spaces
                    }
                    |> Parser.map ExposingList
                ]
            |. Parser.oneOf
                [ PExtra.chompChar ')'
                , Parser.succeed ()
                ]
        ]


exposedItem : Parser ExposedItem
exposedItem =
    Parser.oneOf
        [ lowercaseIdentifier |> Parser.map ExposedValue
        , Parser.succeed (\ident constructors -> ExposedType ident constructors)
            |= uppercaseIdentifier
            |= exposedConstructors
        , Parser.succeed ExposedOperator
            |. PExtra.chompChar '('
            |= operator
            |. PExtra.chompChar ')'
        ]


exposedConstructors : Parser ExposedCustomTypeConstructors
exposedConstructors =
    Parser.oneOf
        [ Parser.succeed identity
            |. PExtra.chompChar '('
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed ExposedConstructorsDotDot
                    |. Parser.token ".."
                , PExtra.sequence
                    { subParser = uppercaseIdentifier
                    , separator = PExtra.chompChar ','
                    , spaces = Parser.spaces
                    }
                    |> Parser.map ExposedConstructors
                ]
            |. Parser.oneOf
                [ PExtra.chompChar ')'
                , Parser.succeed ()
                ]
        , Parser.succeed NoExposedConstructors
        ]



-- Operators --


operator : Parser Operator
operator =
    Parser.oneOf
        [ Parser.backtrackable (PExtra.chompString "++" |> Parser.map (\() -> PlusPlus))
        , PExtra.chompString "+" |> Parser.map (\() -> Plus)
        , PExtra.chompString "-" |> Parser.map (\() -> Minus)
        , PExtra.chompString "*" |> Parser.map (\() -> Multiply)
        , Parser.backtrackable (PExtra.chompString "//" |> Parser.map (\() -> DivideInt))
        , PExtra.chompString "/" |> Parser.map (\() -> DivideFloat)
        , Parser.backtrackable (PExtra.chompString "|>" |> Parser.map (\() -> RightPipe))
        , Parser.backtrackable (PExtra.chompString "<|" |> Parser.map (\() -> LeftPipe))
        , Parser.backtrackable (PExtra.chompString "|=" |> Parser.map (\() -> ParseKeep))
        , PExtra.chompString "|." |> Parser.map (\() -> ParseIgnore)
        , PExtra.chompString ">=" |> Parser.map (\() -> GreaterThan)
        , PExtra.chompString "<=" |> Parser.map (\() -> LessThan)
        ]



-- Module Name --


moduleName : Parser ModuleName
moduleName =
    PExtra.sequenceAtLeastOne
        { subParser = uppercaseIdentifier
        , separator = PExtra.chompChar '.'
        , spaces = Parser.succeed ()
        }
        |> Parser.map (\( head, rest ) -> ModuleName head rest)



-- Declaration


declaration : Parser Declaration
declaration =
    Parser.oneOf
        [ functionDeclaration
        , typeAliasDeclaration
        , Parser.succeed InfixDeclaration
        ]


functionDeclaration : Parser Declaration
functionDeclaration =
    Parser.succeed ValueDeclaration
        |= lowercaseIdentifier
        |. Parser.spaces
        |. PExtra.chompChar '='
        |. Parser.spaces
        |= expression


{-| TODO
-}
typeAliasDeclaration : Parser Declaration
typeAliasDeclaration =
    Parser.succeed TypeAliasDeclaration
        |. PExtra.chompString "type"
        |. Parser.spaces
        |. PExtra.chompString "alias"
        |. Parser.spaces
        |= uppercaseIdentifier
        |. Parser.spaces
        |. PExtra.chompChar '='
        |. Parser.spaces



-- Expression


expression : Parser Expression
expression =
    Parser.succeed Expression



-- Pattern


pattern : Parser Pattern
pattern =
    Parser.succeed Pattern
        |= pattern_
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.spaces
                |. PExtra.chompStringInsensitive "as"
                |. Parser.spaces
                |= lowercaseIdentifier
            , Parser.succeed Nothing
            ]


pattern_ : Parser Pattern_
pattern_ =
    Parser.oneOf
        [ Parser.succeed AnythingPattern
            |. PExtra.chompChar '_'
        , Parser.succeed LowerPattern
            |= lowercaseIdentifier
        , Parser.succeed TuplePattern
            |. PExtra.chompChar '('
            |. Parser.spaces
            |= Parser.lazy (\() -> pattern)
            |. Parser.spaces
            |. PExtra.chompChar ','
            |. Parser.spaces
            |= Parser.lazy (\() -> pattern)
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed Just
                    |. PExtra.chompChar ','
                    |. Parser.spaces
                    |= Parser.lazy (\() -> pattern)
                    |. Parser.spaces
                , Parser.succeed Nothing
                ]
            |. PExtra.chompChar ')'
        , Parser.succeed UnitPattern
            |. PExtra.chompString "()"
        , Parser.succeed RecordPattern
            |. PExtra.chompChar '{'
            |. Parser.spaces
            |= PExtra.sequence
                { subParser = lowercaseIdentifier
                , separator = PExtra.chompChar ','
                , spaces = Parser.spaces
                }
            |. PExtra.chompChar '}'
        , Parser.succeed ParenthesisPattern
            |= Parser.lazy (\() -> pattern)
        , Parser.succeed ListPattern
            |. PExtra.chompChar '['
            |. Parser.spaces
            |= PExtra.sequence
                { subParser = Parser.lazy (\() -> pattern)
                , separator = PExtra.chompChar ','
                , spaces = Parser.spaces
                }
            |. PExtra.chompChar ']'
        , Parser.succeed ConsPattern
            |= Parser.lazy (\() -> pattern)
            |. Parser.spaces
            |. PExtra.chompString "::"
            |. Parser.spaces
            |= Parser.lazy (\() -> pattern)
        , Parser.succeed ()
            |. PExtra.chompChar '\''
            |. Parser.chompIf (\_ -> True)
            |. PExtra.chompChar '\''
            |> Parser.getChompedString
            |> Parser.map CharPattern
        , Parser.succeed ()
            |. PExtra.chompChar '"'
            |. Parser.chompUntil "\""
            |> Parser.getChompedString
            |> Parser.map StringPattern
        , Parser.number
            { int = Just IntPattern
            , hex = Just IntPattern
            , octal = Nothing
            , binary = Nothing
            , float = Just FloatPattern
            }
        , Parser.succeed CtorPattern
            |= uppercaseIdentifier
            |= PExtra.sequence
                { subParser = Parser.lazy (\() -> pattern)
                , separator = Parser.succeed ()
                , spaces = PExtra.spacesAtLeastOne
                }
        ]



-- Tokens --


lowercaseIdentifier : Parser LowercaseIdentifier
lowercaseIdentifier =
    Parser.succeed ()
        |. Parser.chompIf Char.isLower
        |. Parser.chompWhile identifierHelp
        |> Parser.getChompedString
        |> Parser.map LowercaseIdentifier


uppercaseIdentifier : Parser UppercaseIdentifier
uppercaseIdentifier =
    Parser.succeed ()
        |. Parser.chompIf Char.isUpper
        |. Parser.chompWhile identifierHelp
        |> Parser.getChompedString
        |> Parser.map UppercaseIdentifier


identifierHelp : Char -> Bool
identifierHelp c =
    Char.isLower c || Char.isUpper c || Char.isDigit c



-- Located --


type alias Located a =
    { start : ( Int, Int )
    , value : a
    , end : ( Int, Int )
    }


located : Parser a -> Parser (Located a)
located parser =
    Parser.succeed Located
        |= Parser.getPosition
        |= parser
        |= Parser.getPosition
