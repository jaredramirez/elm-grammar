module Elm.Parser exposing
    ( declaration
    , elm
    , exposedItem
    , exposingList
    , lowercaseIdentifier
    , moduleDeclaration
    , moduleImport
    , moduleName
    , operator
    , pattern
    , uppercaseIdentifier
    )

import Elm.AST exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra as PExtra
import Set



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
                [ Parser.keyword moduleString
                , Parser.succeed ()
                    |. Parser.keyword portString
                    |. Parser.spaces
                    |. Parser.keyword moduleString
                ]
            |. Parser.spaces
            |= moduleName
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.keyword exposingString
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
        |. Parser.keyword importString
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed ModuleImport
                |= moduleName
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed identity
                        |. PExtra.chompStringInsensitive asString
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
                        |. Parser.keyword exposingString
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
                    |. Parser.token consString
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
        [ valueOrFunctionDeclaration

        -- TODO: Rest of declaration types
        ]


valueOrFunctionDeclaration : Parser Declaration
valueOrFunctionDeclaration =
    Parser.succeed (\name toDeclaration exp -> toDeclaration name exp)
        |= lowercaseIdentifier
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed
                (\( firstPattern, restPatterns ) ->
                    \name exp ->
                        FunctionDeclaration name
                            firstPattern
                            restPatterns
                            exp
                )
                |= PExtra.sequenceAtLeastOne
                    { subParser = pattern
                    , separator = PExtra.spacesAtLeastOne
                    , spaces = Parser.succeed ()
                    }
            , Parser.succeed (\name exp -> ValueDeclaration name exp)
            ]
        |. PExtra.chompChar '='
        |. Parser.spaces
        -- TODO: Expression Parser
        |= Parser.succeed ExpressionStub



-- Expression


expression : Parser Expression
expression =
    Parser.oneOf
        []



-- Pattern


pattern : Parser Pattern
pattern =
    Parser.succeed (\pat transform -> transform pat)
        |= Parser.oneOf
            [ Parser.succeed AnythingPattern
                |. PExtra.chompChar '_'
            , Parser.map LowerPattern
                (Parser.variable
                    { start = Char.isLower
                    , inner = \c -> Char.isAlphaNum c || c == '_'
                    , reserved = keywordsAsSet
                    }
                )
            , Parser.succeed identity
                |. PExtra.chompChar '('
                |= Parser.oneOf
                    [ Parser.succeed UnitPattern
                        |. PExtra.chompChar ')'
                    , Parser.succeed (\pat fromPattern -> fromPattern pat)
                        |. Parser.spaces
                        |= Parser.lazy (\() -> pattern)
                        |. Parser.spaces
                        |= Parser.oneOf
                            [ Parser.map (\() -> identity)
                                (PExtra.chompChar ')')
                            , Parser.succeed
                                (\second maybeThird ->
                                    \first ->
                                        case maybeThird of
                                            Nothing ->
                                                TuplePattern first second

                                            Just third ->
                                                TriplePattern first second third
                                )
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
                            ]
                    ]
            , Parser.succeed RecordPattern
                |. PExtra.chompChar '{'
                |. Parser.spaces
                |= PExtra.sequence
                    { subParser = lowercaseIdentifier
                    , separator = PExtra.chompChar ','
                    , spaces = Parser.spaces
                    }
                |. PExtra.chompChar '}'
            , Parser.succeed ListPattern
                |. PExtra.chompChar '['
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed []
                        |. PExtra.chompChar ']'
                    , Parser.succeed identity
                        |= PExtra.sequence
                            { subParser = Parser.lazy (\() -> pattern)
                            , separator = PExtra.chompChar ','
                            , spaces = Parser.spaces
                            }
                        |. PExtra.chompChar ']'
                    ]
            , Parser.succeed ()
                |. PExtra.chompChar '\''
                |. Parser.chompIf (\_ -> True)
                |. PExtra.chompChar '\''
                |> Parser.getChompedString
                |> Parser.map (String.dropLeft 1 >> String.dropRight 1 >> CharPattern)
            , Parser.succeed ()
                |. PExtra.chompChar '"'
                |. Parser.chompUntil "\""
                |> Parser.getChompedString
                |> Parser.map (String.dropLeft 1 >> StringPattern)
            , Parser.succeed CtorPattern
                |= uppercaseIdentifier
                |= PExtra.sequence
                    { subParser = Parser.lazy (\() -> pattern)
                    , separator = PExtra.spacesAtLeastOne
                    , spaces = Parser.succeed ()
                    }
            , number IntPattern FloatPattern
            ]
        |= Parser.oneOf
            [ Parser.succeed identity
                |. (PExtra.spacesAtLeastOne |> Parser.backtrackable)
                |= Parser.oneOf
                    [ Parser.succeed (\alias_ -> \pat -> AliasPattern pat alias_)
                        |. Parser.keyword asString
                        |. PExtra.spacesAtLeastOne
                        |= lowercaseIdentifier
                    , Parser.succeed (\rest -> \head -> ConsPattern head rest)
                        |. Parser.keyword consString
                        |. PExtra.spacesAtLeastOne
                        |= Parser.lazy (\() -> pattern)
                    ]
            , Parser.succeed identity
            ]



-- Tokens --


number : (Int -> decodesTo) -> (Float -> decodesTo) -> Parser decodesTo
number fromInt fromFloat =
    Parser.number
        { int = Just fromInt
        , hex = Just fromInt
        , octal = Nothing
        , binary = Nothing
        , float = Just fromFloat
        }


lowercaseIdentifier : Parser String
lowercaseIdentifier =
    Parser.succeed ()
        |. Parser.chompIf Char.isLower
        |. Parser.chompWhile identifierHelp
        |> Parser.getChompedString


uppercaseIdentifier : Parser String
uppercaseIdentifier =
    Parser.succeed ()
        |. Parser.chompIf Char.isUpper
        |. Parser.chompWhile identifierHelp
        |> Parser.getChompedString


identifierHelp : Char -> Bool
identifierHelp c =
    Char.isLower c || Char.isUpper c || Char.isDigit c



-- Constants --


consString : String
consString =
    "::"


asString : String
asString =
    "as"


typeString : String
typeString =
    "type"


aliasString : String
aliasString =
    "alias"


moduleString : String
moduleString =
    "module"


exposingString : String
exposingString =
    "exposing"


portString : String
portString =
    "port"


importString : String
importString =
    "import"


keywords : List String
keywords =
    [ "let"
    , "in"
    , "case"
    , "of"
    , asString
    , typeString
    , aliasString
    , moduleString
    , exposingString
    , portString
    , importString
    ]


keywordsAsSet : Set.Set String
keywordsAsSet =
    Set.fromList keywords



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
