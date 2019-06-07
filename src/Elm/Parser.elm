module Elm.Parser exposing
    ( declaration
    , elm
    , exposedItem
    , exposingList
    , expression
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
        , valuePatternMatchDeclaration

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


valuePatternMatchDeclaration : Parser Declaration
valuePatternMatchDeclaration =
    Parser.succeed ValuePatternMatchDeclaration
        |= pattern
        |. Parser.spaces
        |. PExtra.chompChar '='
        |. Parser.spaces
        -- TODO: Expression Parser
        |= Parser.succeed ExpressionStub



-- Pattern


pattern : Parser Pattern
pattern =
    Parser.succeed (\pat transform -> transform pat)
        |= Parser.oneOf
            [ Parser.succeed AnythingPattern
                |. PExtra.chompChar '_'
            , Parser.succeed RecordPattern
                |. PExtra.chompChar '{'
                |. Parser.spaces
                |= PExtra.sequence
                    { subParser = lowercaseIdentifier
                    , separator = PExtra.chompChar ','
                    , spaces = Parser.spaces
                    }
                |. PExtra.chompChar '}'
            , Parser.succeed CtorPattern
                |= uppercaseIdentifier
                |= PExtra.sequence
                    { subParser = Parser.lazy (\() -> pattern)
                    , separator = PExtra.spacesAtLeastOne
                    , spaces = Parser.succeed ()
                    }
            , charLiteral
                |> Parser.map CharPattern
            , stringLiteral
                |> Parser.map StringPattern
            , variableLiteral |> Parser.map LowerPattern
            , numberLiteral IntPattern FloatPattern
            , listLiteral (Parser.lazy (\() -> pattern))
                |> Parser.map ListPattern
            , unitTupleTripleLiteral (Parser.lazy (\() -> pattern))
                UnitPattern
                TuplePattern
                TriplePattern
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



-- Expression


expression : Parser Expression



-- TODO: Call expression


expression =
    Parser.succeed (\exp transform -> transform exp)
        |= Parser.oneOf
            [ Parser.succeed NegateExpression
                |. PExtra.chompChar '!'
                |= Parser.lazy (\() -> expression)
            , Parser.succeed
                (\( firstPattern, restPatterns ) exp ->
                    LambdaExpression firstPattern restPatterns exp
                )
                |. PExtra.chompChar '\\'
                |= PExtra.sequenceAtLeastOne
                    { subParser = pattern
                    , separator = PExtra.spacesAtLeastOne
                    , spaces = Parser.succeed ()
                    }
                |. Parser.spaces
                |. PExtra.chompString "->"
                |. Parser.spaces
                |= Parser.lazy (\() -> expression)
            , Parser.succeed identity
                |. PExtra.chompChar '{'
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.map RecordExpression
                        (PExtra.sequence
                            { subParser = Parser.lazy (\() -> recordKeyValue)
                            , separator = PExtra.chompChar ','
                            , spaces = Parser.spaces
                            }
                        )
                    , Parser.succeed
                        (\var ( firstProperty, restProperties ) ->
                            UpdateExpression var
                                firstProperty
                                restProperties
                        )
                        |= lowercaseIdentifier
                        |. Parser.spaces
                        |. PExtra.chompChar '|'
                        |. Parser.spaces
                        |= PExtra.sequenceAtLeastOne
                            { subParser = Parser.lazy (\() -> recordKeyValue)
                            , separator = PExtra.spacesAtLeastOne
                            , spaces = Parser.succeed ()
                            }
                        |. Parser.spaces
                    ]
                |. PExtra.chompChar '}'
            , Parser.succeed AccessorExpression
                |. PExtra.chompChar '.'
                |= lowercaseIdentifier
            , Parser.succeed LetExpression
                |. Parser.keyword letString
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed (\value transform -> transform value)
                        |= lowercaseIdentifier
                        |. Parser.spaces
                        |= Parser.oneOf
                            [ Parser.succeed (\exp -> \name -> ValueDeclaration name exp)
                                |. PExtra.chompChar '='
                                |. Parser.spaces
                                |= Parser.lazy (\() -> expression)
                            ]
                    ]
            , charLiteral
                |> Parser.map CharExpression
            , stringLiteral
                |> Parser.map StringExpression
            , variableLiteral |> Parser.map VarExpression
            , listLiteral (Parser.lazy (\() -> expression))
                |> Parser.map ListExpression
            , unitTupleTripleLiteral (Parser.lazy (\() -> expression))
                UnitExpression
                TupleExpression
                TripleExpression
            , numberLiteral IntExpression FloatExpression
            ]
        |= Parser.oneOf
            [ Parser.succeed (\prop -> \exp -> AccessExpression exp prop)
                |. PExtra.chompChar '.'
                |= lowercaseIdentifier
            , Parser.succeed identity
            ]


recordKeyValue : Parser ( LowercaseIdentifier, Expression )
recordKeyValue =
    Parser.succeed Tuple.pair
        |= lowercaseIdentifier
        |. Parser.spaces
        |. PExtra.chompChar '='
        |. Parser.spaces
        |= expression
        |. Parser.spaces



-- Literals --


variableLiteral : Parser String
variableLiteral =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = keywordsAsSet
        }


{-| TODO: Handle escapes
-}
charLiteral : Parser String
charLiteral =
    Parser.succeed ()
        |. PExtra.chompChar '\''
        |. Parser.chompIf (\_ -> True)
        |. PExtra.chompChar '\''
        |> Parser.getChompedString
        |> Parser.map (String.dropLeft 1 >> String.dropRight 1)


{-| TODO: Handle escapes
-}
stringLiteral : Parser String
stringLiteral =
    Parser.succeed ()
        |. PExtra.chompChar '"'
        |. Parser.chompUntil "\""
        |> Parser.getChompedString
        |> Parser.map (String.dropLeft 1)


numberLiteral : (Int -> decodesTo) -> (Float -> decodesTo) -> Parser decodesTo
numberLiteral fromInt fromFloat =
    Parser.number
        { int = Just fromInt
        , hex = Just fromInt
        , octal = Nothing
        , binary = Nothing
        , float = Just fromFloat
        }


unitTupleTripleLiteral :
    Parser decodesTo
    -> decodesTo
    -> (decodesTo -> decodesTo -> decodesTo)
    -> (decodesTo -> decodesTo -> decodesTo -> decodesTo)
    -> Parser decodesTo
unitTupleTripleLiteral subParser fromUnit fromTuple fromTriple =
    Parser.succeed identity
        |. PExtra.chompChar '('
        |= Parser.oneOf
            [ Parser.succeed fromUnit
                |. PExtra.chompChar ')'
            , Parser.succeed (\sub fromSub -> fromSub sub)
                |. Parser.spaces
                |= subParser
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.map (\() -> identity)
                        (PExtra.chompChar ')')
                    , Parser.succeed
                        (\second maybeThird ->
                            \first ->
                                case maybeThird of
                                    Nothing ->
                                        fromTuple first second

                                    Just third ->
                                        fromTriple first second third
                        )
                        |. PExtra.chompChar ','
                        |. Parser.spaces
                        |= subParser
                        |. Parser.spaces
                        |= Parser.oneOf
                            [ Parser.succeed Just
                                |. PExtra.chompChar ','
                                |. Parser.spaces
                                |= subParser
                                |. Parser.spaces
                            , Parser.succeed Nothing
                            ]
                        |. PExtra.chompChar ')'
                    ]
            ]


listLiteral : Parser decodesTo -> Parser (List decodesTo)
listLiteral subParser =
    Parser.succeed identity
        |. PExtra.chompChar '['
        |. Parser.spaces
        |= Parser.succeed identity
        |= PExtra.sequence
            { subParser = subParser
            , separator = PExtra.chompChar ','
            , spaces = Parser.spaces
            }
        |. Parser.spaces
        |. PExtra.chompChar ']'



-- Tokens --


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


letString : String
letString =
    "let"


inString : String
inString =
    "in"


keywords : List String
keywords =
    [ letString
    , inString
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
