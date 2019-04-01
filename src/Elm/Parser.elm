module Elm.Parser exposing
    ( ExposedCustomTypeConstructors(..)
    , ExposedItem(..)
    , ExposingList(..)
    , Identifier(..)
    , ModuleDeclaration(..)
    , ModuleName(..)
    , Operator(..)
    , Trailing(..)
    , exposedItem
    , lowercaseIdentifier
    , moduleDeclaration
    , moduleName
    , operator
    , uppercaseIdentifier
    )

import Parser exposing ((|.), (|=), Parser)



-- Module Declaration --


type ModuleDeclaration
    = ModuleDeclaration ModuleName


moduleDeclaration : Parser ModuleDeclaration
moduleDeclaration =
    Parser.succeed ModuleDeclaration
        |. Parser.spaces
        |. Parser.oneOf
            [ chompStringInsensitive "module"
            , Parser.succeed ()
                |. chompStringInsensitive "port"
                |. Parser.spaces
                |. chompStringInsensitive "module"
            ]
        |. Parser.spaces
        |= moduleName



-- Exposing List --


type ExposingList
    = ExposingList (List ExposedItem)
    | ExposingListDoubleDot (List ExposedItem)


type ExposedItem
    = ExposedValue Identifier
    | ExposedType Identifier ExposedCustomTypeConstructors
    | ExposedOperator Operator


type ExposedCustomTypeConstructors
    = ExposedConstructors (List Identifier) Trailing
    | ExposedConstructorsDotDot
    | NoExposedConstructors


exposedItem : Parser ExposedItem
exposedItem =
    Parser.oneOf
        [ lowercaseIdentifier |> Parser.map ExposedValue
        , Parser.succeed (\ident constructors -> ExposedType ident constructors)
            |= uppercaseIdentifier
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. chompChar '('
                    |= Parser.oneOf
                        [ Parser.succeed ExposedConstructorsDotDot
                            |. Parser.token ".."
                        , sequence
                            { subParser = uppercaseIdentifier
                            , separator = ','
                            , allowSpaces = True
                            }
                            |> Parser.map
                                (\( constructors, trailing ) ->
                                    ExposedConstructors constructors trailing
                                )
                        ]
                    |. chompChar ')'
                , Parser.succeed NoExposedConstructors
                ]
        , Parser.succeed ExposedOperator
            |. chompChar '('
            |= operator
            |. chompChar ')'
        ]


type Operator
    = PlusPlus
    | Plus
    | Minus
    | Multiply
    | DivideFloat
    | DivideInt
    | LeftPipe
    | RightPipe
    | ParseKeep
    | ParseIgnore
    | GreaterThan
    | LessThan


operator : Parser Operator
operator =
    Parser.oneOf
        [ Parser.backtrackable (chompString "++" |> Parser.map (\() -> PlusPlus))
        , chompString "+" |> Parser.map (\() -> Plus)
        , chompString "-" |> Parser.map (\() -> Minus)
        , chompString "*" |> Parser.map (\() -> Multiply)
        , Parser.backtrackable (chompString "//" |> Parser.map (\() -> DivideInt))
        , chompString "/" |> Parser.map (\() -> DivideFloat)
        , Parser.backtrackable (chompString "|>" |> Parser.map (\() -> RightPipe))
        , Parser.backtrackable (chompString "<|" |> Parser.map (\() -> LeftPipe))
        , Parser.backtrackable (chompString "|=" |> Parser.map (\() -> ParseKeep))
        , chompString "|." |> Parser.map (\() -> ParseIgnore)
        , chompString ">=" |> Parser.map (\() -> GreaterThan)
        , chompString "<=" |> Parser.map (\() -> LessThan)
        ]



-- Module Name --


type ModuleName
    = ModuleName Identifier (List Identifier) Trailing


type Trailing
    = Trailing
    | NotTrailing


moduleName : Parser ModuleName
moduleName =
    sequenceAtLeastOne
        { subParser = identifier
        , separator = '.'
        , allowSpaces = False
        }
        |> Parser.map (\( head, rest, trailing ) -> ModuleName head rest trailing)



-- Tokens --


type Identifier
    = LowercaseIdentifier String
    | UppercaseIdentifier String


identifier : Parser Identifier
identifier =
    Parser.oneOf [ uppercaseIdentifier, lowercaseIdentifier ]


lowercaseIdentifier : Parser Identifier
lowercaseIdentifier =
    Parser.succeed ()
        |. Parser.chompIf Char.isLower
        |. Parser.chompWhile identifierHelp
        |> Parser.getChompedString
        |> Parser.map LowercaseIdentifier


uppercaseIdentifier : Parser Identifier
uppercaseIdentifier =
    Parser.succeed ()
        |. Parser.chompIf Char.isUpper
        |. Parser.chompWhile identifierHelp
        |> Parser.getChompedString
        |> Parser.map UppercaseIdentifier


identifierHelp : Char -> Bool
identifierHelp c =
    Char.isLower c || Char.isUpper c || Char.isDigit c



-- Parser Extra --


chompString : String -> Parser ()
chompString string =
    Parser.loop (String.toList string) chompStringHelp


chompStringHelp : List Char -> Parser (Parser.Step (List Char) ())
chompStringHelp listOfChars =
    case listOfChars of
        head :: rest ->
            chompChar head |> Parser.map (\() -> Parser.Loop rest)

        _ ->
            Parser.succeed (Parser.Done ())


chompStringInsensitive : String -> Parser ()
chompStringInsensitive string =
    Parser.loop (String.toList string) chompUpperOrLowerHelp


chompUpperOrLowerHelp : List Char -> Parser (Parser.Step (List Char) ())
chompUpperOrLowerHelp listOfChars =
    case listOfChars of
        head :: rest ->
            Parser.succeed (Parser.Loop rest)
                |. Parser.oneOf
                    [ chompChar (Char.toUpper head)
                    , chompChar (Char.toLower head)
                    ]

        _ ->
            Parser.succeed (Parser.Done ())


chompChar : Char -> Parser ()
chompChar char =
    Parser.chompIf (\c -> c == char)


withDefault : item -> Parser item -> Parser item
withDefault default subParser =
    Parser.oneOf [ subParser, Parser.succeed default ]


type alias SequenceConfig item =
    { subParser : Parser item
    , separator : Char
    , allowSpaces : Bool
    }


sequence : SequenceConfig item -> Parser ( List item, Trailing )
sequence config =
    Parser.loop ( [], NotTrailing ) (sequenceHelp config)


sequenceHelp : SequenceConfig item -> ( List item, Trailing ) -> Parser (Parser.Step ( List item, Trailing ) ( List item, Trailing ))
sequenceHelp { subParser, separator, allowSpaces } ( items, trailing ) =
    let
        chompSpacesIfAllowed =
            if allowSpaces then
                Parser.spaces

            else
                Parser.succeed ()
    in
    Parser.oneOf
        [ Parser.succeed (\nextItem nextTrailing -> Parser.Loop ( nextItem :: items, nextTrailing ))
            |= subParser
            |. chompSpacesIfAllowed
            |= Parser.oneOf
                [ chompChar separator |> Parser.map (\() -> Trailing)
                , Parser.succeed NotTrailing
                ]
            |. chompSpacesIfAllowed
        , Parser.succeed (Parser.Done ( List.reverse items, trailing ))
        ]


sequenceAtLeastOne : SequenceConfig item -> Parser ( item, List item, Trailing )
sequenceAtLeastOne config =
    Parser.succeed (\head ( rest, trailing ) -> ( head, rest, trailing ))
        |= config.subParser
        |= Parser.loop [] (sequenceAtLeastOneHelp config)


sequenceAtLeastOneHelp : SequenceConfig item -> List item -> Parser (Parser.Step (List item) ( List item, Trailing ))
sequenceAtLeastOneHelp { subParser, separator, allowSpaces } items =
    let
        chompSpacesIfAllowed =
            if allowSpaces then
                Parser.spaces

            else
                Parser.succeed ()
    in
    Parser.oneOf
        [ Parser.succeed identity
            |. chompSpacesIfAllowed
            |. chompChar separator
            |. chompSpacesIfAllowed
            |= Parser.oneOf
                [ subParser |> Parser.map (\nextItem -> Parser.Loop (nextItem :: items))
                , Parser.succeed (Parser.Done ( List.reverse items, Trailing ))
                ]
        , Parser.succeed (Parser.Done ( List.reverse items, NotTrailing ))
        ]
