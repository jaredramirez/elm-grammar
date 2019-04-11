module Elm.Parser exposing
    ( Alias(..)
    , Elm(..)
    , ExposedCustomTypeConstructors
    , ExposedCustomTypeConstructors_(..)
    , ExposedItem
    , ExposedItem_(..)
    , ExposingList(..)
    , Identifier(..)
    , Located
    , ModuleDeclaration(..)
    , ModuleImport(..)
    , ModuleName
    , ModuleName_(..)
    , Operator
    , Operator_(..)
    , Trailing(..)
    , elm
    , exposedItem
    , exposingList
    , lowercaseIdentifier
    , moduleDeclaration
    , moduleImport
    , moduleName
    , operator
    , uppercaseIdentifier
    )

import Parser exposing ((|.), (|=), Parser)


type Trailing
    = Trailing
    | TrailingInTheMiddle
    | NotTrailing



-- Elm Source --


type Elm
    = Elm (Maybe ModuleDeclaration) (List ModuleImport)


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
                |= sequence
                    { subParser = moduleImport
                    , separator = ' '
                    , allowSpaces = True
                    }
                |. Parser.spaces
                |> Parser.map Tuple.first
            , Parser.succeed []
            ]



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



-- Module Declaration --


type ModuleDeclaration
    = ModuleDeclaration ModuleName ExposingList
    | ModuleDeclarationPartial ModuleName


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
                [ chompStringInsensitive "module"
                , Parser.succeed ()
                    |. chompStringInsensitive "port"
                    |. Parser.spaces
                    |. chompStringInsensitive "module"
                ]
            |. Parser.spaces
            |= moduleName
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. chompStringInsensitive "exposing"
                    |. Parser.spaces
                    |= Parser.oneOf
                        [ exposingList |> Parser.map Just
                        , Parser.succeed Nothing
                        ]
                , Parser.succeed Nothing
                ]
        ]



-- Module Imports --


type ModuleImport
    = ModuleImport ModuleName Alias (Maybe ExposingList)
    | ModuleImportIncomplete


type Alias
    = AliasPartial
    | Alias Identifier
    | AliasNone


moduleImport : Parser ModuleImport
moduleImport =
    Parser.succeed identity
        |. Parser.spaces
        |. chompStringInsensitive "import"
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed ModuleImport
                |= moduleName
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed identity
                        |. chompStringInsensitive "as"
                        |. Parser.spaces
                        |= Parser.oneOf
                            [ identifier |> Parser.map Alias
                            , Parser.succeed AliasPartial
                            ]
                    , Parser.succeed AliasNone
                    ]
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed identity
                        |. chompStringInsensitive "exposing"
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


type ExposingList
    = ExposingList (List ExposedItem) Trailing
    | ExposingListDoubleDot


exposingList : Parser ExposingList
exposingList =
    Parser.oneOf
        [ Parser.succeed
            (\result parenTrailing ->
                case ( result, parenTrailing ) of
                    ( ExposingList exposed NotTrailing, Trailing ) ->
                        ExposingList exposed parenTrailing

                    _ ->
                        result
            )
            |. chompChar '('
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed ExposingListDoubleDot
                    |. Parser.token ".."
                , sequence
                    { subParser = exposedItem
                    , separator = ','
                    , allowSpaces = True
                    }
                    |> Parser.map
                        (\( exposed, trailing ) ->
                            ExposingList exposed trailing
                        )
                ]
            |= Parser.oneOf
                [ chompChar ')' |> Parser.map (\() -> NotTrailing)
                , Parser.succeed Trailing
                ]
        ]


type alias ExposedItem =
    Located ExposedItem_


type ExposedItem_
    = ExposedValue Identifier
    | ExposedType Identifier ExposedCustomTypeConstructors
    | ExposedOperator Operator


type alias ExposedCustomTypeConstructors =
    Located ExposedCustomTypeConstructors_


type ExposedCustomTypeConstructors_
    = ExposedConstructors (List Identifier) Trailing
    | ExposedConstructorsDotDot
    | NoExposedConstructors


exposedItem : Parser ExposedItem
exposedItem =
    Parser.oneOf
        [ lowercaseIdentifier |> Parser.map ExposedValue
        , Parser.succeed (\ident constructors -> ExposedType ident constructors)
            |= uppercaseIdentifier
            |= (Parser.oneOf
                    [ Parser.succeed
                        (\result parenTrailing ->
                            case ( result, parenTrailing ) of
                                ( ExposedConstructors exposed NotTrailing, Trailing ) ->
                                    ExposedConstructors exposed parenTrailing

                                _ ->
                                    result
                        )
                        |. chompChar '('
                        |. Parser.spaces
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
                        |= Parser.oneOf
                            [ chompChar ')' |> Parser.map (\() -> NotTrailing)
                            , Parser.succeed Trailing
                            ]
                    , Parser.succeed NoExposedConstructors
                    ]
                    |> located
               )
        , Parser.succeed ExposedOperator
            |. chompChar '('
            |= operator
            |. chompChar ')'
        ]
        |> located



-- Operators --


type alias Operator =
    Located Operator_


type Operator_
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
        |> located



-- Module Name --


type alias ModuleName =
    Located ModuleName_


type ModuleName_
    = ModuleName_ Identifier (List Identifier) Trailing


moduleName : Parser ModuleName
moduleName =
    sequenceAtLeastOne
        { subParser = identifier
        , separator = '.'
        , allowSpaces = False
        }
        |> Parser.map (\( head, rest, trailing ) -> ModuleName_ head rest trailing)
        |> located



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


chompUntilEndOfLine : Parser ()
chompUntilEndOfLine =
    Parser.chompWhile (\c -> c /= '\n')


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
                , Parser.succeed
                    (case trailing of
                        Trailing ->
                            NotTrailing

                        _ ->
                            trailing
                    )
                ]
            |. chompSpacesIfAllowed
        , Parser.succeed (Parser.Loop ( items, TrailingInTheMiddle ))
            |. chompChar separator
            |. chompSpacesIfAllowed
        , Parser.succeed (Parser.Done ( List.reverse items, trailing ))
        ]


{-| Differs from sequence in it's implementation. Does not support TrailingInTheMiddle
-}
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
