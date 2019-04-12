module Parser.Extra exposing
    ( SequenceConfig
    , chompChar
    , chompString
    , chompStringHelp
    , chompStringInsensitive
    , chompUntilEndOfLine
    , chompUpperOrLowerHelp
    , sequence
    , sequenceAtLeastOne
    , sequenceAtLeastOneHelp
    , sequenceHelp
    , spacesAtLeastOne
    , withDefault
    )

import Parser exposing ((|.), (|=), Parser)


type Trailing
    = Trailing
    | TrailingInTheMiddle
    | NotTrailing


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


spacesAtLeastOne : Parser ()
spacesAtLeastOne =
    Parser.succeed ()
        |. Parser.chompIf (\c -> c == ' ' || c == '\n' || c == '\u{000D}')
        |. Parser.spaces


type alias SequenceConfig item =
    { subParser : Parser item
    , separator : Parser ()
    , spaces : Parser ()
    }


sequence : SequenceConfig item -> Parser (List item)
sequence config =
    Parser.loop ( [], NotTrailing ) (sequenceHelp config)
        |> Parser.map Tuple.first


sequenceHelp : SequenceConfig item -> ( List item, Trailing ) -> Parser (Parser.Step ( List item, Trailing ) ( List item, Trailing ))
sequenceHelp { subParser, separator, spaces } ( items, trailing ) =
    Parser.oneOf
        [ Parser.succeed (\nextItem nextTrailing -> Parser.Loop ( nextItem :: items, nextTrailing ))
            |= subParser
            |. spaces
            |= Parser.oneOf
                [ separator |> Parser.map (\() -> Trailing)
                , Parser.succeed
                    (case trailing of
                        Trailing ->
                            NotTrailing

                        _ ->
                            trailing
                    )
                ]
            |. spaces
        , Parser.succeed (Parser.Loop ( items, TrailingInTheMiddle ))
            |. separator
            |. spaces
        , Parser.succeed (Parser.Done ( List.reverse items, trailing ))
        ]


{-| Differs from sequence in it's implementation. Does not support TrailingInTheMiddle
-}
sequenceAtLeastOne : SequenceConfig item -> Parser ( item, List item )
sequenceAtLeastOne config =
    Parser.succeed (\head ( rest, trailing ) -> ( head, rest, trailing ))
        |= config.subParser
        |= Parser.loop [] (sequenceAtLeastOneHelp config)
        |> Parser.map (\( head, rest, _ ) -> ( head, rest ))


sequenceAtLeastOneHelp : SequenceConfig item -> List item -> Parser (Parser.Step (List item) ( List item, Trailing ))
sequenceAtLeastOneHelp { subParser, separator, spaces } items =
    Parser.oneOf
        [ Parser.succeed identity
            |. spaces
            |. separator
            |. spaces
            |= Parser.oneOf
                [ subParser |> Parser.map (\nextItem -> Parser.Loop (nextItem :: items))
                , Parser.succeed (Parser.Done ( List.reverse items, Trailing ))
                ]
        , Parser.succeed (Parser.Done ( List.reverse items, NotTrailing ))
        ]
