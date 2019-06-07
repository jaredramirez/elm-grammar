module Parser.Extra exposing
    ( SequenceConfig
    , chompChar
    , chompUntilEndOfLine
    , sequence
    , sequenceAtLeastOne
    , sequenceAtLeastOneHelp
    , sequenceHelp
    , spacesAtLeastOne
    , withDefault
    )

import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


type Trailing
    = Trailing
    | TrailingInTheMiddle
    | NotTrailing


chompChar : Char -> p -> Parser c p ()
chompChar char problem =
    Parser.chompIf (\c -> c == char) problem


chompUntilEndOfLine : Parser c p ()
chompUntilEndOfLine =
    Parser.chompWhile (\c -> c /= '\n')


withDefault : item -> Parser c p item -> Parser c p item
withDefault default subParser =
    Parser.oneOf [ subParser, Parser.succeed default ]


spacesAtLeastOne : p -> Parser c p ()
spacesAtLeastOne problem =
    Parser.succeed ()
        |. Parser.chompIf (\c -> c == ' ' || c == '\n' || c == '\u{000D}') problem
        |. Parser.spaces


type alias SequenceConfig item c p =
    { subParser : Parser c p item
    , separator : Parser c p ()
    , spaces : Parser c p ()
    }


sequence : SequenceConfig item c p -> Parser c p (List item)
sequence config =
    Parser.loop ( [], NotTrailing ) (sequenceHelp config)
        |> Parser.map Tuple.first


sequenceHelp :
    SequenceConfig item c p
    -> ( List item, Trailing )
    -> Parser c p (Parser.Step ( List item, Trailing ) ( List item, Trailing ))
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
sequenceAtLeastOne : SequenceConfig item c p -> Parser c p ( item, List item )
sequenceAtLeastOne config =
    Parser.succeed (\head ( rest, trailing ) -> ( head, rest, trailing ))
        |= config.subParser
        |= Parser.loop [] (sequenceAtLeastOneHelp config)
        |> Parser.map (\( head, rest, _ ) -> ( head, rest ))


sequenceAtLeastOneHelp : SequenceConfig item c p -> List item -> Parser c p (Parser.Step (List item) ( List item, Trailing ))
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
