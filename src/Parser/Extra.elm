module Parser.Extra exposing
    ( SequenceConfig
    , WasTrailing(..)
    , sequence
    , sequenceAtLeastOne
    , sequenceWithOptionalTrailing
    , sequenceWithTrailingLegacy
    , sequenceWithTrailingLegacyHelp
    , spacesAtLeastOne
    )

import Parser.Advanced as Parser exposing (Parser, i, k)


spacesAtLeastOne : p -> Parser c p ()
spacesAtLeastOne problem =
    Parser.succeed ()
        |> i (Parser.chompIf (\c -> c == ' ' || c == '\n' || c == '\u{000D}') problem)
        |> i Parser.spaces


{-| Just like Parser.sequence except separator is `Parser c x ()` instead of `Parser.Token c x ()`
-}
sequence :
    { start : Parser.Token x
    , separator : Parser c x ()
    , end : Parser.Token x
    , spaces : Parser c x ()
    , item : Parser c x a
    , trailing : Parser.Trailing
    }
    -> Parser c x (List a)
sequence config =
    Parser.succeed identity
        |> i (Parser.token config.start)
        |> i config.spaces
        |> k (sequenceEnd (Parser.token config.end) config.spaces config.item config.separator config.trailing)


sequenceEnd :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> Parser.Trailing
    -> Parser c x (List a)
sequenceEnd ender ws parseItem sep trailing =
    let
        chompRest item =
            case trailing of
                Parser.Forbidden ->
                    Parser.loop [ item ] (sequenceEndForbidden ender ws parseItem sep)

                Parser.Optional ->
                    Parser.loop [ item ] (sequenceEndOptional ender ws parseItem sep)
                        |> Parser.map (\( list, _ ) -> list)

                Parser.Mandatory ->
                    ignorer
                        (Parser.succeed identity
                            |> i ws
                            |> i sep
                            |> i ws
                            |> k (Parser.loop [ item ] (sequenceEndMandatory ws parseItem sep))
                        )
                        ender
    in
    Parser.oneOf
        [ parseItem |> Parser.andThen chompRest
        , ender |> Parser.map (\_ -> [])
        ]


{-| Just like Parser.sequence except separator is `Parser c x ()` instead of `Parser.Token c x ()`,
`trailing` is always optinal, and it tells you if the parser _actually was_ trailing.
-}
type WasTrailing
    = WasTrailing
    | WasNotTrailing


sequenceWithOptionalTrailing :
    { start : Parser.Token x
    , separator : Parser c x ()
    , end : Parser.Token x
    , spaces : Parser c x ()
    , item : Parser c x a
    }
    -> Parser c x ( List a, WasTrailing )
sequenceWithOptionalTrailing config =
    Parser.succeed identity
        |> i (Parser.token config.start)
        |> i config.spaces
        |> k (sequenceEndWithOptionalTrailing (Parser.token config.end) config.spaces config.item config.separator)


sequenceEndWithOptionalTrailing :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> Parser c x ( List a, WasTrailing )
sequenceEndWithOptionalTrailing ender ws parseItem sep =
    let
        chompRest item =
            Parser.loop [ item ] (sequenceEndOptional ender ws parseItem sep)
    in
    Parser.oneOf
        [ parseItem |> Parser.andThen chompRest
        , ender |> Parser.map (\_ -> ( [], WasNotTrailing ))
        ]


sequenceEndForbidden :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> List a
    -> Parser c x (Parser.Step (List a) (List a))
sequenceEndForbidden ender ws parseItem sep revItems =
    Parser.succeed identity
        |> i ws
        |> k
            (Parser.oneOf
                [ Parser.succeed identity
                    |> i sep
                    |> i ws
                    |> k (Parser.map (\item -> Parser.Loop (item :: revItems)) parseItem)
                , ender |> Parser.map (\_ -> Parser.Done (List.reverse revItems))
                ]
            )


sequenceEndOptional :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> List a
    -> Parser c x (Parser.Step (List a) ( List a, WasTrailing ))
sequenceEndOptional ender ws parseItem sep revItems =
    let
        parseEnd wasTrailing =
            Parser.map (\_ -> Parser.Done ( List.reverse revItems, wasTrailing )) ender
    in
    Parser.succeed identity
        |> i ws
        |> k
            (Parser.oneOf
                [ Parser.succeed identity
                    |> i sep
                    |> i ws
                    |> k
                        (Parser.oneOf
                            [ parseItem |> Parser.map (\item -> Parser.Loop (item :: revItems))
                            , parseEnd WasTrailing
                            ]
                        )
                , parseEnd WasNotTrailing
                ]
            )


sequenceEndMandatory :
    Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> List a
    -> Parser c x (Parser.Step (List a) (List a))
sequenceEndMandatory ws parseItem sep revItems =
    Parser.oneOf
        [ Parser.map (\item -> Parser.Loop (item :: revItems)) <|
            ignorer parseItem (ignorer ws (ignorer sep ws))
        , Parser.map (\_ -> Parser.Done (List.reverse revItems)) (Parser.succeed ())
        ]


ignorer : Parser c x keep -> Parser c x ignore -> Parser c x keep
ignorer keepParser ignoreParser =
    keepParser |> i ignoreParser



-- Legacy custom sequences


type TrailingExtra
    = Trailing
    | TrailingInTheMiddle
    | NotTrailing


type alias SequenceConfig item c p =
    { subParser : Parser c p item
    , separator : Parser c p ()
    , spaces : Parser c p ()
    }


sequenceWithTrailingLegacy : SequenceConfig item c p -> Parser c p (List item)
sequenceWithTrailingLegacy config =
    Parser.loop ( [], NotTrailing ) (sequenceWithTrailingLegacyHelp config)
        |> Parser.map Tuple.first


sequenceWithTrailingLegacyHelp :
    SequenceConfig item c p
    -> ( List item, TrailingExtra )
    -> Parser c p (Parser.Step ( List item, TrailingExtra ) ( List item, TrailingExtra ))
sequenceWithTrailingLegacyHelp { subParser, separator, spaces } ( items, trailing ) =
    Parser.oneOf
        [ Parser.succeed (\nextItem nextTrailing -> Parser.Loop ( nextItem :: items, nextTrailing ))
            |> k subParser
            |> i spaces
            |> k
                (Parser.oneOf
                    [ separator |> Parser.map (\() -> Trailing)
                    , Parser.succeed
                        (case trailing of
                            Trailing ->
                                NotTrailing

                            _ ->
                                trailing
                        )
                    ]
                )
            |> i spaces
        , Parser.succeed (Parser.Loop ( items, TrailingInTheMiddle ))
            |> i separator
            |> i spaces
        , Parser.succeed (Parser.Done ( List.reverse items, trailing ))
        ]


sequenceAtLeastOne : SequenceConfig item c p -> Parser c p ( item, List item )
sequenceAtLeastOne config =
    Parser.succeed (\head ( rest, trailing ) -> ( head, rest, trailing ))
        |> k config.subParser
        |> k (Parser.loop [] (sequenceAtLeastOneHelp config))
        |> Parser.map (\( head, rest, _ ) -> ( head, rest ))


sequenceAtLeastOneHelp : SequenceConfig item c p -> List item -> Parser c p (Parser.Step (List item) ( List item, TrailingExtra ))
sequenceAtLeastOneHelp { subParser, separator, spaces } items =
    Parser.oneOf
        [ Parser.succeed identity
            |> i spaces
            |> i separator
            |> i spaces
            |> k
                (Parser.oneOf
                    [ subParser |> Parser.map (\nextItem -> Parser.Loop (nextItem :: items))
                    , Parser.succeed (Parser.Done ( List.reverse items, Trailing ))
                    ]
                )
        , Parser.succeed (Parser.Done ( List.reverse items, NotTrailing ))
        ]
