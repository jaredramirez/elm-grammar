module Parser.Extra exposing
    ( SequenceConfig
    , WasTrailing(..)
    , chompChar
    , chompUntilEndOfLine
    , join
    , sequence
    , sequenceAtLeastOne
    , sequenceWithOptionalTrailing
    , sequenceWithTrailingLegacy
    , sequenceWithTrailingLegacyHelp
    , spacesAtLeastOne
    , withDefault
    )

import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


type TrailingExtra
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
sequence i =
    Parser.succeed identity
        |. Parser.token i.start
        |. i.spaces
        |= sequenceEnd (Parser.token i.end) i.spaces i.item i.separator i.trailing


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
                            |. ws
                            |. sep
                            |. ws
                            |= Parser.loop [ item ] (sequenceEndMandatory ws parseItem sep)
                        )
                        ender
    in
    Parser.oneOf
        [ parseItem |> Parser.andThen chompRest
        , ender |> Parser.map (\_ -> [])
        ]


{-| Just like Parser.sequence except separator is `Parser c x ()` instead of `Parser.Token c x ()`
and it tells you if the parser was actually trailing.
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
sequenceWithOptionalTrailing i =
    Parser.succeed identity
        |. Parser.token i.start
        |. i.spaces
        |= sequenceEndWithOptionalTrailing (Parser.token i.end) i.spaces i.item i.separator


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
        |. ws
        |= Parser.oneOf
            [ Parser.succeed identity
                |. sep
                |. ws
                |= Parser.map (\item -> Parser.Loop (item :: revItems)) parseItem
            , ender |> Parser.map (\_ -> Parser.Done (List.reverse revItems))
            ]


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
        |. ws
        |= Parser.oneOf
            [ Parser.succeed identity
                |. sep
                |. ws
                |= Parser.oneOf
                    [ parseItem |> Parser.map (\item -> Parser.Loop (item :: revItems))
                    , parseEnd WasTrailing
                    ]
            , parseEnd WasNotTrailing
            ]


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


keeper : Parser c x (a -> b) -> Parser c x a -> Parser c x b
keeper parseFunc parseArg =
    parseFunc |= parseArg


ignorer : Parser c x keep -> Parser c x ignore -> Parser c x keep
ignorer keepParser ignoreParser =
    keepParser |. ignoreParser


join : Parser c x (Parser c x v) -> Parser c x v
join double =
    Parser.andThen identity double



-- Legacy custom sequences


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


sequenceAtLeastOne : SequenceConfig item c p -> Parser c p ( item, List item )
sequenceAtLeastOne config =
    Parser.succeed (\head ( rest, trailing ) -> ( head, rest, trailing ))
        |= config.subParser
        |= Parser.loop [] (sequenceAtLeastOneHelp config)
        |> Parser.map (\( head, rest, _ ) -> ( head, rest ))


sequenceAtLeastOneHelp : SequenceConfig item c p -> List item -> Parser c p (Parser.Step (List item) ( List item, TrailingExtra ))
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
