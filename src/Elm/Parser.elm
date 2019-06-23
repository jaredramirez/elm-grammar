module Elm.Parser exposing
    ( elm
    , exposedItem
    , exposingList
    , expression
    , lowercaseIdentifier
    , moduleDeclaration
    , moduleImport
    , moduleName
    , operator
    , pattern
    , typeDeclaration
    , type_
    , uppercaseIdentifier
    , valueDeclaration
    )

import Elm.AST exposing (..)
import Maybe.Extra as Maybe
import Parser.Advanced as Parser exposing (i, k)
import Parser.Extra as PExtra
import Set


type alias Parser a =
    Parser.Parser Context Problem a



-- Elm Source --


elm : Parser ElmModule
elm =
    Parser.succeed ElmModule
        |> i Parser.spaces
        |> k
            (Parser.oneOf
                [ Parser.succeed identity
                    |> k moduleDeclaration
                    |> i Parser.spaces
                    |> Parser.map Just
                , Parser.succeed Nothing
                ]
            )
        |> k
            (Parser.oneOf
                [ Parser.succeed identity
                    |> k
                        (PExtra.sequence
                            { start = emptyToken
                            , end = emptyToken
                            , item =
                                Parser.oneOf
                                    [ Parser.succeed Just
                                        |> i Parser.spaces
                                        |> k moduleImport
                                    , Parser.succeed Nothing
                                        |> i (Parser.chompUntil newLineToken)
                                        |> i Parser.spaces
                                    ]
                            , separator = atLeastOneSpace
                            , spaces = Parser.succeed ()
                            , trailing = Parser.Forbidden
                            }
                        )
                    |> Parser.map Maybe.values
                , Parser.succeed []
                ]
            )



-- Module Declaration --


moduleDeclaration : Parser ModuleDeclaration
moduleDeclaration =
    Parser.inContext CModuleDeclaration
        (Parser.succeed
            (\name exposedList ->
                ModuleDeclaration name exposedList
            )
            |> i
                (Parser.oneOf
                    [ Parser.keyword moduleToken
                    , Parser.succeed ()
                        |> i (Parser.keyword portToken)
                        |> i Parser.spaces
                        |> i (Parser.keyword moduleToken)
                    ]
                )
            |> i Parser.spaces
            |> k moduleName
            |> i Parser.spaces
            |> k
                (Parser.oneOf
                    [ Parser.succeed identity
                        |> i (Parser.keyword exposingToken)
                        |> i Parser.spaces
                        |> k exposingList
                    , Parser.succeed ExposingNone
                    ]
                )
        )



-- Module Imports --


moduleImport : Parser ModuleImport
moduleImport =
    Parser.succeed identity
        |> i (Parser.keyword importToken)
        |> i atLeastOneSpace
        |> k moduleName
        |> Parser.andThen
            (\importedModuleName ->
                Parser.inContext (CModuleImport importedModuleName) <|
                    Parser.oneOf
                        [ Parser.succeed
                            (\alias_ exposing_ ->
                                ModuleImport
                                    importedModuleName
                                    alias_
                                    exposing_
                            )
                            -- TODO: Remove backtrackable?
                            |> i (Parser.backtrackable atLeastOneSpace)
                            |> k
                                (Parser.oneOf
                                    [ Parser.succeed identity
                                        |> i (Parser.token asToken)
                                        |> i Parser.spaces
                                        |> k
                                            (Parser.oneOf
                                                [ uppercaseIdentifier |> Parser.map Alias
                                                , Parser.succeed AliasPartial
                                                ]
                                            )
                                    , Parser.succeed AliasNone
                                    ]
                                )
                            |> i Parser.spaces
                            |> k
                                (Parser.oneOf
                                    [ Parser.succeed identity
                                        |> i (Parser.keyword exposingToken)
                                        |> i Parser.spaces
                                        |> k exposingList
                                    , Parser.succeed ExposingNone
                                    ]
                                )
                        , Parser.succeed (ModuleImport importedModuleName AliasNone ExposingNone)
                        ]
            )



-- Exposing List --


type Item a
    = Item a
    | InvalidItem
    | FilterItemOut


exposingList : Parser ExposingList
exposingList =
    Parser.inContext CExposingList <|
        Parser.oneOf
            [ Parser.succeed identity
                |> i (Parser.symbol openParenthesisToken)
                |> k
                    (Parser.oneOf
                        [ Parser.succeed identity
                            |> i Parser.spaces
                            |> k
                                (Parser.oneOf
                                    [ Parser.succeed ExposingAll
                                        |> i (Parser.symbol dotDotToken)
                                        |> i Parser.spaces
                                        |> i
                                            (Parser.oneOf
                                                [ Parser.symbol closeParenthesisToken
                                                , Parser.succeed ()
                                                ]
                                            )
                                    , Parser.symbol closeParenthesisToken
                                        |> Parser.map (\() -> ExposingExplicit [])
                                    , Parser.succeed
                                        (\( listOfItems, wasTrailing ) ->
                                            let
                                                ( values, didFilterOut ) =
                                                    List.foldr
                                                        (\cur ( curValues, curDidFilterOut ) ->
                                                            case cur of
                                                                InvalidItem ->
                                                                    ( curValues, curDidFilterOut )

                                                                FilterItemOut ->
                                                                    ( curValues, True )

                                                                Item value ->
                                                                    ( value :: curValues, curDidFilterOut )
                                                        )
                                                        ( [], False )
                                                        listOfItems
                                            in
                                            case wasTrailing of
                                                PExtra.WasTrailing ->
                                                    ExposingTrailing values

                                                PExtra.WasNotTrailing ->
                                                    if didFilterOut then
                                                        ExposingTrailing values

                                                    else
                                                        ExposingExplicit values
                                        )
                                        |> k
                                            (PExtra.sequenceWithOptionalTrailing
                                                { start = emptyToken
                                                , end = emptyToken
                                                , item =
                                                    Parser.oneOf
                                                        [ exposedItem
                                                            |> Parser.map Item
                                                        , Parser.getChompedString (Parser.chompWhile (\c -> c /= ',' && c /= ')'))
                                                            |> Parser.map
                                                                (\chomped ->
                                                                    if String.isEmpty chomped then
                                                                        FilterItemOut

                                                                    else
                                                                        InvalidItem
                                                                )
                                                        ]
                                                , separator = Parser.token commaToken
                                                , spaces = Parser.spaces
                                                }
                                            )
                                        |> i
                                            (Parser.oneOf
                                                [ Parser.symbol closeParenthesisToken
                                                , Parser.succeed ()
                                                ]
                                            )
                                    ]
                                )
                        , Parser.succeed (ExposingTrailing [])
                        ]
                    )
            , Parser.succeed ExposingNone
            ]


exposedItem : Parser ExposedItem
exposedItem =
    Parser.inContext CExposedItems <|
        Parser.oneOf
            [ lowercaseIdentifier |> Parser.map ExposedValue
            , Parser.succeed (\ident constructors -> ExposedType ident constructors)
                |> k uppercaseIdentifier
                |> k exposedConstructors
            , Parser.succeed ExposedOperator
                |> i (Parser.symbol openParenthesisToken)
                |> k operator
                |> i (Parser.symbol closeParenthesisToken)
            ]


exposedConstructors : Parser ExposedCustomTypeConstructors
exposedConstructors =
    Parser.inContext CExposedConstructors <|
        Parser.oneOf
            [ Parser.succeed identity
                |> i (Parser.symbol openParenthesisToken)
                |> i Parser.spaces
                |> k
                    (Parser.oneOf
                        [ Parser.token dotDotToken
                            |> Parser.map (\() -> ExposedConstructorsDotDot)
                        , Parser.sequence
                            { start = emptyToken
                            , end = emptyToken
                            , item = uppercaseIdentifier
                            , separator = commaToken
                            , spaces = Parser.spaces
                            , trailing = Parser.Optional
                            }
                            |> Parser.map ExposedConstructors
                        ]
                    )
                |> i
                    (Parser.oneOf
                        [ Parser.symbol closeParenthesisToken
                        , Parser.succeed ()
                        ]
                    )
            , Parser.succeed NoExposedConstructors
            ]



-- Operators --


operator : Parser Operator
operator =
    Parser.inContext COperator <|
        Parser.oneOf
            [ Parser.succeed identity
                |> i (Parser.symbol plusToken)
                |> k
                    (Parser.oneOf
                        [ Parser.symbol plusToken |> Parser.map (\() -> PlusPlus)
                        , Parser.succeed Plus
                        ]
                    )
            , Parser.succeed identity
                |> i (Parser.symbol forwardSlashToken)
                |> k
                    (Parser.oneOf
                        [ Parser.symbol forwardSlashToken |> Parser.map (\() -> DivideInt)
                        , Parser.succeed DivideFloat
                        ]
                    )
            , Parser.succeed identity
                |> i (Parser.symbol pipeToken)
                |> k
                    (Parser.oneOf
                        [ Parser.symbol rightCarrotToken |> Parser.map (\() -> RightPipe)
                        , Parser.symbol equalsToken |> Parser.map (\() -> ParseKeep)
                        , Parser.symbol dotToken |> Parser.map (\() -> ParseIgnore)
                        ]
                    )
            , Parser.succeed identity
                |> i (Parser.symbol rightCarrotToken)
                |> k
                    (Parser.oneOf
                        [ Parser.symbol equalsToken |> Parser.map (\() -> GreaterThanOrEqual)
                        , Parser.succeed GreaterThan
                        ]
                    )
            , Parser.succeed identity
                |> i (Parser.symbol leftCarrotToken)
                |> k
                    (Parser.oneOf
                        [ Parser.symbol pipeToken |> Parser.map (\() -> LeftPipe)
                        , Parser.symbol equalsToken |> Parser.map (\() -> LessThanOrEqual)
                        , Parser.succeed LessThan
                        ]
                    )
            , Parser.symbol minusToken |> Parser.map (\() -> Minus)
            , Parser.symbol starToken |> Parser.map (\() -> Multiply)
            ]



-- Module Name --


moduleName : Parser ModuleName
moduleName =
    Parser.inContext CModuleName
        (PExtra.sequenceAtLeastOne
            { subParser = uppercaseIdentifier
            , separator = Parser.symbol dotToken
            , spaces = Parser.succeed ()
            }
            |> Parser.map (\( head, rest ) -> ModuleName head rest)
        )



-- Types --


type_ : Parser Type
type_ =
    Parser.inContext CType
        (Parser.oneOf
            [ customType
            , typeTerm
            ]
            |> Parser.andThen
                (\tipe ->
                    Parser.oneOf
                        [ Parser.succeed (\result -> LambdaType tipe result)
                            -- TODO: Remove backtrackable
                            |> i (Parser.backtrackable Parser.spaces)
                            |> i (Parser.token arrowToken)
                            |> i Parser.spaces
                            |> k (Parser.lazy (\() -> type_))
                        , Parser.succeed tipe
                        ]
                )
        )


typeTerm : Parser Type
typeTerm =
    Parser.oneOf
        [ customTypeWithoutArgs
        , lowercaseIdentifier |> Parser.map VariableType
        , unitTupleParensLiteral (Parser.lazy (\() -> type_)) UnitType TupleType
        , recordType
        ]



-- Custom Type and Qualified Custom Type --


customTypeWithoutArgs : Parser Type
customTypeWithoutArgs =
    Parser.succeed (\modName transformer -> transformer modName)
        |> k uppercaseIdentifier
        |> k
            (Parser.oneOf
                [ Parser.succeed
                    (\( moduleNames, ctor ) ->
                        \firstModuleName ->
                            QualType (ModuleName firstModuleName moduleNames) ctor []
                    )
                    |> i (Parser.token dotToken)
                    |> k (Parser.loop [] customTypeHelp)
                , Parser.succeed (\modName -> Type modName [])
                ]
            )


customType : Parser Type
customType =
    Parser.succeed (\modName transformer -> transformer modName)
        |> k uppercaseIdentifier
        |> k
            (Parser.oneOf
                [ Parser.succeed
                    (\( moduleNames, ctor ) subPatterns ->
                        \firstModuleName ->
                            QualType (ModuleName firstModuleName moduleNames) ctor subPatterns
                    )
                    |> i (Parser.token dotToken)
                    |> k (Parser.loop [] customTypeHelp)
                    |> k
                        (PExtra.sequence
                            { start = emptyToken
                            , end = emptyToken
                            , item =
                                Parser.succeed identity
                                    -- TODO: Remove backtrackable?
                                    |> i (Parser.backtrackable atLeastOneSpace)
                                    |> k (Parser.lazy (\() -> typeTerm))
                            , separator = Parser.succeed ()
                            , spaces = Parser.succeed ()
                            , trailing = Parser.Forbidden
                            }
                        )
                , Parser.succeed (\subPatterns -> \modName -> Type modName subPatterns)
                    |> k
                        (PExtra.sequenceWithTrailingLegacy
                            { subParser = Parser.lazy (\() -> typeTerm)
                            , separator = atLeastOneSpace
                            , spaces = Parser.succeed ()
                            }
                        )
                ]
            )


customTypeHelp :
    List UppercaseIdentifier
    -> Parser (Parser.Step (List UppercaseIdentifier) ( List UppercaseIdentifier, UppercaseIdentifier ))
customTypeHelp uppercaseIdentifiers =
    Parser.succeed (\upperVar transformer -> transformer upperVar)
        |> k uppercaseIdentifier
        |> k
            (Parser.oneOf
                [ Parser.token dotToken
                    |> Parser.map (\() -> \upperVar -> Parser.Loop (upperVar :: uppercaseIdentifiers))
                , Parser.succeed
                    (\upperVar -> Parser.Done ( List.reverse uppercaseIdentifiers, upperVar ))
                ]
            )



-- Record Type --


recordType : Parser Type
recordType =
    Parser.succeed (\firstLower transformer -> transformer firstLower)
        |> i (Parser.symbol openCurlyBracketToken)
        |> i Parser.spaces
        |> k lowercaseIdentifier
        |> i Parser.spaces
        |> k
            (Parser.oneOf
                [ Parser.succeed (\rest -> \firstLower -> RecordType (Just firstLower) rest)
                    |> i (Parser.token pipeToken)
                    |> i Parser.spaces
                    |> k
                        (PExtra.sequenceWithTrailingLegacy
                            { subParser =
                                recordKeyValue
                                    (Parser.lazy (\() -> type_))
                                    (Parser.symbol colonToken)
                            , separator = Parser.symbol commaToken
                            , spaces = Parser.spaces
                            }
                        )
                , Parser.succeed
                    (\firstType rest ->
                        \firstLower -> RecordType Nothing (( firstLower, firstType ) :: rest)
                    )
                    |> i (Parser.symbol colonToken)
                    |> i Parser.spaces
                    |> k (Parser.lazy (\() -> type_))
                    |> i Parser.spaces
                    |> k
                        (Parser.oneOf
                            [ Parser.succeed identity
                                |> i (Parser.token commaToken)
                                |> i Parser.spaces
                                |> k
                                    (PExtra.sequenceWithTrailingLegacy
                                        { subParser =
                                            recordKeyValue
                                                (Parser.lazy (\() -> type_))
                                                (Parser.symbol colonToken)
                                        , separator = Parser.symbol commaToken
                                        , spaces = Parser.spaces
                                        }
                                    )
                            , Parser.succeed []
                            ]
                        )
                ]
            )
        |> i (Parser.symbol closeCurlyBracketToken)



-- Type Declaration --


typeDeclaration : Parser TypeDeclaration
typeDeclaration =
    Parser.succeed identity
        |> i (Parser.keyword typeToken)
        |> i atLeastOneSpace
        |> k
            (Parser.oneOf
                [ Parser.succeed TypeAlias
                    |> i (Parser.keyword aliasToken)
                    |> i atLeastOneSpace
                    |> k uppercaseIdentifier
                    |> i atLeastOneSpace
                    |> k
                        (PExtra.sequence
                            { start = emptyToken
                            , end = emptyToken
                            , item = lowercaseIdentifier
                            , separator = atLeastOneSpace
                            , spaces = Parser.succeed ()
                            , trailing = Parser.Mandatory
                            }
                        )
                    |> i (Parser.token equalsToken)
                    |> i atLeastOneSpace
                    |> k type_
                , Parser.succeed
                    (\typeName typeArgs firstCtor restCtors ->
                        CustomType typeName typeArgs (firstCtor :: restCtors)
                    )
                    |> k uppercaseIdentifier
                    |> i atLeastOneSpace
                    |> k
                        (PExtra.sequence
                            { start = emptyToken
                            , end = emptyToken
                            , item = lowercaseIdentifier
                            , separator = atLeastOneSpace
                            , spaces = Parser.succeed ()
                            , trailing = Parser.Optional
                            }
                        )
                    |> i (Parser.token equalsToken)
                    |> i Parser.spaces
                    |> k customTypeCtor
                    |> k
                        (Parser.oneOf
                            [ Parser.succeed identity
                                |> i (Parser.token pipeToken)
                                |> i Parser.spaces
                                |> k
                                    (PExtra.sequence
                                        { start = emptyToken
                                        , end = emptyToken
                                        , item = customTypeCtor
                                        , separator =
                                            Parser.succeed ()
                                                |> i Parser.spaces
                                                |> i (Parser.token pipeToken)
                                                |> i Parser.spaces
                                        , spaces = Parser.succeed ()
                                        , trailing = Parser.Forbidden
                                        }
                                    )
                            , Parser.succeed []
                            ]
                        )
                ]
            )


customTypeCtor : Parser ( UppercaseIdentifier, List Type )
customTypeCtor =
    Parser.succeed Tuple.pair
        |> k uppercaseIdentifier
        |> k
            (PExtra.sequence
                { start = emptyToken
                , end = emptyToken
                , item =
                    Parser.succeed identity
                        -- TODO: Remove backtrackable?
                        |> i (Parser.backtrackable atLeastOneSpace)
                        |> k type_
                , separator = Parser.succeed ()
                , spaces = Parser.succeed ()
                , trailing = Parser.Optional
                }
            )
        |> i Parser.spaces



-- Value Declaration --


valueDeclaration : Parser ValueDeclaration
valueDeclaration =
    valueDeclarationWithExpressionParser expression


valueDeclarationWithExpressionParser : Parser Expression -> Parser ValueDeclaration
valueDeclarationWithExpressionParser parseExpression =
    Parser.inContext CDeclaration <|
        Parser.oneOf
            [ Parser.succeed (\name transformer -> transformer name)
                |> k variableLiteral
                |> k
                    (Parser.oneOf
                        [ Parser.inContext CFunctionDeclaration
                            (Parser.succeed
                                (\( firstPattern, restPatterns ) declarations ->
                                    \name ->
                                        FunctionDeclaration name firstPattern restPatterns declarations
                                )
                                -- TODO: Remove backtrackable?
                                |> i (Parser.backtrackable atLeastOneSpace)
                                |> k
                                    (PExtra.sequenceAtLeastOne
                                        { subParser = patternTerm
                                        , separator = atLeastOneSpace
                                        , spaces = Parser.succeed ()
                                        }
                                    )
                                |> i Parser.spaces
                                |> i (Parser.symbol equalsToken)
                                |> i Parser.spaces
                                |> k parseExpression
                            )
                        , Parser.inContext CValueDeclaration
                            (Parser.succeed (\declarations -> \name -> ValueDeclaration name declarations)
                                |> i Parser.spaces
                                |> i (Parser.symbol equalsToken)
                                |> i Parser.spaces
                                |> k parseExpression
                            )
                        ]
                    )
            , Parser.inContext CPatternMatchDeclaration
                (Parser.succeed PatternMatchDeclaration
                    |> k pattern
                    |> i Parser.spaces
                    |> i (Parser.symbol equalsToken)
                    |> i Parser.spaces
                    |> k parseExpression
                )
            ]



-- Pattern --


pattern : Parser Pattern
pattern =
    Parser.inContext CPattern
        (Parser.succeed (\pat transform -> transform pat)
            |> k
                (Parser.oneOf
                    [ ctorPattern
                    , patternTerm
                    ]
                )
            |> k
                (Parser.oneOf
                    [ Parser.succeed identity
                        -- TODO: Remove backtrackable?
                        |> i (Parser.backtrackable atLeastOneSpace)
                        |> k
                            (Parser.oneOf
                                [ Parser.inContext CAliasPattern
                                    (Parser.succeed (\alias_ -> \pat -> AliasPattern pat alias_)
                                        |> i (Parser.keyword asToken)
                                        |> i atLeastOneSpace
                                        |> k lowercaseIdentifier
                                    )
                                , Parser.inContext CConsPattern
                                    (Parser.succeed (\rest -> \head -> ConsPattern head rest)
                                        |> i (Parser.keyword consToken)
                                        |> i atLeastOneSpace
                                        |> k (Parser.lazy (\() -> pattern))
                                    )
                                ]
                            )
                    , Parser.succeed identity
                    ]
                )
        )


patternTerm : Parser Pattern
patternTerm =
    Parser.oneOf
        [ Parser.succeed AnythingPattern
            |> i (Parser.symbol underscoreToken)
        , Parser.inContext CRecordPattern
            (Parser.succeed RecordPattern
                |> i (Parser.symbol openCurlyBracketToken)
                |> i Parser.spaces
                |> k
                    (PExtra.sequenceWithTrailingLegacy
                        { subParser = lowercaseIdentifier
                        , separator = Parser.symbol commaToken
                        , spaces = Parser.spaces
                        }
                    )
                |> i (Parser.symbol closeCurlyBracketToken)
            )
        , Parser.inContext CCtorPattern ctorPatternWithoutArgs
        , charLiteral |> Parser.map CharPattern
        , stringLiteral |> Parser.map StringPattern
        , variableLiteral |> Parser.map LowerPattern
        , numberLiteral IntPattern FloatPattern
        , listLiteral (Parser.lazy (\() -> pattern))
            |> Parser.map ListPattern
        , unitTupleParensLiteral (Parser.lazy (\() -> pattern))
            UnitPattern
            TuplePattern
        ]



-- Ctor Pattern and Qualified Ctor Pattern --


ctorPatternWithoutArgs : Parser Pattern
ctorPatternWithoutArgs =
    Parser.inContext CCtorPattern
        (Parser.succeed (\modName transformer -> transformer modName)
            |> k uppercaseIdentifier
            |> k
                (Parser.oneOf
                    [ Parser.succeed
                        (\( moduleNames, ctor ) ->
                            \firstModuleName ->
                                QualCtorPattern (ModuleName firstModuleName moduleNames) ctor []
                        )
                        |> i (Parser.token dotToken)
                        |> k (Parser.loop [] qualifiedCtorPatternHelp)
                    , Parser.succeed (\modName -> CtorPattern modName [])
                    ]
                )
        )


ctorPattern : Parser Pattern
ctorPattern =
    Parser.inContext CCtorPattern
        (Parser.succeed (\modName transformer -> transformer modName)
            |> k uppercaseIdentifier
            |> k
                (Parser.oneOf
                    [ Parser.succeed
                        (\( moduleNames, ctor ) subPatterns ->
                            \firstModuleName ->
                                QualCtorPattern (ModuleName firstModuleName moduleNames) ctor subPatterns
                        )
                        |> i (Parser.token dotToken)
                        |> k (Parser.loop [] qualifiedCtorPatternHelp)
                        |> k
                            (PExtra.sequence
                                { start = emptyToken
                                , end = emptyToken
                                , item =
                                    Parser.succeed identity
                                        -- TODO: Remove backtrackable?
                                        |> i (Parser.backtrackable atLeastOneSpace)
                                        |> k (Parser.lazy (\() -> patternTerm))
                                , separator = Parser.succeed ()
                                , spaces = Parser.succeed ()
                                , trailing = Parser.Forbidden
                                }
                            )
                    , Parser.map (\subPatterns -> \modName -> CtorPattern modName subPatterns)
                        (PExtra.sequenceWithTrailingLegacy
                            { subParser = Parser.lazy (\() -> patternTerm)
                            , separator = atLeastOneSpace
                            , spaces = Parser.succeed ()
                            }
                        )
                    ]
                )
        )


qualifiedCtorPatternHelp :
    List UppercaseIdentifier
    -> Parser (Parser.Step (List UppercaseIdentifier) ( List UppercaseIdentifier, UppercaseIdentifier ))
qualifiedCtorPatternHelp uppercaseIdentifiers =
    Parser.succeed (\upperVar transformer -> transformer upperVar)
        |> k uppercaseIdentifier
        |> k
            (Parser.oneOf
                [ Parser.map (\() -> \upperVar -> Parser.Loop (upperVar :: uppercaseIdentifiers))
                    (Parser.token dotToken)
                , Parser.succeed
                    (\upperVar -> Parser.Done ( List.reverse uppercaseIdentifiers, upperVar ))
                ]
            )



-- Expression


expression : Parser Expression
expression =
    Parser.getCol
        |> Parser.andThen
            (\startOfExpression ->
                Parser.oneOf
                    [ letExpression
                    , ifExpression
                    , caseExpression
                    , lambdaExpression
                    , possiblyNegativeTerm
                        |> Parser.andThen (endOfExpression startOfExpression)
                    ]
            )



-- Expression Term --


possiblyNegativeTerm : Parser Expression
possiblyNegativeTerm =
    Parser.oneOf
        [ Parser.succeed NegateExpression
            |> i (Parser.token minusToken)
            |> k expressionTerm
        , expressionTerm
        ]


expressionTerm : Parser Expression
expressionTerm =
    Parser.succeed identity
        |> i checkIndent
        |> k
            (Parser.oneOf
                [ Parser.inContext CVariable <|
                    (variableLiteral
                        |> Parser.map VarExpression
                        |> Parser.andThen accessible
                    )
                , qualifiedVariableExpression
                    |> Parser.andThen accessible
                , recordOrUpdateExpression
                    |> Parser.andThen accessible
                , charLiteral |> Parser.map CharExpression
                , Parser.inContext CString <|
                    (stringLiteral |> Parser.map StringExpression)
                , Parser.inContext CList <|
                    (listLiteral (Parser.lazy (\() -> expression)) |> Parser.map ListExpression)
                , unitTupleParensLiteral (Parser.lazy (\() -> expression))
                    UnitExpression
                    TupleExpression

                -- Must come before number
                , Parser.succeed AccessorExpression
                    |> i (Parser.backtrackable (Parser.token dotToken))
                    |> k lowercaseIdentifier
                , Parser.inContext CNumber <| numberLiteral IntExpression FloatExpression
                ]
            )



-- After Expression --


accessible : Expression -> Parser Expression
accessible exp =
    Parser.oneOf
        [ Parser.succeed (\field -> AccessExpression exp field)
            |> i (Parser.token dotToken)
            |> k lowercaseIdentifier
        , Parser.succeed exp
        ]


endOfExpression : Int -> Expression -> Parser Expression
endOfExpression startOfExpression exp =
    Parser.oneOf
        [ Parser.inContext CCallExpression
            (Parser.succeed
                (\( firstArg, restArgs ) ->
                    CallExpression exp firstArg restArgs
                )
                |> k
                    (Parser.withIndent
                        startOfExpression
                        (PExtra.sequence
                            { start = emptyToken
                            , end = emptyToken
                            , item =
                                Parser.succeed identity
                                    -- TODO: Remove backtrackable?
                                    |> i (Parser.backtrackable atLeastOneSpace)
                                    |> k (Parser.lazy (\() -> expressionTerm))
                            , separator = Parser.succeed ()
                            , spaces = Parser.succeed ()
                            , trailing = Parser.Forbidden
                            }
                            |> Parser.andThen
                                (\args ->
                                    case args of
                                        [] ->
                                            Parser.problem ExpectingArguement

                                        head :: rest ->
                                            Parser.succeed ( head, rest )
                                )
                        )
                    )
            )
        , Parser.succeed (\op secondExp -> BinOpCallExpression exp op secondExp)
            -- TODO: Remove backtrackable?
            |> i (Parser.backtrackable Parser.spaces)
            |> k operator
            |> i Parser.spaces
            |> k (Parser.lazy (\() -> expression))
        , Parser.succeed exp
        ]



-- Lambda Expression --


lambdaExpression : Parser Expression
lambdaExpression =
    Parser.inContext CLambdaExpression
        (Parser.succeed
            (\( firstPattern, restPatterns ) exp ->
                LambdaExpression firstPattern restPatterns exp
            )
            |> i (Parser.symbol backSlashToken)
            |> k
                (PExtra.sequenceAtLeastOne
                    { subParser = pattern
                    , separator = atLeastOneSpace
                    , spaces = Parser.succeed ()
                    }
                )
            |> i Parser.spaces
            |> i (Parser.symbol arrowToken)
            |> i Parser.spaces
            |> k (Parser.lazy (\() -> expression))
        )



-- Record/Update Expression --


recordOrUpdateExpression : Parser Expression
recordOrUpdateExpression =
    Parser.succeed identity
        |> i (Parser.symbol openCurlyBracketToken)
        |> i Parser.spaces
        |> k
            (Parser.oneOf
                [ Parser.inContext CRecordExpression
                    (Parser.map RecordExpression
                        (PExtra.sequenceWithTrailingLegacy
                            { subParser =
                                recordKeyValue
                                    (Parser.lazy (\() -> expression))
                                    (Parser.symbol equalsToken)
                            , separator = Parser.symbol commaToken
                            , spaces = Parser.spaces
                            }
                        )
                    )
                , Parser.inContext CUpdateExpression
                    (Parser.succeed
                        (\var ( firstProperty, restProperties ) ->
                            UpdateExpression var
                                firstProperty
                                restProperties
                        )
                        |> k lowercaseIdentifier
                        |> i Parser.spaces
                        |> i (Parser.symbol pipeToken)
                        |> i Parser.spaces
                        |> k
                            (PExtra.sequenceAtLeastOne
                                { subParser =
                                    recordKeyValue (Parser.lazy (\() -> expression))
                                        (Parser.symbol equalsToken)
                                , separator = atLeastOneSpace
                                , spaces = Parser.succeed ()
                                }
                            )
                        |> i Parser.spaces
                    )
                ]
            )
        |> i (Parser.symbol closeCurlyBracketToken)



-- Qualified variable Expression --


qualifiedVariableExpression : Parser Expression
qualifiedVariableExpression =
    Parser.loop [] qualifiedVariableExpressionHelp
        |> Parser.andThen
            (\( moduleNames, var ) ->
                case moduleNames of
                    [] ->
                        Parser.problem ExpectingAtLeastOneModuleName

                    head :: rest ->
                        Parser.succeed (QualVarExpression (ModuleName head rest) var)
            )


qualifiedVariableExpressionHelp :
    List UppercaseIdentifier
    -> Parser (Parser.Step (List UppercaseIdentifier) ( List UppercaseIdentifier, LowercaseIdentifier ))
qualifiedVariableExpressionHelp uppercaseIdentifiers =
    Parser.oneOf
        [ Parser.succeed
            (\modName -> Parser.Loop (modName :: uppercaseIdentifiers))
            |> k uppercaseIdentifier
            |> i (Parser.token dotToken)
        , if List.isEmpty uppercaseIdentifiers then
            Parser.problem InternalQualifiedVarExpressionProblem

          else
            Parser.map
                (\lowercaseVar ->
                    Parser.Done
                        ( List.reverse uppercaseIdentifiers
                        , lowercaseVar
                        )
                )
                lowercaseIdentifier
        ]



-- If Expression --


ifExpression : Parser Expression
ifExpression =
    Parser.succeed IfExpression
        |> i (Parser.keyword ifToken)
        |> i atLeastOneSpace
        |> k (Parser.lazy (\() -> expression))
        |> i atLeastOneSpace
        |> i (Parser.keyword thenToken)
        |> i atLeastOneSpace
        |> k (Parser.lazy (\() -> expression))
        |> i atLeastOneSpace
        |> i (Parser.keyword elseToken)
        |> i atLeastOneSpace
        |> k (Parser.lazy (\() -> expression))



-- Case Expression --


caseExpression : Parser Expression
caseExpression =
    Parser.succeed CaseExpression
        |> i (Parser.keyword caseToken)
        |> i atLeastOneSpace
        |> k pattern
        |> i atLeastOneSpace
        |> i (Parser.keyword ofToken)
        |> i atLeastOneSpace
        |> k (Parser.loop [] caseExpressionHelp)


caseExpressionHelp :
    List ( Pattern, Expression )
    -> Parser (Parser.Step (List ( Pattern, Expression )) (List ( Pattern, Expression )))
caseExpressionHelp items =
    Parser.oneOf
        [ Parser.succeed
            (\casePat caseExp -> Parser.Loop (( casePat, caseExp ) :: items))
            |> k pattern
            |> i Parser.spaces
            |> i (Parser.token arrowToken)
            |> i Parser.spaces
            |> k (Parser.lazy (\() -> expression))
            |> i atLeastOneSpace
        , Parser.succeed (Parser.Done (List.reverse items))
        ]



-- Let Expression --


letExpression : Parser Expression
letExpression =
    Parser.inContext CLetExpression
        (Parser.succeed LetExpression
            |> i (Parser.keyword letToken)
            |> i atLeastOneSpace
            |> k
                (Parser.inContext CLetExpressionDeclarations <|
                    Parser.loop [] letExpressionHelp
                )
            |> k
                (Parser.inContext CLetExpressionBody
                    (Parser.succeed identity
                        -- No spacesAtLeastOne here because the space is parsed
                        -- after the last let declaration
                        |> i (Parser.keyword inToken)
                        |> i atLeastOneSpace
                        |> k (Parser.lazy (\_ -> expression))
                    )
                )
        )


letExpressionHelp :
    List ValueDeclaration
    -> Parser (Parser.Step (List ValueDeclaration) (List ValueDeclaration))
letExpressionHelp items =
    Parser.oneOf
        [ Parser.succeed (\nextItem -> Parser.Loop (nextItem :: items))
            |> k (valueDeclarationWithExpressionParser (Parser.lazy (\() -> expression)))
            |> i atLeastOneSpace
        , Parser.succeed (Parser.Done (List.reverse items))
        ]



-- Record Helpers --


recordKeyValue : Parser item -> Parser sep -> Parser ( LowercaseIdentifier, item )
recordKeyValue item separator =
    Parser.inContext CRecordKeyValue
        (Parser.succeed Tuple.pair
            |> k lowercaseIdentifier
            |> i Parser.spaces
            |> i separator
            |> i Parser.spaces
            |> k item
            |> i Parser.spaces
        )



-- Literals --


variableLiteral : Parser String
variableLiteral =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = keywordsAsSet
        , expecting = ExpectingVariable
        }


charLiteral : Parser String
charLiteral =
    Parser.inContext CChar
        (Parser.succeed identity
            |> i (Parser.symbol singleQuoteToken)
            |> k
                (Parser.oneOf
                    [ Parser.getChompedString
                        (Parser.succeed ()
                            |> i (Parser.chompIf (\c -> c == '\\') ExpectingBackSlash)
                            |> i (Parser.chompIf Char.isAlphaNum ExpectingCharacter)
                        )
                    , Parser.chompIf Char.isAlphaNum ExpectingCharacter |> Parser.getChompedString
                    ]
                )
            |> i (Parser.symbol singleQuoteToken)
        )


stringLiteral : Parser String
stringLiteral =
    Parser.succeed identity
        |> i (Parser.token doubleQuoteToken)
        |> k (Parser.loop [] stringLiteralHelp)


stringLiteralHelp : List String -> Parser (Parser.Step (List String) String)
stringLiteralHelp revChunks =
    Parser.oneOf
        [ Parser.succeed ()
            |> i (Parser.token backSlashToken)
            |> i (Parser.chompIf (\_ -> True) ExpectingAny)
            |> Parser.getChompedString
            |> Parser.map (\chunk -> Parser.Loop (chunk :: revChunks))
        , Parser.token doubleQuoteToken
            |> Parser.map (\_ -> Parser.Done (String.join "" (List.reverse revChunks)))
        , Parser.chompWhile isUninteresting
            |> Parser.getChompedString
            |> Parser.map (\chunk -> Parser.Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'


numberLiteral : (Int -> decodesTo) -> (Float -> decodesTo) -> Parser decodesTo
numberLiteral fromInt fromFloat =
    Parser.inContext CNumber <|
        Parser.number
            { int = Ok fromInt
            , hex = Ok fromInt
            , octal = Err InvalidNumber
            , binary = Err InvalidNumber
            , float = Ok fromFloat
            , invalid = InvalidNumber
            , expecting = ExpectingNumber
            }


{-| TODO: Do these contexts make sense?
-}
unitTupleParensLiteral :
    Parser decodesTo
    -> decodesTo
    -> (decodesTo -> decodesTo -> List decodesTo -> decodesTo)
    -> Parser decodesTo
unitTupleParensLiteral subParser fromUnit fromTuple =
    Parser.succeed identity
        |> i (Parser.symbol openParenthesisToken)
        |> k
            (Parser.oneOf
                [ Parser.map (\_ -> fromUnit) (Parser.symbol closeParenthesisToken)
                , Parser.inContext CTupleParens
                    (Parser.succeed (\first transformer -> transformer first)
                        |> i Parser.spaces
                        |> k subParser
                        |> i Parser.spaces
                        |> k
                            (Parser.oneOf
                                [ Parser.succeed identity
                                    |> i (Parser.symbol closeParenthesisToken)
                                , Parser.succeed (\second rest -> \first -> fromTuple first second rest)
                                    |> i (Parser.symbol commaToken)
                                    |> i Parser.spaces
                                    |> k subParser
                                    |> i Parser.spaces
                                    |> k
                                        (Parser.oneOf
                                            [ Parser.succeed identity
                                                |> i (Parser.symbol commaToken)
                                                |> k
                                                    (Parser.sequence
                                                        { start = emptyToken
                                                        , end = emptyToken
                                                        , item = subParser
                                                        , separator = commaToken
                                                        , spaces = Parser.spaces
                                                        , trailing = Parser.Optional
                                                        }
                                                    )
                                            , Parser.succeed []
                                            ]
                                        )
                                    |> i (Parser.symbol closeParenthesisToken)
                                ]
                            )
                    )
                ]
            )


listLiteral : Parser decodesTo -> Parser (List decodesTo)
listLiteral subParser =
    Parser.inContext CList
        (Parser.succeed identity
            |> i (Parser.symbol openSquareBracketToken)
            |> i Parser.spaces
            |> k (Parser.succeed identity)
            |> k
                (PExtra.sequenceWithTrailingLegacy
                    { subParser = subParser
                    , separator = Parser.symbol commaToken
                    , spaces = Parser.spaces
                    }
                )
            |> i Parser.spaces
            |> i (Parser.symbol closeSquareBracketToken)
        )



-- Tokens --


lowercaseIdentifier : Parser String
lowercaseIdentifier =
    Parser.succeed ()
        |> i (Parser.chompIf Char.isLower ExpectingLowerCharacter)
        |> i (Parser.chompWhile identifierHelp)
        |> Parser.getChompedString


uppercaseIdentifier : Parser String
uppercaseIdentifier =
    Parser.succeed ()
        |> i (Parser.chompIf Char.isUpper ExpectingUpperCharacter)
        |> i (Parser.chompWhile identifierHelp)
        |> Parser.getChompedString


identifierHelp : Char -> Bool
identifierHelp c =
    Char.isLower c || Char.isUpper c || Char.isDigit c



-- Keywords --


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


caseString : String
caseString =
    "case"


ofString : String
ofString =
    "of"


ifString : String
ifString =
    "if"


thenString : String
thenString =
    "then"


elseString : String
elseString =
    "else"


keywords : List String
keywords =
    [ letString
    , inString
    , asString
    , typeString
    , aliasString
    , moduleString
    , exposingString
    , portString
    , importString
    , caseString
    , ofString
    , ifString
    , thenString
    , elseString
    ]


keywordsAsSet : Set.Set String
keywordsAsSet =
    Set.fromList keywords



-- Tokens --


asToken : Parser.Token Problem
asToken =
    Parser.Token asString ExpectingAs


typeToken : Parser.Token Problem
typeToken =
    Parser.Token typeString ExpectingType


aliasToken : Parser.Token Problem
aliasToken =
    Parser.Token aliasString ExpectingAlias


moduleToken : Parser.Token Problem
moduleToken =
    Parser.Token moduleString ExpectingModule


exposingToken : Parser.Token Problem
exposingToken =
    Parser.Token exposingString ExpectingExposing


portToken : Parser.Token Problem
portToken =
    Parser.Token portString ExpectingPort


importToken : Parser.Token Problem
importToken =
    Parser.Token importString ExpectingImport


letToken : Parser.Token Problem
letToken =
    Parser.Token letString ExpectingLet


inToken : Parser.Token Problem
inToken =
    Parser.Token inString ExpectingIn


caseToken : Parser.Token Problem
caseToken =
    Parser.Token caseString ExpectingCase


ofToken : Parser.Token Problem
ofToken =
    Parser.Token ofString ExpectingOf


ifToken : Parser.Token Problem
ifToken =
    Parser.Token ifString ExpectingIf


thenToken : Parser.Token Problem
thenToken =
    Parser.Token thenString ExpectingThen


elseToken : Parser.Token Problem
elseToken =
    Parser.Token elseString ExpectingElse


consToken : Parser.Token Problem
consToken =
    Parser.Token "::" ExpectingCons


colonToken : Parser.Token Problem
colonToken =
    Parser.Token ":" ExpectingColon


openParenthesisToken : Parser.Token Problem
openParenthesisToken =
    Parser.Token "(" ExpectingOpenParen


closeParenthesisToken : Parser.Token Problem
closeParenthesisToken =
    Parser.Token ")" ExpectingCloseParen


commaToken : Parser.Token Problem
commaToken =
    Parser.Token "," ExpectingComma


dotToken : Parser.Token Problem
dotToken =
    Parser.Token "." ExpectingDot


dotDotToken : Parser.Token Problem
dotDotToken =
    Parser.Token ".." ExpectingDotDot


plusToken : Parser.Token Problem
plusToken =
    Parser.Token "+" ExpectingPlus


minusToken : Parser.Token Problem
minusToken =
    Parser.Token "-" ExpectingMinus


starToken : Parser.Token Problem
starToken =
    Parser.Token "*" ExpectingStar


forwardSlashToken : Parser.Token Problem
forwardSlashToken =
    Parser.Token "/" ExpectingForwardSlash


rightCarrotToken : Parser.Token Problem
rightCarrotToken =
    Parser.Token ">" ExpectingRightCarrot


leftCarrotToken : Parser.Token Problem
leftCarrotToken =
    Parser.Token "<" ExpectingLeftCarrot


pipeToken : Parser.Token Problem
pipeToken =
    Parser.Token "|" ExpectingPipe


equalsToken : Parser.Token Problem
equalsToken =
    Parser.Token "=" ExpectingEqual


backSlashToken : Parser.Token Problem
backSlashToken =
    Parser.Token "\\" ExpectingBackSlash


arrowToken : Parser.Token Problem
arrowToken =
    Parser.Token "->" ExpectingArrow


underscoreToken : Parser.Token Problem
underscoreToken =
    Parser.Token "_" ExpectingUnderscore


openCurlyBracketToken : Parser.Token Problem
openCurlyBracketToken =
    Parser.Token "{" ExpectingOpenCurlyBracket


closeCurlyBracketToken : Parser.Token Problem
closeCurlyBracketToken =
    Parser.Token "}" ExpectingCloseCurlyBracket


negateToken : Parser.Token Problem
negateToken =
    Parser.Token "!" ExpectingNegate


singleQuoteToken : Parser.Token Problem
singleQuoteToken =
    Parser.Token "'" ExpectingSingleQuote


doubleQuoteToken : Parser.Token Problem
doubleQuoteToken =
    Parser.Token "\"" ExpectingDoubleQuote


openSquareBracketToken : Parser.Token Problem
openSquareBracketToken =
    Parser.Token "[" ExpectingOpenSquareBracket


closeSquareBracketToken : Parser.Token Problem
closeSquareBracketToken =
    Parser.Token "]" ExpectingCloseSquareBracket


emptyToken : Parser.Token Problem
emptyToken =
    Parser.Token "" ExpectingNothing


newLineToken : Parser.Token Problem
newLineToken =
    Parser.Token "\n" ExpectingNothing



-- Parser Extras Applied --


atLeastOneSpace : Parser ()
atLeastOneSpace =
    PExtra.spacesAtLeastOne ExpectingSpaces



-- Indent Helpers --


checkIndent : Parser ()
checkIndent =
    Parser.succeed (\indent column -> indent <= column)
        |> k Parser.getIndent
        |> k Parser.getCol
        |> Parser.andThen checkIndentHelp


checkIndentHelp : Bool -> Parser ()
checkIndentHelp isIndented =
    if isIndented then
        Parser.succeed ()

    else
        Parser.problem ExpectingIndent



-- Located --


type alias Located a =
    { start : ( Int, Int )
    , value : a
    , end : ( Int, Int )
    }


located : Parser a -> Parser (Located a)
located parser =
    Parser.succeed Located
        |> k Parser.getPosition
        |> k parser
        |> k Parser.getPosition
