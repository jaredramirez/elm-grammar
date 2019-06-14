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
    , type_
    , uppercaseIdentifier
    )

import Elm.AST exposing (..)
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Extra as PExtra
import Set


type alias Parser a =
    Parser.Parser Context Problem a


type Context
    = CRecordKeyValue
    | CNegateExpression
    | CLambdaExpression
    | CRecordExpression
    | CUpdateExpression
    | CAccessorExpression
    | CLetExpression
    | CLetExpressionDeclarations
    | CLetExpressionBody
    | CAccessExpression
    | CCallExpression
    | CPattern
    | CRecordPattern
    | CCtorPattern
    | CAliasPattern
    | CConsPattern
    | CChar
    | CString
    | CVariable
    | CList
    | CNumber
    | CTupleParens
    | CFunctionDeclaration
    | CValueDeclaration
    | CValuePatternMatchDeclaration
    | CModuleName
    | COperator
    | CExposedItems
    | CExposedConstructors
    | CExposingList
    | CModuleImport ModuleName
    | CModuleDeclaration
    | CDeclaration
    | CType


type Problem
    = ExpectingModule
    | ExpectingPort
    | ExpectingExposing
    | ExpectingImport
    | ExpectingSpaces
    | ExpectingAs
    | ExpectingType
    | ExpectingAlias
    | ExpectingLet
    | ExpectingIn
    | ExpectingOpenParen
    | ExpectingCloseParen
    | ExpectingComma
    | ExpectingDot
    | ExpectingDotDot
    | ExpectingCons
    | ExpectingPlus
    | ExpectingMinus
    | ExpectingStar
    | ExpectingForwardSlash
    | ExpectingRightCarrot
    | ExpectingLeftCarrot
    | ExpectingPipe
    | ExpectingEqual
    | ExpectingBackSlash
    | ExpectingArrow
    | ExpectingUnderscore
    | ExpectingOpenCurlyBracket
    | ExpectingCloseCurlyBracket
    | ExpectingNegate
    | ExpectingVariable
    | ExpectingSingleQuote
    | ExpectingDoubleQuote
    | ExpectingCharacter
    | ExpectingOpenSquareBracket
    | ExpectingCloseSquareBracket
    | ExpectingNumber
    | InvalidNumber
    | ExpectingLowerCharacter
    | ExpectingUpperCharacter
    | ExpectingNothing
    | ExpectingAny
    | ExpectingCase
    | ExpectingOf
    | ExpectingIndent
    | ExpectingArguement
    | ExpectingIf
    | ExpectingThen
    | ExpectingElse
    | InternalCallExpressionProblem
    | InternalQualifiedVarExpressionProblem
    | ExpectingAtLeastOneModuleName
    | ExpectingColon



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
                |= PExtra.sequenceWithTrailing
                    { subParser = moduleImport
                    , separator = spacesAtLeastOne
                    , spaces = Parser.succeed ()
                    }
            , Parser.succeed []
            ]



-- Module Declaration --


moduleDeclaration : Parser ModuleDeclaration
moduleDeclaration =
    Parser.inContext CModuleDeclaration <|
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
                    [ Parser.keyword moduleToken
                    , Parser.succeed ()
                        |. Parser.keyword portToken
                        |. Parser.spaces
                        |. Parser.keyword moduleToken
                    ]
                |. Parser.spaces
                |= moduleName
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed identity
                        |. Parser.keyword exposingToken
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
    moduleImportName
        |> Parser.andThen
            (\importedModuleName ->
                Parser.inContext (CModuleImport importedModuleName) <|
                    Parser.succeed (ModuleImport importedModuleName)
                        |= Parser.oneOf
                            [ Parser.succeed identity
                                |. Parser.token asToken
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
                                |. Parser.keyword exposingToken
                                |. Parser.spaces
                                |= Parser.oneOf
                                    [ exposingList |> Parser.map Just
                                    , Parser.succeed Nothing
                                    ]
                            , Parser.succeed Nothing
                            ]
            )


moduleImportName : Parser ModuleName
moduleImportName =
    Parser.succeed identity
        |. Parser.spaces
        |. Parser.keyword importToken
        |. Parser.spaces
        |= moduleName
        |. Parser.spaces



-- Exposing List --


exposingList : Parser ExposingList
exposingList =
    Parser.inContext CExposingList <|
        Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol openParenthesisToken
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed ExposingListDoubleDot
                        |. Parser.token consToken
                    , PExtra.sequenceWithTrailing
                        { subParser = exposedItem
                        , separator = Parser.symbol commaToken
                        , spaces = Parser.spaces
                        }
                        |> Parser.map ExposingList
                    ]
                |. Parser.oneOf
                    [ Parser.symbol closeParenthesisToken
                    , Parser.succeed ()
                    ]
            ]


exposedItem : Parser ExposedItem
exposedItem =
    Parser.inContext CExposedItems <|
        Parser.oneOf
            [ lowercaseIdentifier |> Parser.map ExposedValue
            , Parser.succeed (\ident constructors -> ExposedType ident constructors)
                |= uppercaseIdentifier
                |= exposedConstructors
            , Parser.succeed ExposedOperator
                |. Parser.symbol openParenthesisToken
                |= operator
                |. Parser.symbol closeParenthesisToken
            ]


exposedConstructors : Parser ExposedCustomTypeConstructors
exposedConstructors =
    Parser.inContext CExposedConstructors <|
        Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol openParenthesisToken
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.token dotDotToken
                        |> Parser.map (\() -> ExposedConstructorsDotDot)
                    , PExtra.sequenceWithTrailing
                        { subParser = uppercaseIdentifier
                        , separator = Parser.symbol commaToken
                        , spaces = Parser.spaces
                        }
                        |> Parser.map ExposedConstructors
                    ]
                |. Parser.oneOf
                    [ Parser.symbol closeParenthesisToken
                    , Parser.succeed ()
                    ]
            , Parser.succeed NoExposedConstructors
            ]



-- Operators --


operator : Parser Operator
operator =
    Parser.inContext COperator <|
        Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol plusToken
                |= Parser.oneOf
                    [ Parser.symbol plusToken |> Parser.map (\() -> PlusPlus)
                    , Parser.succeed Plus
                    ]
            , Parser.succeed identity
                |. Parser.symbol forwardSlashToken
                |= Parser.oneOf
                    [ Parser.symbol forwardSlashToken |> Parser.map (\() -> DivideInt)
                    , Parser.succeed DivideFloat
                    ]
            , Parser.succeed identity
                |. Parser.symbol pipeToken
                |= Parser.oneOf
                    [ Parser.symbol rightCarrotToken |> Parser.map (\() -> RightPipe)
                    , Parser.symbol equalsToken |> Parser.map (\() -> ParseKeep)
                    , Parser.symbol dotToken |> Parser.map (\() -> ParseIgnore)
                    ]
            , Parser.succeed identity
                |. Parser.symbol rightCarrotToken
                |= Parser.oneOf
                    [ Parser.symbol equalsToken |> Parser.map (\() -> GreaterThanOrEqual)
                    , Parser.succeed GreaterThan
                    ]
            , Parser.succeed identity
                |. Parser.symbol leftCarrotToken
                |= Parser.oneOf
                    [ Parser.symbol pipeToken |> Parser.map (\() -> LeftPipe)
                    , Parser.symbol equalsToken |> Parser.map (\() -> LessThanOrEqual)
                    , Parser.succeed LessThan
                    ]
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
                            |. Parser.backtrackable Parser.spaces
                            |. Parser.token arrowToken
                            |. Parser.spaces
                            |= Parser.lazy (\() -> type_)
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
        |= uppercaseIdentifier
        |= Parser.oneOf
            [ Parser.succeed
                (\( moduleNames, ctor ) ->
                    \firstModuleName ->
                        QualCustomType (ModuleName firstModuleName moduleNames) ctor []
                )
                |. Parser.token dotToken
                |= Parser.loop [] customTypeHelp
            , Parser.succeed (\modName -> CustomType modName [])
            ]


customType : Parser Type
customType =
    Parser.succeed (\modName transformer -> transformer modName)
        |= uppercaseIdentifier
        |= Parser.oneOf
            [ Parser.succeed
                (\( moduleNames, ctor ) subPatterns ->
                    \firstModuleName ->
                        QualCustomType (ModuleName firstModuleName moduleNames) ctor subPatterns
                )
                |. Parser.token dotToken
                |= Parser.loop [] customTypeHelp
                |. spacesAtLeastOne
                |= PExtra.sequence
                    { start = emptyToken
                    , end = emptyToken
                    , item = Parser.lazy (\() -> typeTerm)

                    -- TODO: Remove backtrackable?
                    , separator = Parser.backtrackable spacesAtLeastOne
                    , spaces = Parser.succeed ()
                    , trailing = Parser.Forbidden
                    }
            , Parser.succeed (\subPatterns -> \modName -> CustomType modName subPatterns)
                |= PExtra.sequenceWithTrailing
                    { subParser = Parser.lazy (\() -> typeTerm)
                    , separator = spacesAtLeastOne
                    , spaces = Parser.succeed ()
                    }
            ]


customTypeHelp :
    List UppercaseIdentifier
    -> Parser (Parser.Step (List UppercaseIdentifier) ( List UppercaseIdentifier, UppercaseIdentifier ))
customTypeHelp uppercaseIdentifiers =
    Parser.succeed (\upperVar transformer -> transformer upperVar)
        |= uppercaseIdentifier
        |= Parser.oneOf
            [ Parser.token dotToken
                |> Parser.map (\() -> \upperVar -> Parser.Loop (upperVar :: uppercaseIdentifiers))
            , Parser.succeed
                (\upperVar -> Parser.Done ( List.reverse uppercaseIdentifiers, upperVar ))
            ]



-- Record Type --


recordType : Parser Type
recordType =
    Parser.succeed (\firstLower transformer -> transformer firstLower)
        |. Parser.symbol openCurlyBracketToken
        |. Parser.spaces
        |= lowercaseIdentifier
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed (\rest -> \firstLower -> RecordType (Just firstLower) rest)
                |. Parser.token pipeToken
                |. Parser.spaces
                |= PExtra.sequenceWithTrailing
                    { subParser =
                        recordKeyValue
                            (Parser.lazy (\() -> type_))
                            (Parser.symbol colonToken)
                    , separator = Parser.symbol commaToken
                    , spaces = Parser.spaces
                    }
            , Parser.succeed
                (\firstType rest ->
                    \firstLower -> RecordType Nothing (( firstLower, firstType ) :: rest)
                )
                |. Parser.symbol colonToken
                |. Parser.spaces
                |= Parser.lazy (\() -> type_)
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed identity
                        |. Parser.token commaToken
                        |. Parser.spaces
                        |= PExtra.sequenceWithTrailing
                            { subParser =
                                recordKeyValue
                                    (Parser.lazy (\() -> type_))
                                    (Parser.symbol colonToken)
                            , separator = Parser.symbol commaToken
                            , spaces = Parser.spaces
                            }
                    , Parser.succeed []
                    ]
            ]
        |. Parser.symbol closeCurlyBracketToken



-- Declaration


declaration : Parser Declaration
declaration =
    declarationWithExpressionParser expression


declarationWithExpressionParser : Parser Expression -> Parser Declaration
declarationWithExpressionParser parseExpression =
    Parser.inContext CDeclaration <|
        Parser.oneOf
            [ Parser.succeed (\name transformer -> transformer name)
                |= variableLiteral
                |= Parser.oneOf
                    [ Parser.inContext CFunctionDeclaration <|
                        Parser.succeed
                            (\( firstPattern, restPatterns ) declarations ->
                                \name ->
                                    FunctionDeclaration name firstPattern restPatterns declarations
                            )
                            -- TODO: Remove backtrackable?
                            |. Parser.backtrackable spacesAtLeastOne
                            |= PExtra.sequenceAtLeastOne
                                { subParser = pattern
                                , separator = spacesAtLeastOne
                                , spaces = Parser.succeed ()
                                }
                            |. Parser.spaces
                            |. Parser.symbol equalsToken
                            |. Parser.spaces
                            |= parseExpression
                    , Parser.inContext CValueDeclaration <|
                        Parser.succeed (\declarations -> \name -> ValueDeclaration name declarations)
                            |. Parser.spaces
                            |. Parser.symbol equalsToken
                            |. Parser.spaces
                            |= parseExpression
                    ]
            , Parser.inContext CValuePatternMatchDeclaration <|
                Parser.succeed ValuePatternMatchDeclaration
                    |= pattern
                    |. Parser.spaces
                    |. Parser.symbol equalsToken
                    |. Parser.spaces
                    |= parseExpression
            ]



-- Pattern --


pattern : Parser Pattern
pattern =
    Parser.inContext CPattern <|
        Parser.succeed (\pat transform -> transform pat)
            |= Parser.oneOf
                [ ctorPattern
                , patternTerm
                ]
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.backtrackable spacesAtLeastOne
                    |= Parser.oneOf
                        [ Parser.inContext CAliasPattern <|
                            Parser.succeed (\alias_ -> \pat -> AliasPattern pat alias_)
                                |. Parser.keyword asToken
                                |. spacesAtLeastOne
                                |= lowercaseIdentifier
                        , Parser.inContext CConsPattern <|
                            Parser.succeed (\rest -> \head -> ConsPattern head rest)
                                |. Parser.keyword consToken
                                |. spacesAtLeastOne
                                |= Parser.lazy (\() -> pattern)
                        ]
                , Parser.succeed identity
                ]


patternTerm : Parser Pattern
patternTerm =
    Parser.oneOf
        [ Parser.succeed AnythingPattern
            |. Parser.symbol underscoreToken
        , Parser.inContext CRecordPattern <|
            Parser.succeed RecordPattern
                |. Parser.symbol openCurlyBracketToken
                |. Parser.spaces
                |= PExtra.sequenceWithTrailing
                    { subParser = lowercaseIdentifier
                    , separator = Parser.symbol commaToken
                    , spaces = Parser.spaces
                    }
                |. Parser.symbol closeCurlyBracketToken
        , Parser.inContext CCtorPattern <|
            ctorPatternWithoutArgs
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
            |= uppercaseIdentifier
            |= Parser.oneOf
                [ Parser.succeed
                    (\( moduleNames, ctor ) ->
                        \firstModuleName ->
                            QualCtorPattern (ModuleName firstModuleName moduleNames) ctor []
                    )
                    |. Parser.token dotToken
                    |= Parser.loop [] qualifiedCtorPatternHelp
                , Parser.succeed (\modName -> CtorPattern modName [])
                ]
        )


ctorPattern : Parser Pattern
ctorPattern =
    Parser.inContext CCtorPattern
        (Parser.succeed (\modName transformer -> transformer modName)
            |= uppercaseIdentifier
            |= Parser.oneOf
                [ Parser.succeed
                    (\( moduleNames, ctor ) subPatterns ->
                        \firstModuleName ->
                            QualCtorPattern (ModuleName firstModuleName moduleNames) ctor subPatterns
                    )
                    |. Parser.token dotToken
                    |= Parser.loop [] qualifiedCtorPatternHelp
                    |. spacesAtLeastOne
                    |= PExtra.sequence
                        { start = emptyToken
                        , end = emptyToken
                        , item = Parser.lazy (\() -> patternTerm)

                        -- TODO: Remove backtrackable?
                        , separator = Parser.backtrackable spacesAtLeastOne
                        , spaces = Parser.succeed ()
                        , trailing = Parser.Forbidden
                        }
                , Parser.succeed (\subPatterns -> \modName -> CtorPattern modName subPatterns)
                    |= PExtra.sequenceWithTrailing
                        { subParser = Parser.lazy (\() -> patternTerm)
                        , separator = spacesAtLeastOne
                        , spaces = Parser.succeed ()
                        }
                ]
        )


qualifiedCtorPatternHelp :
    List UppercaseIdentifier
    -> Parser (Parser.Step (List UppercaseIdentifier) ( List UppercaseIdentifier, UppercaseIdentifier ))
qualifiedCtorPatternHelp uppercaseIdentifiers =
    Parser.succeed (\upperVar transformer -> transformer upperVar)
        |= uppercaseIdentifier
        |= Parser.oneOf
            [ Parser.token dotToken
                |> Parser.map (\() -> \upperVar -> Parser.Loop (upperVar :: uppercaseIdentifiers))
            , Parser.succeed
                (\upperVar -> Parser.Done ( List.reverse uppercaseIdentifiers, upperVar ))
            ]



-- Expression


type alias ExpressionState =
    { isInFunctionCall : Bool }


expression : Parser Expression
expression =
    expressionWithState { isInFunctionCall = False }


expressionWithState : ExpressionState -> Parser Expression
expressionWithState state =
    -- Parser.inContext CExpression
    Parser.getCol
        |> Parser.andThen
            (\column ->
                Parser.succeed
                    (\exp transform -> transform exp)
                    |. checkIndent
                    |= Parser.oneOf
                        [ Parser.inContext CNegateExpression <|
                            Parser.succeed NegateExpression
                                |. Parser.symbol negateToken
                                |= Parser.lazy (\() -> expressionWithState state)
                        , Parser.inContext CLambdaExpression <|
                            Parser.succeed
                                (\( firstPattern, restPatterns ) exp ->
                                    LambdaExpression firstPattern restPatterns exp
                                )
                                |. Parser.symbol backSlashToken
                                |= PExtra.sequenceAtLeastOne
                                    { subParser = pattern
                                    , separator = spacesAtLeastOne
                                    , spaces = Parser.succeed ()
                                    }
                                |. Parser.spaces
                                |. Parser.symbol arrowToken
                                |. Parser.spaces
                                |= Parser.lazy (\() -> expressionWithState state)
                        , Parser.succeed identity
                            |. Parser.symbol openCurlyBracketToken
                            |. Parser.spaces
                            |= Parser.oneOf
                                [ Parser.inContext CRecordExpression <|
                                    Parser.map RecordExpression
                                        (PExtra.sequenceWithTrailing
                                            { subParser =
                                                recordKeyValue
                                                    (Parser.lazy (\() -> expressionWithState state))
                                                    (Parser.symbol equalsToken)
                                            , separator = Parser.symbol commaToken
                                            , spaces = Parser.spaces
                                            }
                                        )
                                , Parser.inContext CUpdateExpression <|
                                    Parser.succeed
                                        (\var ( firstProperty, restProperties ) ->
                                            UpdateExpression var
                                                firstProperty
                                                restProperties
                                        )
                                        |= lowercaseIdentifier
                                        |. Parser.spaces
                                        |. Parser.symbol pipeToken
                                        |. Parser.spaces
                                        |= PExtra.sequenceAtLeastOne
                                            { subParser =
                                                recordKeyValue (Parser.lazy (\() -> expressionWithState state))
                                                    (Parser.symbol equalsToken)
                                            , separator = spacesAtLeastOne
                                            , spaces = Parser.succeed ()
                                            }
                                        |. Parser.spaces
                                ]
                            |. Parser.symbol closeCurlyBracketToken
                        , Parser.inContext CAccessorExpression <|
                            Parser.succeed AccessorExpression
                                |. Parser.symbol dotToken
                                |= lowercaseIdentifier
                        , qualifiedVariableExpression
                        , letExpression state
                        , caseExpression state
                        , ifExpression state
                        , charLiteral |> Parser.map CharExpression
                        , Parser.inContext CString <|
                            (stringLiteral |> Parser.map StringExpression)
                        , Parser.inContext CVariable <|
                            (variableLiteral |> Parser.map VarExpression)
                        , Parser.inContext CList <|
                            (listLiteral (Parser.lazy (\() -> expressionWithState state)) |> Parser.map ListExpression)
                        , unitTupleParensLiteral (Parser.lazy (\() -> expressionWithState state))
                            UnitExpression
                            TupleExpression
                        , Parser.inContext CNumber <|
                            numberLiteral IntExpression FloatExpression
                        ]
                    |= Parser.oneOf
                        [ Parser.inContext CAccessExpression <|
                            Parser.succeed (\prop -> \exp -> AccessExpression exp prop)
                                |. Parser.symbol dotToken
                                |= lowercaseIdentifier
                        , Parser.succeed (\op secondExp -> \firstExp -> BinOpCallExpression firstExp op secondExp)
                            -- TODO: Remove backtrackable?
                            |. Parser.backtrackable Parser.spaces
                            |= operator
                            |. Parser.spaces
                            |= Parser.lazy (\() -> expressionWithState state)
                        , Parser.inContext CCallExpression <|
                            if state.isInFunctionCall then
                                Parser.problem InternalCallExpressionProblem

                            else
                                Parser.succeed
                                    (\( firstArg, restArgs ) ->
                                        \exp -> CallExpression exp firstArg restArgs
                                    )
                                    -- TODO: Remove backtrackable?
                                    |. Parser.backtrackable spacesAtLeastOne
                                    |= Parser.withIndent column
                                        (PExtra.sequence
                                            { start = emptyToken
                                            , end = emptyToken
                                            , item =
                                                Parser.lazy
                                                    (\() -> expressionWithState { state | isInFunctionCall = True })

                                            -- TODO: Remove backtrackable?
                                            , separator = Parser.backtrackable spacesAtLeastOne
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
                        , Parser.succeed identity
                        ]
            )



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
            |= uppercaseIdentifier
            |. Parser.token dotToken
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


ifExpression : ExpressionState -> Parser Expression
ifExpression state =
    Parser.succeed IfExpression
        |. Parser.keyword ifToken
        |. spacesAtLeastOne
        |= Parser.lazy (\() -> expressionWithState state)
        |. spacesAtLeastOne
        |. Parser.keyword thenToken
        |. spacesAtLeastOne
        |= Parser.lazy (\() -> expressionWithState state)
        |. spacesAtLeastOne
        |. Parser.keyword elseToken
        |. spacesAtLeastOne
        |= Parser.lazy (\() -> expressionWithState state)



-- Case Expression --


caseExpression : ExpressionState -> Parser Expression
caseExpression state =
    Parser.succeed CaseExpression
        |. Parser.keyword caseToken
        |. spacesAtLeastOne
        |= pattern
        |. spacesAtLeastOne
        |. Parser.keyword ofToken
        |. spacesAtLeastOne
        |= Parser.loop [] (caseExpressionHelp state)


caseExpressionHelp :
    ExpressionState
    -> List ( Pattern, Expression )
    -> Parser (Parser.Step (List ( Pattern, Expression )) (List ( Pattern, Expression )))
caseExpressionHelp state items =
    Parser.oneOf
        [ Parser.succeed
            (\casePat caseExp -> Parser.Loop (( casePat, caseExp ) :: items))
            |= pattern
            |. Parser.spaces
            |. Parser.token arrowToken
            |. Parser.spaces
            |= Parser.lazy (\() -> expressionWithState state)
            |. spacesAtLeastOne
        , Parser.succeed (Parser.Done (List.reverse items))
        ]



-- Let Expression --


letExpression : ExpressionState -> Parser Expression
letExpression state =
    Parser.inContext CLetExpression
        (Parser.succeed LetExpression
            |. Parser.keyword letToken
            |. spacesAtLeastOne
            |= (Parser.inContext CLetExpressionDeclarations <|
                    Parser.loop [] (letExpressionHelp state)
               )
            |= (Parser.inContext CLetExpressionBody <|
                    Parser.succeed identity
                        -- No spacesAtLeastOne here because the space is parsed
                        -- after the last let declaration
                        |. Parser.keyword inToken
                        |. spacesAtLeastOne
                        |= Parser.lazy (\_ -> expression)
               )
        )


letExpressionHelp :
    ExpressionState
    -> List Declaration
    -> Parser (Parser.Step (List Declaration) (List Declaration))
letExpressionHelp state items =
    Parser.oneOf
        [ Parser.succeed (\nextItem -> Parser.Loop (nextItem :: items))
            |= declarationWithExpressionParser
                (Parser.lazy (\() -> expressionWithState state))
            |. spacesAtLeastOne
        , Parser.succeed (Parser.Done (List.reverse items))
        ]



-- Record Helpers --


recordKeyValue : Parser item -> Parser sep -> Parser ( LowercaseIdentifier, item )
recordKeyValue item separator =
    Parser.inContext CRecordKeyValue <|
        Parser.succeed Tuple.pair
            |= lowercaseIdentifier
            |. Parser.spaces
            |. separator
            |. Parser.spaces
            |= item
            |. Parser.spaces



-- Literals --


variableLiteral : Parser String
variableLiteral =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = keywordsAsSet
        , expecting = ExpectingVariable
        }


{-| TODO: Handle escapes
<https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm>
-}
charLiteral : Parser String
charLiteral =
    Parser.inContext CChar <|
        Parser.succeed identity
            |. Parser.symbol singleQuoteToken
            |= (Parser.chompIf (\_ -> True) ExpectingCharacter |> Parser.getChompedString)
            |. Parser.symbol singleQuoteToken


stringLiteral : Parser String
stringLiteral =
    Parser.succeed identity
        |. Parser.token doubleQuoteToken
        |= Parser.loop [] stringLiteralHelp


stringLiteralHelp : List String -> Parser (Parser.Step (List String) String)
stringLiteralHelp revChunks =
    Parser.oneOf
        [ Parser.succeed ()
            |. Parser.token backSlashToken
            |. Parser.chompIf (\_ -> True) ExpectingAny
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
        |. Parser.symbol openParenthesisToken
        |= Parser.oneOf
            [ Parser.succeed fromUnit
                |. Parser.symbol closeParenthesisToken
            , Parser.inContext CTupleParens <|
                Parser.succeed (\first transformer -> transformer first)
                    |. Parser.spaces
                    |= subParser
                    |. Parser.spaces
                    |= Parser.oneOf
                        [ Parser.succeed identity
                            |. Parser.symbol closeParenthesisToken
                        , Parser.succeed (\second rest -> \first -> fromTuple first second rest)
                            |. Parser.symbol commaToken
                            |. Parser.spaces
                            |= subParser
                            |. Parser.spaces
                            |= Parser.oneOf
                                [ Parser.succeed identity
                                    |. Parser.symbol commaToken
                                    |= Parser.sequence
                                        { start = emptyToken
                                        , end = emptyToken
                                        , item = subParser
                                        , separator = commaToken
                                        , spaces = Parser.spaces
                                        , trailing = Parser.Optional
                                        }
                                , Parser.succeed []
                                ]
                            |. Parser.symbol closeParenthesisToken
                        ]
            ]


listLiteral : Parser decodesTo -> Parser (List decodesTo)
listLiteral subParser =
    Parser.inContext CList <|
        Parser.succeed identity
            |. Parser.symbol openSquareBracketToken
            |. Parser.spaces
            |= Parser.succeed identity
            |= PExtra.sequenceWithTrailing
                { subParser = subParser
                , separator = Parser.symbol commaToken
                , spaces = Parser.spaces
                }
            |. Parser.spaces
            |. Parser.symbol closeSquareBracketToken



-- Tokens --


lowercaseIdentifier : Parser String
lowercaseIdentifier =
    Parser.succeed ()
        |. Parser.chompIf Char.isLower ExpectingLowerCharacter
        |. Parser.chompWhile identifierHelp
        |> Parser.getChompedString


uppercaseIdentifier : Parser String
uppercaseIdentifier =
    Parser.succeed ()
        |. Parser.chompIf Char.isUpper ExpectingUpperCharacter
        |. Parser.chompWhile identifierHelp
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



-- Parser Extras Applied --


spacesAtLeastOne : Parser ()
spacesAtLeastOne =
    PExtra.spacesAtLeastOne ExpectingSpaces



-- Indent Helpers --


checkIndent : Parser ()
checkIndent =
    Parser.succeed (\indent column -> indent <= column)
        |= Parser.getIndent
        |= Parser.getCol
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
        |= Parser.getPosition
        |= parser
        |= Parser.getPosition
