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
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Extra as PExtra
import Set


type alias Parser a =
    Parser.Parser Context Problem a


type Context
    = CRecordKeyValue
    | CExpression
    | CNegateExpression
    | CLambdaExpression
    | CRecordExpression
    | CUpdateExpression
    | CAccessorExpression
    | CLetExpression
    | CLetExpressionDeclarations
    | CLetExpressionBody
    | CAccessExpression
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
    | CUnitTupleTripleParens
    | CTupleTripleParens
    | CTupleTriple
    | CTriple
    | CValueFunctionDeclaration
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


type Problem
    = ExpectingModule
    | ExpectingPort
    | ExpectingExposing
    | ExpectingImport
    | ExpectingSpaces
    | ExpectingNewLine
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
    Parser.inContext CValueFunctionDeclaration <|
        Parser.succeed (\name toDeclaration exp -> toDeclaration name exp)
            |= lowercaseIdentifier
            -- TODO: spacesAtLeastOne?
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.inContext CFunctionDeclaration <|
                    Parser.succeed
                        (\( firstPattern, restPatterns ) ->
                            \name exp ->
                                FunctionDeclaration name
                                    firstPattern
                                    restPatterns
                                    exp
                        )
                        |= PExtra.sequenceAtLeastOne
                            { subParser = pattern
                            , separator = spacesAtLeastOne
                            , spaces = Parser.succeed ()
                            }
                , Parser.inContext CValueDeclaration <|
                    Parser.succeed (\name exp -> ValueDeclaration name exp)
                ]
            |. Parser.symbol equalsToken
            |. Parser.spaces
            -- TODO: Expression Parser
            |= Parser.succeed ExpressionStub


valuePatternMatchDeclaration : Parser Declaration
valuePatternMatchDeclaration =
    Parser.inContext CValuePatternMatchDeclaration <|
        Parser.succeed ValuePatternMatchDeclaration
            |= pattern
            |. Parser.spaces
            |. Parser.symbol equalsToken
            |. Parser.spaces
            -- TODO: Expression Parser
            |= Parser.succeed ExpressionStub



-- Pattern


pattern : Parser Pattern
pattern =
    Parser.inContext CPattern <|
        Parser.succeed (\pat transform -> transform pat)
            |= Parser.oneOf
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
                    Parser.succeed CtorPattern
                        |= uppercaseIdentifier
                        |= PExtra.sequenceWithTrailing
                            { subParser = Parser.lazy (\() -> pattern)
                            , separator = spacesAtLeastOne
                            , spaces = Parser.succeed ()
                            }
                , charLiteral |> Parser.map CharPattern
                , stringLiteral |> Parser.map StringPattern
                , variableLiteral |> Parser.map LowerPattern
                , numberLiteral IntPattern FloatPattern
                , listLiteral (Parser.lazy (\() -> pattern))
                    |> Parser.map ListPattern
                , unitTupleTripleParensLiteral (Parser.lazy (\() -> pattern))
                    UnitPattern
                    TuplePattern
                    TriplePattern
                ]
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. (spacesAtLeastOne |> Parser.backtrackable)
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



-- Expression


expression : Parser Expression
expression =
    Parser.inContext CExpression <|
        Parser.succeed (\exp transform -> transform exp)
            |= Parser.oneOf
                [ Parser.inContext CNegateExpression <|
                    Parser.succeed NegateExpression
                        |. Parser.symbol negateToken
                        |= Parser.lazy (\() -> expression)
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
                        |= Parser.lazy (\() -> expression)
                , Parser.succeed identity
                    |. Parser.symbol openCurlyBracketToken
                    |. Parser.spaces
                    |= Parser.oneOf
                        [ Parser.inContext CRecordExpression <|
                            Parser.map RecordExpression
                                (PExtra.sequenceWithTrailing
                                    { subParser = Parser.lazy (\() -> recordKeyValue)
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
                                    { subParser = Parser.lazy (\() -> recordKeyValue)
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
                , letDeclarations
                , charLiteral |> Parser.map CharExpression
                , Parser.inContext CString <|
                    (stringLiteral |> Parser.map StringExpression)
                , Parser.inContext CVariable <|
                    (variableLiteral |> Parser.map VarExpression)
                , Parser.inContext CList <|
                    (listLiteral (Parser.lazy (\() -> expression)) |> Parser.map ListExpression)
                , unitTupleTripleParensLiteral (Parser.lazy (\() -> expression))
                    UnitExpression
                    TupleExpression
                    TripleExpression
                , Parser.inContext CNumber <|
                    numberLiteral IntExpression FloatExpression
                ]
            |= Parser.oneOf
                [ Parser.inContext CAccessExpression <|
                    Parser.succeed (\prop -> \exp -> AccessExpression exp prop)
                        |. Parser.symbol dotToken
                        |= lowercaseIdentifier
                , Parser.succeed identity

                -- TODO: Call expression
                ]


letDeclarations : Parser Expression
letDeclarations =
    Parser.inContext CLetExpression
        (Parser.getIndent
            |> Parser.andThen
                (\indent ->
                    Parser.inContext CLetExpressionDeclarations <|
                        Parser.succeed identity
                            |. Parser.keyword letToken
                            |. spacesAtLeastOne
                            |= (Parser.loop [] letDeclarationsHelp
                                    |> Parser.withIndent (indent + 1)
                               )
                )
            |> Parser.andThen
                (\declarations ->
                    Parser.inContext CLetExpressionBody <|
                        Parser.succeed (LetExpression declarations)
                            -- No spacesAtLeastOne here because the space is parsed
                            -- after the last let declaration
                            |. Parser.keyword inToken
                            |. spacesAtLeastOne
                            |= Parser.lazy (\_ -> expression)
                )
        )


letDeclarationsHelp : List Declaration -> Parser (Parser.Step (List Declaration) (List Declaration))
letDeclarationsHelp items =
    Parser.oneOf
        [ Parser.succeed (\nextItem -> Parser.Loop (nextItem :: items))
            |= letDeclaration
            |. spacesAtLeastOne
        , Parser.succeed (Parser.Done (List.reverse items))
        ]


letDeclaration : Parser Declaration
letDeclaration =
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
                        |= Parser.lazy (\() -> expression)
                , Parser.inContext CValueDeclaration <|
                    Parser.succeed (\declarations -> \name -> ValueDeclaration name declarations)
                        |. Parser.spaces
                        |. Parser.symbol equalsToken
                        |. Parser.spaces
                        |= Parser.lazy (\() -> expression)
                ]
        , Parser.inContext CValuePatternMatchDeclaration <|
            Parser.succeed ValuePatternMatchDeclaration
                |= pattern
                |. Parser.spaces
                |. Parser.symbol equalsToken
                |. Parser.spaces
                |= Parser.lazy (\() -> expression)
        ]


recordKeyValue : Parser ( LowercaseIdentifier, Expression )
recordKeyValue =
    Parser.inContext CRecordKeyValue <|
        Parser.succeed Tuple.pair
            |= lowercaseIdentifier
            |. Parser.spaces
            |. Parser.symbol equalsToken
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
unitTupleTripleParensLiteral :
    Parser decodesTo
    -> decodesTo
    -> (decodesTo -> decodesTo -> decodesTo)
    -> (decodesTo -> decodesTo -> decodesTo -> decodesTo)
    -> Parser decodesTo
unitTupleTripleParensLiteral subParser fromUnit fromTuple fromTriple =
    Parser.inContext CUnitTupleTripleParens <|
        Parser.succeed identity
            |. Parser.symbol openParenthesisToken
            |= Parser.oneOf
                [ Parser.succeed fromUnit
                    |. Parser.symbol closeParenthesisToken
                , Parser.inContext CTupleTripleParens <|
                    Parser.succeed (\sub fromSub -> fromSub sub)
                        |. Parser.spaces
                        |= subParser
                        |. Parser.spaces
                        |= Parser.oneOf
                            [ Parser.succeed identity
                                |. Parser.symbol closeParenthesisToken
                            , Parser.inContext CTupleTriple <|
                                Parser.succeed
                                    (\second maybeThird ->
                                        \first ->
                                            case maybeThird of
                                                Nothing ->
                                                    fromTuple first second

                                                Just third ->
                                                    fromTriple first second third
                                    )
                                    |. Parser.symbol commaToken
                                    |. Parser.spaces
                                    |= subParser
                                    |. Parser.spaces
                                    |= Parser.oneOf
                                        [ Parser.inContext CTriple <|
                                            Parser.succeed Just
                                                |. Parser.symbol commaToken
                                                |. Parser.spaces
                                                |= subParser
                                                |. Parser.spaces
                                        , Parser.succeed Nothing
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


consToken : Parser.Token Problem
consToken =
    Parser.Token "::" ExpectingCons


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
