module Elm.AST exposing
    ( Alias(..)
    , Context(..)
    , ElmModule(..)
    , ExposedCustomTypeConstructors(..)
    , ExposedItem(..)
    , ExposingList(..)
    , Expression(..)
    , LowercaseIdentifier
    , ModuleDeclaration(..)
    , ModuleImport(..)
    , ModuleName(..)
    , Operator(..)
    , Pattern(..)
    , Problem(..)
    , Type(..)
    , TypeDeclaration(..)
    , UppercaseIdentifier
    , ValueDeclaration(..)
    )


type ElmModule
    = ElmModule (Maybe ModuleDeclaration) (List ModuleImport)


type ModuleDeclaration
    = ModuleDeclaration ModuleName ExposingList


type ModuleImport
    = ModuleImport ModuleName Alias ExposingList


type Alias
    = AliasPartial
    | Alias UppercaseIdentifier
    | AliasNone


type ExposingList
    = ExposingExplicit (List ExposedItem)
    | ExposingAll
    | ExposingNone
      -- Incremental
    | ExposingTrailing (List ExposedItem)
    | ExposingProblemTrailing (List ExposedItem) Problem
    | ExposingProblem (List ExposedItem) Problem


type ExposedItem
    = ExposedValue LowercaseIdentifier
    | ExposedType UppercaseIdentifier ExposedCustomTypeConstructors
    | ExposedOperator Operator


type ExposedCustomTypeConstructors
    = ExposedConstructors (List UppercaseIdentifier)
    | ExposedConstructorsDotDot
    | NoExposedConstructors


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
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual


type ModuleName
    = ModuleName UppercaseIdentifier (List UppercaseIdentifier)


type Type
    = LambdaType Type Type
    | VariableType LowercaseIdentifier
    | Type UppercaseIdentifier (List Type)
    | QualType ModuleName UppercaseIdentifier (List Type)
    | RecordType (Maybe LowercaseIdentifier) (List ( LowercaseIdentifier, Type ))
    | UnitType
    | TupleType Type Type (List Type)


type ValueDeclaration
    = ValueDeclaration LowercaseIdentifier Expression
    | FunctionDeclaration LowercaseIdentifier Pattern (List Pattern) Expression
    | PatternMatchDeclaration Pattern Expression


type TypeDeclaration
    = TypeAlias UppercaseIdentifier (List LowercaseIdentifier) Type
    | CustomType UppercaseIdentifier (List LowercaseIdentifier) (List ( UppercaseIdentifier, List Type ))


type Expression
    = VarExpression LowercaseIdentifier
    | ListExpression (List Expression)
    | RecordExpression (List ( LowercaseIdentifier, Expression ))
    | NegateExpression Expression
    | LambdaExpression Pattern (List Pattern) Expression
    | CallExpression Expression Expression (List Expression)
    | BinOpCallExpression Expression Operator Expression
    | LetExpression (List ValueDeclaration) Expression
    | AccessorExpression LowercaseIdentifier
    | AccessExpression Expression LowercaseIdentifier
    | UpdateExpression LowercaseIdentifier ( LowercaseIdentifier, Expression ) (List ( LowercaseIdentifier, Expression ))
    | CaseExpression Pattern (List ( Pattern, Expression ))
    | IfExpression Expression Expression Expression
    | TupleExpression Expression Expression (List Expression)
    | UnitExpression
    | IntExpression Int
    | FloatExpression Float
    | CharExpression String
    | StringExpression String
    | QualVarExpression ModuleName LowercaseIdentifier


type Pattern
    = AnythingPattern
    | LowerPattern LowercaseIdentifier
    | TuplePattern Pattern Pattern (List Pattern)
    | UnitPattern
    | RecordPattern (List LowercaseIdentifier)
    | ListPattern (List Pattern)
    | CharPattern String
    | StringPattern String
    | IntPattern Int
    | FloatPattern Float
    | CtorPattern UppercaseIdentifier (List Pattern)
    | ConsPattern Pattern Pattern
    | AliasPattern Pattern LowercaseIdentifier
    | QualCtorPattern ModuleName UppercaseIdentifier (List Pattern)


type alias LowercaseIdentifier =
    String


type alias UppercaseIdentifier =
    String


type alias Identifier =
    String



-- Parser types


type Context
    = CRecordKeyValue
    | CLambdaExpression
    | CRecordExpression
    | CUpdateExpression
    | CLetExpression
    | CLetExpressionDeclarations
    | CLetExpressionBody
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
    | CPatternMatchDeclaration
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
    | InternalQualifiedVarExpressionProblem
    | ExpectingAtLeastOneModuleName
    | ExpectingColon
