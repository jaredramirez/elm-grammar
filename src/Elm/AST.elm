module Elm.AST exposing
    ( Alias(..)
    , Declaration(..)
    , Elm(..)
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
    , UppercaseIdentifier
    )


type Elm
    = Elm (Maybe ModuleDeclaration) (List ModuleImport)


type ModuleDeclaration
    = ModuleDeclaration ModuleName ExposingList
    | ModuleDeclarationPartial ModuleName


type ModuleImport
    = ModuleImport ModuleName Alias (Maybe ExposingList)
    | ModuleImportIncomplete


type Alias
    = AliasPartial
    | Alias UppercaseIdentifier
    | AliasNone


type ExposingList
    = ExposingList (List ExposedItem)
    | ExposingListDoubleDot


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
    | LessThan


type ModuleName
    = ModuleName UppercaseIdentifier (List UppercaseIdentifier)


type TopLevelDeclaration
    = Declaration Declaration
    | TypeAliasDeclaration UppercaseIdentifier (List LowercaseIdentifier) -- TODO: Type
    | CustomTypeDeclaration UppercaseIdentifier (List LowercaseIdentifier) -- TODO: Custom Type


type Declaration
    = ValueDeclaration LowercaseIdentifier Expression
    | FunctionDeclaration LowercaseIdentifier Pattern (List Pattern) Expression
    | ValuePatternMatchDeclaration Pattern Expression


type Expression
    = ExpressionStub
    | VarExpression LowercaseIdentifier
    | QualVarExpression ModuleName LowercaseIdentifier
    | ListExpression (List Expression)
    | RecordExpression (List ( LowercaseIdentifier, Expression ))
    | NegateExpression Expression
    | LambdaExpression Pattern (List Pattern) Expression
    | CallExpression Expression (List Expression)
    | LetExpression Declaration
    | AccessorExpression LowercaseIdentifier
    | UpdateExpression LowercaseIdentifier (List ( LowercaseIdentifier, Expression ))
    | TupleExpression Expression Expression
    | TripleExpression Expression Expression Expression
    | UnitExpression
    | IntExpression Int
    | FloatExpression Float
    | CharExpression String
    | StringExpression String


type Pattern
    = AnythingPattern
    | LowerPattern LowercaseIdentifier
    | TuplePattern Pattern Pattern
    | TriplePattern Pattern Pattern Pattern
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


type alias LowercaseIdentifier =
    String


type alias UppercaseIdentifier =
    String


type alias Identifier =
    String
