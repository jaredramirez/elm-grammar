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
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual


type ModuleName
    = ModuleName UppercaseIdentifier (List UppercaseIdentifier)


type Declaration
    = ValueDeclaration LowercaseIdentifier Expression
    | FunctionDeclaration LowercaseIdentifier Pattern (List Pattern) Expression
    | ValuePatternMatchDeclaration Pattern Expression


type Expression
    = VarExpression LowercaseIdentifier
    | ListExpression (List Expression)
    | RecordExpression (List ( LowercaseIdentifier, Expression ))
    | NegateExpression Expression
    | LambdaExpression Pattern (List Pattern) Expression
    | CallExpression Expression Expression (List Expression)
    | BinOpCallExpression Expression Operator Expression
    | LetExpression (List Declaration) Expression
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
