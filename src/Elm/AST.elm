module Elm.AST exposing
    ( Alias(..)
    , Declaration(..)
    , Elm(..)
    , ExposedCustomTypeConstructors(..)
    , ExposedItem(..)
    , ExposingList(..)
    , Expression(..)
    , LowercaseIdentifier(..)
    , ModuleDeclaration(..)
    , ModuleImport(..)
    , ModuleName(..)
    , Operator(..)
    , Pattern(..)
    , UppercaseIdentifier(..)
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


type Declaration
    = ValueDeclaration LowercaseIdentifier Expression
    | TypeAliasDeclaration UppercaseIdentifier
    | CustomTypeDeclaration
    | PortAnnotation
    | OperatorConfig
    | InfixDeclaration


type Expression
    = Expression
    | IntExpression
    | FloatExpression


type Pattern
    = AnythingPattern
    | LowerPattern LowercaseIdentifier
    | TuplePattern Pattern Pattern (Maybe Pattern)
    | UnitPattern
    | RecordPattern (List LowercaseIdentifier)
    | ParenthesisPattern Pattern (Maybe LowercaseIdentifier)
    | ListPattern (List Pattern)
    | ConsPattern Pattern Pattern
    | CharPattern String
    | StringPattern String
    | IntPattern Int
    | FloatPattern Float
    | CtorPattern UppercaseIdentifier (List Pattern)


type LowercaseIdentifier
    = LowercaseIdentifier String


type UppercaseIdentifier
    = UppercaseIdentifier String
