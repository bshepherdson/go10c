{
module Parser where

import Lexer
}

%name parseGo
%tokentype { LexemeClass }
%error { parseError }

%token
    eol         { LEOL }
    eof         { LEOF }
    ident       { LIdent $$ }
    intlit      { LInt $$ }
    stringlit   { LString $$ }
    charlit     { LChar $$ }
    boollit     { LBool $$ }
    break       { LBreak }
    case        { LCase }
    chan        { LChan }
    const       { LConst }
    continue    { LContinue }
    default     { LDefault }
    defer       { LDefer }
    else        { LElse }
    fallthrough { LFallthrough }
    for         { LFor }
    go          { LGo }
    goto        { LGoto }
    if          { LIf }
    import      { LImport }
    interface   { LInterface }
    map         { LMap }
    package     { LPackage }
    range       { LRange }
    return      { LReturn }
    select      { LSelect }
    struct      { LStruct }
    switch      { LSwitch }
    type        { LType }
    var         { LVar }
    OP          { LOpenP }
    CP          { LCloseP }
    OCB         { LOpenCB }
    CCB         { LCloseCB }
    OSB         { LOpenSB }
    CSB         { LCloseSB }
    colon       { LColon }
    dot         { LDot }
    '+'         { LOp "+" }
    '-'         { LOp "-" }
    '*'         { LOp "*" }
    '/'         { LOp "/" }
    '%'         { LOp "%" }
    '&'         { LOp "&" }
    '|'         { LOp "|" }
    '^'         { LOp "^" }
    '<<'        { LOp "<<" }
    '>>'        { LOp ">>" }
    '&^'        { LOp "&^" }
    '+='        { LOp "+=" }
    '-='        { LOp "-=" }
    '*='        { LOp "*=" }
    '/='        { LOp "/=" }
    '%='        { LOp "%=" }
    '&='        { LOp "&=" }
    '|='        { LOp "|=" }
    '^='        { LOp "^=" }
    '<<='       { LOp "<<=" }
    '>>='       { LOp ">>=" }
    '&^='       { LOp "&^=" }
    '&&'        { LOp "&&" }
    '||'        { LOp "||" }
    '<-'        { LOp "<-" }
    '++'        { LOp "++" }
    '--'        { LOp "--" }
    '=='        { LOp "==" }
    '<'         { LOp "<" }
    '>'         { LOp ">" }
    '='         { LOp "=" }
    '!'         { LOp "!" }
    '!='        { LOp "!=" }
    '<='        { LOp "<=" }
    '>='        { LOp ">=" }
    ':='        { LOp ":=" }
    '...'       { LOp "..." }
%%

Exp1 : Exp1 '+' Term    { BinOp "+" $1 $3 }
     | Exp1 '-' Term    { BinOp "-" $1 $3 }
     | Term             { $1 }

Term : Term '*' Factor  { BinOp "*" $1 $3 }
     | Term '/' Factor  { BinOp "/" $1 $3 }
     | Factor           { $1 }

Factor : intlit         { LitInt $1 }
       | boollit        { LitBool $1 }
       | charlit        { LitChar $1 }
       | stringlit      { LitString $1 }
       | ident dot ident{ QualVar (QualIdent $1 $3) }
       | ident          { Var $1 }
       | OP Exp1 CP     { $2 }


{

data LexemeClass = LEOL
                 | LEOF
                 | LIdent String
                 | LInt Int
                 | LString String
                 | LChar Char
                 | LBool Bool
                 | LBreak
                 | LCase
                 | LChan
                 | LConst
                 | LContinue
                 | LDefault
                 | LDefer
                 | LElse
                 | LFallthrough
                 | LFor
                 | LGo
                 | LGoto
                 | LIf
                 | LImport
                 | LInterface
                 | LMap
                 | LPackage
                 | LRange
                 | LReturn
                 | LSelect
                 | LStruct
                 | LSwitch
                 | LType
                 | LVar
                 | LOp String
                 | LOpenP
                 | LCloseP
                 | LOpenCB
                 | LCloseCB
                 | LOpenSB
                 | LCloseSB
                 | LColon
                 | LDot
    deriving (Show)

data QualIdent = QualIdent { package :: String, name :: String }
  deriving (Show)

data Expr = LitInt Int
          | LitBool Bool
          | LitChar Char
          | LitString String
          | LitStruct Type [(String, Expr)]
          | LitArray Type [Expr]
          | Var String
          | QualVar QualIdent
          | Selector Expr String
          | Index Expr Expr -- array expression and index expression
          | Call Expr [Expr] -- function expression and argument expressions
          | BuiltinCall String (Maybe Type) [Expr] -- name of builtin, maybe a type as the first arg, and a list of parameter expressions
          | Conversion Type Expr
          | UnOp String Expr -- unary operator and expression
          | BinOp String Expr Expr -- binary operator and operands
    deriving (Show)

data Type = TypeName String 
          | TypeArray Type
          | TypeStruct [(String, Type)] -- a struct with its parameters and their types.
          | TypePointer Type
          | TypeFunction [Type] Type -- a function giving its argument types and (singular, in my Go) return type.
          | TypeVoid -- nonexistent type used for functions without return values
  deriving (Eq, Show)

data Statement = StmtTypeDecl String Type
               | StmtVarDecl String Type (Maybe Expr)
               | StmtFunction String [(String, Type)] Type (Maybe [Statement])
               | StmtLabel String
               | StmtExpr Expr
               | StmtInc Expr
               | StmtDec Expr
               | StmtAssignment (Maybe String) Expr Expr -- the string is an operand, or Nothing for basic assignment
               | StmtEmpty
               | StmtIf Statement Expr [Statement] [Statement] -- the initializer, condition, block and else block. (the else block contains a single StmtIf, for an "else if")
               | StmtFor Statement Expr Statement [Statement] -- initializer, condition, increment, block
               | StmtSwitch Statement Expr [([Expr], [Statement])] [Statement] -- initializer, switching variable (can be omitted, then equiv to "true". parser fills this in),
                                                                               -- list of cases (comma-separated list of expr values, and bodies), default case (maybe empty)
               | StmtReturn (Maybe Expr)
               | StmtBreak String -- label
               | StmtContinue String -- label
               | StmtGoto String -- label
  deriving (Show)

parseError _ = error "Parse error"

}
