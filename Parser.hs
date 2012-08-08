module Parser where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Expr

import Numeric

import Control.Monad
import Control.Applicative hiding (many, (<|>))

import Data.Functor.Identity (Identity)
import Data.Int
import Data.Word
import Data.Char (chr)

import qualified Data.Map as M

data AST = Ident String
         | Type Type
         | Block [Statement]
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
  deriving (Show)

data Expr = LitInt Int
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

data QualIdent = QualIdent { package :: String, name :: String }
  deriving (Show)

-- syntax!
-- Standard C expressions and precedence as far as I know, should be possible to use the Parsec helper for that.
-- C-style comments, line and block. Block comments containing a newline are treated as a newline in the parsing.
-- Newlines are significant.
-- Lexeme classes: identifiers, keywords, operators and delimiters, literals. White space is ignored except in so far as it separates tokens.
-- Top-level has a package declaration
-- Semicolons are optional in a variety of places, to be automatically inserted.
-- Identifiers start with a letter (includes _), there are some reserved ones.
-- 

-- utility functions


-- whitespace but not newlines
blanks :: Parser ()
blanks = many (oneOf " \t\r") >> return ()

blanksAround p = blanks *> p <* blanks
spacesAround p = spaces *> p <* spaces

blanksComma = blanksAround $ char ','
spacesComma = spacesAround $ char ','

-- an "end of line" in Go is either a semicolon or a newline
eol :: Parser ()
eol = try (blanks >> newline >> spaces) <|> (blanks >> char ';' >> spaces)


------------------------------------------------
-- TYPES
------------------------------------------------

pTypeLit :: Parser Type
pTypeLit = try pTypePointer <|> try pTypeArray <|> try pTypeStruct <|> pTypeFunction

pTypePointer :: Parser Type
pTypePointer = TypePointer <$> (char '*' *> blanks *> pType)

pTypeArray :: Parser Type
pTypeArray = TypeArray <$> (string "[]" *> blanks *> pType)

pTypeStruct :: Parser Type
pTypeStruct = do
    string "struct"
    blanks
    char '{'
    spaces
    fieldLines <- sepBy pIdentListAndType eol
    let fields = concatMap (\(is, t) -> map (\i -> (i,t)) is) fieldLines -- ungroup the fields
    char '}'
    return $ TypeStruct fields


pTypeFunction :: Parser Type
pTypeFunction = do
    string "func"
    blanks
    char '('
    spaces
    args <- try (sepBy pType blanksComma)
           <|> do
              argGroups <- sepBy pIdentListAndType blanksComma
              let args = concatMap (\(as, t) -> map (\_ -> t) as) argGroups -- with names, so ungroup and grab just the types
              return args
    spaces
    char ')'
    blanks
    ret <- option TypeVoid pType
    return $ TypeFunction args ret


pIdentListAndType :: Parser ([String], Type)
pIdentListAndType = do
    idents <- sepBy pIdent (try blanksComma)
    blanks
    t <- pType
    blanks
    return (idents, t)


pTypeName :: Parser Type
pTypeName = TypeName <$> pIdent

pType :: Parser Type
pType = try (char '(' *> spacesAround pType <* char ')') <|> try pTypeLit <|> pTypeName



----------------------------------
-- blocks and statements
----------------------------------

pTypeDecl :: Parser [Statement]
pTypeDecl = do
    string "type"
    blanks
    try (char '(' *> spacesAround (sepBy pTypeDeclInner eol) <* char ')') <|> fmap (:[]) pTypeDeclInner
  where pTypeDeclInner = StmtTypeDecl <$> (pIdent <* blanks) <*> pType


pVarDecl :: Parser [Statement]
pVarDecl = do
    string "var"
    blanks
    varss <- try (char '(' *> spacesAround (sepBy pVarDeclInner eol) <* char ')') <|> fmap (:[]) pVarDeclInner
    return $ concat varss
  where pVarDeclInner = do
          (vs, t) <- pIdentListAndType
          blanks
          defs_ <- option [] $ char '=' *> blanksAround (sepBy pExpr blanksComma )
          let defs = map Just defs_ ++ repeat Nothing -- fill out the rest of the list with Nothings.
          return $ zipWith (\v x -> StmtVarDecl v t x) vs defs


-- Returns a list of statements even though it's only one. makes it easier to concat everything.
pFuncDecl :: Parser [Statement]
pFuncDecl = do
    string "func"
    blanks
    name <- pIdent
    blanks
    char '('
    spaces
    args <- do
      argGroups <- sepBy pIdentListAndType blanksComma
      let args = concatMap (\(as, t) -> map (\a -> (a,t)) as) argGroups -- ungroup and pair each with its type
      return args
    spaces
    char ')'
    blanks
    ret <- try $ option TypeVoid pType
    blanks
    body <- try $ option Nothing (Just <$> (blanks *> pBlock))
    return [StmtFunction name args ret body]

pBlock :: Parser [Statement]
pBlock = concat <$> (char '{' *> spacesAround (sepBy pStatement eol) <* char '}')

pStatement :: Parser [Statement]
pStatement = try pTypeDecl <|> try pVarDecl <|> try pFuncDecl <|> pBlock



-----------------------------------------
-- expressions
-----------------------------------------

pLetter = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
pIdent :: Parser String
pIdent = do
  f <- pLetter
  fs <- many (pLetter <|> digit)
  let i = f:fs
  guard . not $ any (== i) keywords
  return i

keywords = ["break", "default", "func", "interface", "select",
            "case", "defer", "go", "map", "struct",
            "chan", "else", "goto", "package", "switch",
            "const", "fallthrough", "if", "range", "type",
            "continue", "for", "import", "return", "var"
            ]

pQualIdent :: Parser QualIdent
pQualIdent = QualIdent <$> pIdent <* char '.' <*> pIdent

pLitIntDec, pLitIntHex, pLitIntOct :: Parser Int
pLitIntDec = read <$> many1 digit
pLitIntHex = do
  i <- readHex <$> (string "0x" *> many1 hexDigit)
  case i of
    [(x,"")] -> return (x::Int)
    _        -> fail "Bad hex number"

pLitIntOct = do
  i <- readOct <$> (string "0" *> many1 octDigit)
  case i of
    [(x, "")] -> return x
    _         -> fail "Bad octal number"

pLitInt :: Parser Expr
pLitInt = do
  x <- option " " $ string "-"
  LitInt . (if x == "-" then negate else id) <$> (try pLitIntHex <|> try pLitIntOct <|> pLitIntDec)



pLitCharHex :: Parser Expr
pLitCharHex = do
  string "\\x"
  hex <- count 2 hexDigit
  case readHex hex of
    [(x, "")] -> return $ LitChar (chr x)
    _ -> fail "Two-digit hex number required."

pLitCharOct :: Parser Expr
pLitCharOct = do
  string "\\"
  oct <- count 3 octDigit
  case readOct oct of
    [(x, "")] -> return $ LitChar (chr x)
    _ -> fail "Three-digit octal number required."

pLitCharEscape :: M.Map Char Char -> Parser Expr
pLitCharEscape escapes = do
  char '\\'
  x <- anyChar
  case M.lookup x escapes of
    Just c  -> return $ LitChar c
    Nothing -> fail "Bad escape character"

baseEscapes, charEscapes, stringEscapes :: M.Map Char Char
baseEscapes = M.fromList [('a', '\a'), ('b', '\b'), ('f', '\f'), ('n', '\n'), ('r', '\r'), ('t', '\t'), ('v', '\v'), ('\\', '\\')]
charEscapes = M.insert '\'' '\'' baseEscapes
stringEscapes = M.insert '"' '"' baseEscapes


pLitChar :: Parser Expr
pLitChar = do
    char '\''
    c <- try pLitCharHex <|> try pLitCharOct <|> try (pLitCharEscape charEscapes) <|> (LitChar <$> noneOf "'\\\n\r\t\a\b\v\f")
    char '\''
    return c


-- includes newlines and all kinds of things, with no interpolation of backslashes or anything of the kind.
pLitStringRaw :: Parser Expr
pLitStringRaw = do
    char '`'
    s <- many $ noneOf "`"
    char '`'
    return $ LitString $ filter (/= '\r') s -- stripping carriage returns is the only transformation performed

pLitStringInterp :: Parser Expr
pLitStringInterp = do
    char '"'
    s <- many $ try pLitCharHex <|> try pLitCharOct <|> try (pLitCharEscape stringEscapes) <|> (LitChar <$> noneOf "\"\\\n\r\a\b\v\f")
    char '"'
    let s' = map (\(LitChar c) -> c) s -- turn it into a raw string
    return $ LitString s'

pLitString :: Parser Expr
pLitString = try pLitStringRaw <|> pLitStringInterp


pLitComposite :: Parser Expr
pLitComposite = do
    t <- pType
    blanks
    char '{'
    spaces
    lit <- case t of
        TypeStruct _ -> pLitCompStruct t
        TypeArray _  -> pLitCompArray t
        _            -> fail "Bad literal type"
    spaces
    char '}'
    return lit

pLitCompArray :: Type -> Parser Expr
pLitCompArray t = LitArray t <$> sepBy pExpr spacesComma

pLitCompStruct :: Type -> Parser Expr
pLitCompStruct t = LitStruct t <$> sepBy pElement spacesComma
  where pElement = (,) <$> pIdent <* char ':' <* spaces <*> pExpr

pLiteral :: Parser Expr
pLiteral = try pLitString <|> try pLitChar <|> try pLitInt <|> try pLitComposite

pOperand :: Parser Expr
pOperand = try pLiteral <|> try (QualVar <$> pQualIdent) <|> (Var <$> pIdent)


pConversion :: Parser Expr
pConversion = do
    t <- pType
    char '('
    x <- spacesAround pExpr
    char ')'
    return $ Conversion t x

pBuiltinCall :: Parser Expr
pBuiltinCall = do
    builtin <- choice [string "len", string "new"]
    blanks
    char '('
    mtype <- option Nothing (Just <$> pType <* spacesComma)
    args <- sepBy pExpr spacesComma
    spaces
    char ')'
    return $ BuiltinCall builtin mtype args

pSelector :: Parser Expr
pSelector = do
    x <- pPrimExpr
    char '.'
    n <- pIdent
    return $ Selector x n

pIndex :: Parser Expr
pIndex = do
    x <- pPrimExpr
    char '['
    i <- spacesAround pExpr
    char ']'
    return $ Index x i


pCall :: Parser Expr
pCall = do
    x <- pPrimExpr
    blanks
    char '('
    args <- spacesAround $ sepBy pExpr spacesComma
    char ')'
    return $ Call x args


pPrimExpr :: Parser Expr
pPrimExpr = try pOperand <|> try pConversion <|> try pBuiltinCall <|> try pSelector <|> try pIndex <|> pCall

prefix s = Prefix (UnOp <$> spacesAround (string s))
binary s = Infix (BinOp <$> spacesAround (string s)) AssocLeft

table = [map prefix ["+", "-", "!", "^", "*", "&"]
        ,map binary ["*", "/", "%", "<<", ">>", "&", "&^"]
        ,map binary ["+", "-", "|", "^"]
        ,map binary ["==", "!=", "<", "<=", ">", ">="]
        ,map binary ["&&"]
        ,map binary ["||"]
        ]


pBaseExpr :: Parser Expr
pBaseExpr = try (char '(' *> spacesAround pExpr <* char ')') <|> pPrimExpr 

pExpr :: Parser Expr
pExpr = buildExpressionParser table pBaseExpr

