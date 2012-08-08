module Parser where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

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
         | LitInt Int
         | LitChar Char
         | LitString String
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

data Expr = Expr
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

pLitInt :: Parser AST
pLitInt = do
  x <- option " " $ string "-"
  LitInt . (if x == "-" then negate else id) <$> (try pLitIntHex <|> try pLitIntOct <|> pLitIntDec)



pLitCharHex :: Parser AST
pLitCharHex = do
  string "\\x"
  hex <- count 2 hexDigit
  case readHex hex of
    [(x, "")] -> return $ LitChar (chr x)
    _ -> fail "Two-digit hex number required."

pLitCharOct :: Parser AST
pLitCharOct = do
  string "\\"
  oct <- count 3 octDigit
  case readOct oct of
    [(x, "")] -> return $ LitChar (chr x)
    _ -> fail "Three-digit octal number required."

pLitCharEscape :: M.Map Char Char -> Parser AST
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


pLitChar :: Parser AST
pLitChar = do
    char '\''
    c <- try pLitCharHex <|> try pLitCharOct <|> try (pLitCharEscape charEscapes) <|> (LitChar <$> noneOf "'\\\n\r\t\a\b\v\f")
    char '\''
    return c


-- includes newlines and all kinds of things, with no interpolation of backslashes or anything of the kind.
pLitStringRaw :: Parser AST
pLitStringRaw = do
    char '`'
    s <- many $ noneOf "`"
    char '`'
    return $ LitString $ filter (/= '\r') s -- stripping carriage returns is the only transformation performed

pLitStringInterp :: Parser AST
pLitStringInterp = do
    char '"'
    s <- many $ try pLitCharHex <|> try pLitCharOct <|> try (pLitCharEscape stringEscapes) <|> (LitChar <$> noneOf "\"\\\n\r\a\b\v\f")
    char '"'
    let s' = map (\(LitChar c) -> c) s -- turn it into a raw string
    return $ LitString s'

pLitString :: Parser AST
pLitString = try pLitStringRaw <|> pLitStringInterp


-- whitespace but not newlines
blanks :: Parser ()
blanks = many (oneOf " \t\r") >> return ()

blanksAround p = blanks *> p <* blanks
spacesAround p = spaces *> p <* spaces

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
    args <- try (sepBy pType (blanksAround $ char ','))
           <|> do
              argGroups <- sepBy pIdentListAndType (blanksAround $ char ',')
              let args = concatMap (\(as, t) -> map (\_ -> t) as) argGroups -- with names, so ungroup and grab just the types
              return args
    spaces
    char ')'
    blanks
    ret <- option TypeVoid pType
    return $ TypeFunction args ret


pIdentListAndType :: Parser ([String], Type)
pIdentListAndType = do
    idents <- sepBy pIdent (try $ blanksAround (char ','))
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
          defs_ <- option [] $ char '=' *> blanksAround (sepBy pExpr (blanksAround $ char ','))
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
      argGroups <- sepBy pIdentListAndType (blanksAround $ char ',')
      let args = concatMap (\(as, t) -> map (\a -> (a,t)) as) argGroups -- ungroup and pair each with its type
      return args
    spaces
    char ')'
    blanks
    ret <- try $ option TypeVoid pType
    blanks
    body <- try $ option Nothing $ Just <$> (char '{' *> spacesAround (sepBy pStatement eol) <* char '}')
    return [StmtFunction name args ret body]

pStatement = return $ StmtLabel "foo"

pExpr = return Expr

