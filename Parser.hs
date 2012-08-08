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
pIdent :: Parser AST
pIdent = do
  f <- pLetter
  fs <- many (pLetter <|> digit)
  let i = f:fs
  guard . not $ any (== i) keywords
  return $ Ident i

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
    args <- try (sepBy pType (blanks >> char ',' >> blanks))
         <|> do
            argGroups <- sepBy pIdentListAndType (blanks >> char ',' >> blanks)
            let args = concatMap (\(as, t) -> map (\_ -> t) as) argGroups -- with names, so ungroup and grab just the types
            return args
    spaces
    char ')'
    blanks
    ret <- option TypeVoid pType
    return $ TypeFunction args ret


pIdentListAndType :: Parser ([String], Type)
pIdentListAndType = do
    idents <- sepBy pIdent (try $ blanks >> char ',' >> blanks)
    blanks
    t <- pType
    blanks
    return (map (\(Ident i) -> i) idents, t)


pTypeName :: Parser Type
pTypeName = do
  Ident i <- pIdent
  return $ TypeName i


pType :: Parser Type
pType = try (char '(' *> spaces *> pType <* spaces <* char ')') <|> try pTypeLit <|> pTypeName

