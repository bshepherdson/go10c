
module AST where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Control.Applicative ((<*>), (<$>))


data AST = Ident String
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
pIdent :: Parser AST
pIdent = do
  f <- pLetter
  fs <- many (pLetter <|> digit)
  return $ Ident (f:fs)



