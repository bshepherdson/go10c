module Main where

import GoLexer
import Parser

import Control.Monad
import Control.Applicative


main = do
    str <- getContents
    print $ parseGo (alexScanTokens str)


