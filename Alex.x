{
module GoLexer where

import Numeric (readOct, readHex)
import Data.Char (chr)
import qualified Data.Map as M
}

%wrapper "basic"

$digit = 0-9
$octdigit = 0-7
$hexdigit = [0-9a-fA-F]

$alpha = [_a-zA-Z]

$blank = [\ \t\v\f\r]
$any = [.\n]

$graphical = [\ -\~]
$charesc = [abfnrtv\\] -- backslashable characters, except for quotes
@escape = \\ ($charesc | $octdigit{3} | x $hexdigit{2})
@string = $graphical # [\"\\] | " " | @escape | \\\" | $blank+

@ident = $alpha [$alpha $digit]*

tokens :-

$blank+ ; -- ignore non-newline whitespace
\n { const LNewline }
\; { const LSemi }

-- comments to be ignored
\/\/.* ;
-- \/\*"$any*\*\/ ;  -- disabled because it doesn't work and I can't find a way to do it without switching to the monad wrapper.

-- string and character literals
\" @string* \"          { LString . init . tail . escape stringEscapes } -- strip the leading and trailing quotes.
\` [^`]* \`             { LString . init . tail } -- likewise.
\' ($graphical # [\'\\] | \\\' | @escape) \'  { LChar . head . tail . escape charEscapes } -- first character inside the quotes

-- integer literals
0 $octdigit+                { mkInt readOct . tail }
0x $hexdigit+               { mkInt readHex . drop 2 }
$digit+                     { mkInt reads   }

-- boolean literals
true    { const $ LBool True }
false   { const $ LBool False }


-- keywords
break       { keyword LBreak }
case        { keyword LCase }
chan        { keyword LChan }
const       { keyword LConst }
continue    { keyword LContinue }
default     { keyword LDefault }
defer       { keyword LDefer }
else        { keyword LElse }
fallthrough { keyword LFallthrough }
for         { keyword LFor }
func        { keyword LFunc }
go          { keyword LGo }
goto        { keyword LGoto }
if          { keyword LIf }
import      { keyword LImport }
interface   { keyword LInterface }
map         { keyword LMap }
package     { keyword LPackage }
range       { keyword LRange }
return      { keyword LReturn }
select      { keyword LSelect }
struct      { keyword LStruct }
switch      { keyword LSwitch }
type        { keyword LType }
var         { keyword LVar }

-- builtins
new         { keyword LNew }
delete      { keyword LDelete }
panic       { keyword LPanic }


-- identifiers
@ident      { LIdent }


-- operators

"+"|"-"|"*"|\/|"%"|"&"|\||"^"|"<<"|">>"|"&^"|"+="|"-="|"*="|"/="|"%="|"&="| \|= | "^=" | "<<=" | ">>=" | "&^=" | "&&" | \|\| | "<-" | "++" | "--" | "==" | "<" | ">" | "=" | "!" | "!=" | "<=" | ">=" | ":=" | "..." { LOp }

-- bracketing
"(" { (const LOpenP) }
")" { (const LCloseP) }
"{" { (const LOpenCB) }
"}" { (const LCloseCB) }
"[" { (const LOpenSB) }
"]" { (const LCloseSB) }

":" { (const LColon) }
"." { (const LDot) }
"," { (const LComma) }

eof { (const LEOF) }

{

data Token = LNewline
           | LSemi
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
           | LFunc
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
           | LNew
           | LDelete
           | LPanic
           | LOp String
           | LOpenP
           | LCloseP
           | LOpenCB
           | LCloseCB
           | LOpenSB
           | LCloseSB
           | LColon
           | LDot
           | LComma
    deriving (Show)


mkInt :: ReadS Int -> String -> Token
mkInt f s = case f s of
    [(x,"")] -> LInt x
    _        -> error $ "Broken integer literal: '" ++ s ++ "'"

keyword = const

baseEscapes, charEscapes, stringEscapes :: M.Map Char Char
baseEscapes = M.fromList [('a', '\a'), ('b', '\b'), ('f', '\f'), ('n', '\n'), ('r', '\r'), ('t', '\t'), ('v', '\v'), ('\\', '\\')]
charEscapes = M.insert '\'' '\'' baseEscapes
stringEscapes = M.insert '"' '"' baseEscapes

-- collapses literal backslashes in the string into escape characters
escape :: M.Map Char Char -> String -> String
escape _ [] = []
escape m ('\\':a:b:c:rest) | a == 'x' = case readHex (b:c:[]) of
                                            [(x,[])] -> chr x : escape m rest
                                            _ -> error $ "Broken hex escape character: " ++ ['\\',a,b,c]
                           | a `elem` "012" = case readOct (a:b:c:[]) of
                                                  [(x,[])] -> chr x : escape m rest
                                                  _ -> error $ "Broken octal escape character: " ++ ['\\',a,b,c]
escape m ('\\':c:s) = case M.lookup c m of
    Just esc -> esc : escape m s
    Nothing  -> error $ "Illegal escape character: \\" ++ [c]
escape m (x:xs) = x : escape m xs

}


