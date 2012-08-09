{
module Lexer where

import Numeric (readOct, readHex)
}

%wrapper "monad"

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
\n | \; { mkL (const LEOL) }

-- comments to be ignored
\/\/.* ;
\/\*"$any*\*\/ ;

-- string and character literals
\" @string* \"          { mkL LString }
\` [^`]* \`             { mkL LString }
\' ($graphical # [\'\\] | \\\' | @escape) \'  { mkL (LChar . head) }

-- integer literals
0 $octdigit+                { mkInt readOct }
$digit+                     { mkInt reads   }
0x $hexdigit+               { mkInt readHex }

-- boolean literals
true    { mkL (const $ LBool True) }
false   { mkL (const $ LBool False) }


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


-- identifiers
_           { keyword LPlaceholder }
@ident      { mkL LIdent }


-- operators

"+"|"-"|"*"|\/|"%"|"&"|\||"^"|"<<"|">>"|"&^"|"+="|"-="|"*="|"/="|"%="|"&="| \|= | "^=" | "<<=" | ">>=" | "&^=" | "&&" | \|\| | "<-" | "++" | "--" | "==" | "<" | ">" | "=" | "!" | "!=" | "<=" | ">=" | ":=" | "..." { mkL LOp }

-- bracketing
"(" { mkL (const LOpenP) }
")" { mkL (const LCloseP) }
"{" { mkL (const LOpenCB) }
"}" { mkL (const LCloseCB) }
"[" { mkL (const LOpenSB) }
"]" { mkL (const LCloseSB) }

":" { mkL (const LColon) }
"." { mkL (const LDot) }

eof { mkL (const LEOF) }

{

data Lexeme = L AlexPosn LexemeClass String
    deriving (Show)

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


mkL :: (String -> LexemeClass) -> AlexInput -> Int -> Alex Lexeme
mkL f (p,_,_,str) len = return $ L p (f s) s
    where s = take len str

mkInt :: ReadS Int -> AlexInput -> Int -> Alex Lexeme
mkInt f ai@(p,_,_,str) len = case f (take len str) of
    [(x,"")] -> mkL (const $ LInt x) ai len
    _ -> alexError "Broken integer literal"

keyword :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
keyword c = mkL (const c)


alexEOF = return $ L (AlexPn 0 0 0) LEOF ""

}


