{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import GoLexer
import GoParser

import Control.Monad
import Control.Monad.State.Strict

import Control.Applicative
import Control.Arrow (first, second)

import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Char (ord, isUpper)
import Data.List
import Data.Either (rights)

import Control.Exception
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO
import System.Exit

-- alright, thoughts:
-- * need a symbol table to hold things we know about.
--     * each symbol has a type.
--     * there's a stack of symbol tables with the innermost scope at the top.
--     * functions and variables can coexist in the symbol table
--     * should the symbol table include tracking what's stored in each register?
-- * register handling
--     * what to do when we run out -- should be rare enough to get away with just failing to compile, at least until v1.1
--     * optimization: keeping things in registers persistently instead of always fetching them from the stack.
--         * that's tricky because it requires analyzing whole blocks and functions instead of just a line or expression at a time.
--         * leaving that aside for now.
--         * thanks to the [SP+next word] argument types, locals and stack arguments are probably easily accessed in one instruction.
--         * might require two passes, once to determine the registers that are needed and how widely variables are used
--         * register reservation mechanism?
--     * without the above optimization, only really necessary to hold arguments (A-C) and the intermediate results of expressions
--     * return values go in A, but that can wait until the end.
-- * labels
--     * functions: _go10c_package_function
--     * globals:   _go10c_package_varName
--     * jumps: _go10c_package_function_uniqueNumber
--     * that last means we need a global label number in a state monad.


type SymbolTable = M.Map QualIdent Type

data CompilerState = CS {
     symbols :: [SymbolTable]
    ,dirtyRegs :: [String] -- registers that have been dirtied in a function and need cleaning up.
    ,freeRegs :: [String] -- registers that are currently in use and can't be used now.
    ,strings :: [(String,String)] -- a collection of string literals to be written into the binary. pairs are (symbol name, content).
    ,types :: SymbolTable
    ,args :: [(QualIdent, Location)] -- a list of argument names and their locations.
    ,locals :: [(QualIdent, Location)] --  a list of local variables and their locations.
    ,globals :: [(QualIdent, Location)] -- a list of global variables and their locations.
    ,this :: String -- the name of the function currently being compiled, used for labels.
    ,unique :: Int -- an incrementing number to allow the construction of globally unique labels.
    ,packageName :: String -- the package name for the file being compiled
    } deriving (Show)

data Location = LocReg String
              | LocStack Int -- places above the frame pointer, J
              | LocLabel String -- in the given label
              | LocConstant Int
    deriving (Show)


newtype Compiler a = Compiler (StateT CompilerState IO a)
    deriving (Functor, Applicative, Monad, MonadState CompilerState, MonadIO)


runCompiler :: Compiler a -> CompilerState -> IO (a, CompilerState)
runCompiler (Compiler a) s = runStateT a s


-- ADT for the output opcodes, to allow for pattern matched optimizations
data Asm = SET Arg Arg
         | ADD Arg Arg
         | SUB Arg Arg
         | MUL Arg Arg
         | MLI Arg Arg
         | DIV Arg Arg
         | DVI Arg Arg
         | MOD Arg Arg
         | MDI Arg Arg
         | AND Arg Arg
         | BOR Arg Arg
         | XOR Arg Arg
         | SHR Arg Arg
         | ASR Arg Arg
         | SHL Arg Arg
         | IFB Arg Arg
         | IFC Arg Arg
         | IFE Arg Arg
         | IFN Arg Arg
         | IFG Arg Arg
         | IFA Arg Arg
         | IFL Arg Arg
         | IFU Arg Arg
         -- ADX, SBX, STI and STD are not used.
         | JSR Arg
         -- INT, IAG, IAS, RFI, IAQ, HWN, HWQ and HWI are not used.
         | HCF -- arg is unused, compiles a literal 0.
         | LabelDef String
         | DAT String -- DAT statement with the literal asm following
         | Comment Asm String -- statement with a comment attached. Useful for annotating output for debugging.

data Arg = Reg String -- used only for main registers A-J. May be clobbered.
         | RegP String -- used for main registers, DO NOT CLOBBER. Usually used for arguments.
         | PUSH
         | POP
         | PEEK
         | SP
         | PC
         | EX
         | Lit Int
         | AddrLit Int
         | AddrReg String -- main registers only!
         | AddrRegLit String Int -- likewise
         | AddrLabel String
         | Label String
    deriving (Eq)

data ExprResult = XR [Asm] Arg

instance Show Asm where
    show (JSR a) = "JSR " ++ show a
    show HCF = "HCF 0"
    show (LabelDef s) = ":" ++ s
    show (DAT s) = "DAT " ++ s
    show (SET b a) = "SET " ++ show b ++ ", " ++ show a
    show (ADD b a) = "ADD " ++ show b ++ ", " ++ show a
    show (SUB b a) = "SUB " ++ show b ++ ", " ++ show a
    show (MUL b a) = "MUL " ++ show b ++ ", " ++ show a
    show (MLI b a) = "MLI " ++ show b ++ ", " ++ show a
    show (DIV b a) = "DIV " ++ show b ++ ", " ++ show a
    show (DVI b a) = "DVI " ++ show b ++ ", " ++ show a
    show (MOD b a) = "MOD " ++ show b ++ ", " ++ show a
    show (MDI b a) = "MDI " ++ show b ++ ", " ++ show a
    show (AND b a) = "AND " ++ show b ++ ", " ++ show a
    show (BOR b a) = "BOR " ++ show b ++ ", " ++ show a
    show (XOR b a) = "XOR " ++ show b ++ ", " ++ show a
    show (SHR b a) = "SHR " ++ show b ++ ", " ++ show a
    show (ASR b a) = "ASR " ++ show b ++ ", " ++ show a
    show (SHL b a) = "SHL " ++ show b ++ ", " ++ show a
    show (IFB b a) = "IFB " ++ show b ++ ", " ++ show a
    show (IFC b a) = "IFC " ++ show b ++ ", " ++ show a
    show (IFE b a) = "IFE " ++ show b ++ ", " ++ show a
    show (IFN b a) = "IFN " ++ show b ++ ", " ++ show a
    show (IFG b a) = "IFG " ++ show b ++ ", " ++ show a
    show (IFA b a) = "IFA " ++ show b ++ ", " ++ show a
    show (IFL b a) = "IFL " ++ show b ++ ", " ++ show a
    show (IFU b a) = "IFU " ++ show b ++ ", " ++ show a
    show (Comment a c) = show a ++ " ; " ++ c

instance Show Arg where
    show (Reg s) = s
    show (RegP s) = s
    show PUSH = "PUSH"
    show POP = "POP"
    show PEEK = "PEEK"
    show SP = "SP"
    show PC = "PC"
    show EX = "EX"
    show (Lit n) = show n
    show (AddrLit n) = "[" ++ show n ++ "]"
    show (AddrReg r) = "[" ++ r ++ "]"
    show (AddrRegLit r n) = "[" ++ r ++ "+" ++ show n ++ "]"
    show (AddrLabel s) = "[" ++ s ++ "]"
    show (Label s) = s

-- shorthand for a binary "opcode" like IFG or ADD, used in expression helper functions.
type Opcode = Arg -> Arg -> Asm

doCompile :: SourceFile -> [String] -> IO [Asm]
doCompile (SourceFile thePackage imports statements_) libdirs = do
    -- handle the imports
    allImports <- evalStateT (importsClosure imports (if null libdirs then ["."] else libdirs)) []
    let statements = allImports ++ fixSelectors statements_
    let allGlobals = findVariables statements
        allConstants = findConstants statements
        allTypes = M.fromList $ findTypes statements ++ builtinTypes
        globalCode = flip concatMap allGlobals $ \(QualIdent mp i, t) -> [LabelDef (mkLabelInternal (fromMaybe thePackage mp) i)] ++ replicate (typeSizeInternal allTypes t) (DAT "0") -- include the label and enough space for the global
        -- a function called main becomes the start point.
        -- if we have a main, compile a jump to it as the first bit of code.
        -- if we don't, compile an HCF 0.
        allSymbols = findSymbols statements
        startCode = case lookup (QualIdent Nothing "main") allSymbols of
            Nothing -> [HCF]
            Just _  -> [SET PC (Label (mkLabelInternal thePackage "main"))]

        cs = CS {
            symbols = [ M.fromList allSymbols ],
            dirtyRegs = [],
            freeRegs = [],
            strings = [],
            types = allTypes,
            args = [],
            locals = [],
            globals = map (\(g@(QualIdent mp i), _) -> (g, LocLabel (mkLabelInternal (fromMaybe thePackage mp) i))) allGlobals ++ allConstants,
            this = "",
            unique = 1,
            packageName = thePackage
        }

    (compiledCode, finalState) <- runCompiler (concat <$> mapM compile statements) cs
    let stringsCode = concatMap (\(label, str) -> [LabelDef label, DAT $ "\"" ++ str ++ "\", 0"]) (strings finalState)
    return $ startCode ++ globalCode ++ stringsCode ++ compiledCode


type ImportMonad a = StateT [String] IO a

-- maintains a list of imports already imported as the state, returns all the top-level declarations from all the imports, in dependency order.
importsClosure :: [Import] -> [String] -> ImportMonad [Statement]
importsClosure [] _ = return []
importsClosure (Import malias file : rest) libdirs = do
    seen <- seenImport file
    exports <- case seen of
        True  -> return []
        False -> do
            fileAttempts <- liftIO $ mapM (\d -> loadFile (d ++ "/" ++ file ++ ".go")) libdirs -- try to look up the file in each directory in libdirs
            SourceFile package innerImports statements <- case rights fileAttempts of
                [] -> error $ "Could not find import named '" ++ file ++ "'. Search paths: " ++ unwords libdirs
                (s:_) -> return s
            modify (file:)
            recursiveExports <- importsClosure innerImports libdirs
            let exports = exportedStatements package statements
            return $ recursiveExports ++ exports
    laterExports <- importsClosure rest libdirs
    return $ exports ++ laterExports

--start here
seenImport :: String -> ImportMonad Bool
seenImport i = do
    seen <- get
    case filter (==i) seen of
        (_:_) -> return True
        []    -> return False


-- returns the subset of statements from a list of statements which are exportable
-- note that only functions and globals (and constants) with uppercase first letters are exported.
exportedStatements :: String -> [Statement] -> [Statement]
exportedStatements _ [] = []
exportedStatements pkg (StmtTypeDecl (QualIdent Nothing i) t : rest) | isUpper (head i) = StmtTypeDecl (QualIdent (Just pkg) i) (exportedType pkg t) : exportedStatements pkg rest
exportedStatements pkg (StmtVarDecl (QualIdent Nothing i)  t x : rest) | isUpper (head i) = StmtVarDecl  (QualIdent (Just pkg) i) t x : exportedStatements pkg rest
exportedStatements pkg (StmtConstDecl (QualIdent Nothing i)  mt x : rest) | isUpper (head i) = StmtConstDecl  (QualIdent (Just pkg) i) mt x : exportedStatements pkg rest
exportedStatements pkg (StmtShortVarDecl (QualIdent Nothing i) x : rest) | isUpper (head i) = StmtShortVarDecl (QualIdent (Just pkg) i) x : exportedStatements pkg rest
exportedStatements pkg (StmtFunction (QualIdent Nothing i) args ret _ : rest) | isUpper (head i) = StmtFunction (QualIdent (Just pkg) i) args ret Nothing : exportedStatements pkg rest
exportedStatements pkg (_:rest) = exportedStatements pkg rest

exportedType :: String -> Type -> Type
exportedType pkg (TypeStruct fields) = TypeStruct $ map (second $ exportedType pkg) fields
exportedType pkg (TypeName (QualIdent Nothing t)) | not (t `elem` ["int", "uint", "char", "string", "bool"]) = TypeName (QualIdent (Just pkg) t)
exportedType _ t = t

-- All built-in types: int, uint, char, string, bool.
builtinTypes :: [(QualIdent, Type)]
builtinTypes = [(QualIdent Nothing "string", TypeString)
               ,(QualIdent Nothing "int", TypeInt)
               ,(QualIdent Nothing "uint", TypeUint)
               ,(QualIdent Nothing "char", TypeChar)
               ,(QualIdent Nothing "bool", TypeBool)
               ]


-- returns the type of a symbol, dying with an error if it's not found.
lookupSymbol :: QualIdent -> Compiler Type
lookupSymbol i = do
    ms <- maybeLookupSymbol i
    case ms of
        Nothing -> error $ "unknown identifier: " ++ show i
        Just t  -> return t

-- returns the type of a symbol, or Nothing if it's not found.
maybeLookupSymbol :: QualIdent -> Compiler (Maybe Type)
maybeLookupSymbol i = do
    syms <- gets symbols
    let hits = map (M.lookup i) syms
    case filter isJust hits of
        (x:_) -> return x
        _     -> return Nothing


-- Looks up the location where a variable is stored. Locals trump args trump globals.
lookupLocation :: QualIdent -> Compiler Location
lookupLocation i = do
    s <- get
    let res = map (lookup i) [locals s, args s, globals s]
    case dropWhile isNothing res of
        [] -> error $ "Could not find a storage location for the variable " ++ show i
        (Just loc:_) -> return loc


-- Adds a symbol to the innermost scope.
addSymbol :: QualIdent -> Type -> Compiler ()
addSymbol i t = modify $ \s -> s { symbols = (M.insert i t (head (symbols s))) : tail (symbols s) }


-- Does a deep search to find all the local variables in a list of statements.
findLocals :: [Statement] -> [QualIdent]
findLocals [] = []
findLocals (StmtVarDecl i _ _ : rest) = i : findLocals rest
findLocals (StmtShortVarDecl i _ : rest) = i : findLocals rest
findLocals (StmtIf initializer _ ifblock elseblock : rest) = findLocals initializer ++ findLocals ifblock ++ findLocals elseblock ++ findLocals rest
findLocals (StmtFor initializer _ incrementer block : rest) = findLocals initializer ++ findLocals incrementer ++ findLocals block ++ findLocals rest
findLocals (StmtSwitch initializer _ cases : rest) = findLocals initializer ++ concatMap (findLocals.snd) cases ++ findLocals rest
findLocals (_:rest) = findLocals rest


-- finds variables and functions in a list of statements, intended for use on a whole file.
findSymbols :: [Statement] -> [(QualIdent, Type)]
findSymbols [] = []
findSymbols (StmtVarDecl s t _ : rest) = (s, t) : findSymbols rest
findSymbols (StmtConstDecl s t _ : rest) = (s, t) : findSymbols rest
findSymbols (StmtFunction name args ret _ : rest) = (name, TypeFunction (map snd args) ret) : findSymbols rest
findSymbols (_:rest) = findSymbols rest

-- finds variables with a shallow search. intended to find globals in the whole file.
findVariables :: [Statement] -> [(QualIdent, Type)]
findVariables [] = []
findVariables (StmtVarDecl i@(QualIdent Nothing _) t _ : rest) = (i, t) : findVariables rest
findVariables (_:rest) = findVariables rest

-- finds constants with a shallow search. intended to find global constants in the whole file.
findConstants :: [Statement] -> [(QualIdent, Location)]
findConstants [] = []
findConstants (StmtConstDecl i _ (Just (LitInt x)) : rest) = (i, LocConstant x) : findConstants rest
findConstants (StmtConstDecl i _ (Just _) : rest) = compileError $ "Only integer literals are supported as contants."
findConstants (StmtConstDecl _ _ Nothing : rest) = compileError $ "const declarations must include a value"
findConstants (_:rest) = findConstants rest


-- finds type declarations in a list of statements, intended for use on a whole file.
findTypes :: [Statement] -> [(QualIdent, Type)]
findTypes [] = []
findTypes (StmtTypeDecl s t : rest) = case t of
    (TypeStruct fields) -> if null (filter (isUpper.head.fst) fields) then (s, t) : findTypes rest else error "Struct fields must begin with a lowercase letter."
    _ -> (s, t) : findTypes rest
findTypes (_:rest) = findTypes rest


-- Finds every Var expression in the file, converting it to a Selector if the letter after the . is lowercase.
mapExpressions :: (Expr -> Expr) -> [Statement] -> [Statement]
mapExpressions f (StmtVarDecl i t (Just x) : rest) = StmtVarDecl i t (Just (f x)) : mapExpressions f rest
mapExpressions f (StmtShortVarDecl i x : rest) = StmtShortVarDecl i (f x) : mapExpressions f rest
mapExpressions f (StmtFunction i args ret (Just stmts) : rest) = StmtFunction i args ret (Just (mapExpressions f stmts)) : mapExpressions f rest
mapExpressions f (StmtExpr x : rest) = StmtExpr (f x) : mapExpressions f rest
mapExpressions f (StmtInc x : rest) = StmtInc (f x) : mapExpressions f rest
mapExpressions f (StmtDec x : rest) = StmtDec (f x) : mapExpressions f rest
mapExpressions f (StmtAssignment s lv rv : rest) = StmtAssignment s (f lv) (f rv) : mapExpressions f rest
mapExpressions f (StmtIf initializer condition ifbody elsebody : rest) =
    StmtIf (mapExpressions f initializer) (f condition) (mapExpressions f ifbody) (mapExpressions f elsebody) : mapExpressions f rest
mapExpressions f (StmtFor initializer condition incrementer body : rest) =
    StmtFor (mapExpressions f initializer) (f condition) (mapExpressions f incrementer) (mapExpressions f body) : mapExpressions f rest
mapExpressions f (StmtSwitch initializer switcher cases : rest) = StmtSwitch (mapExpressions f initializer) (f switcher) (map (\(xs, stmts) -> (map f xs, mapExpressions f stmts)) cases) : mapExpressions f rest
mapExpressions f (StmtReturn (Just x) : rest) = StmtReturn (Just (f x)) : mapExpressions f rest
mapExpressions f (s:rest) = s : mapExpressions f rest
mapExpressions _ [] = []


fixSelectors :: [Statement] -> [Statement]
fixSelectors = mapExpressions fixer
    where fixer (Var (QualIdent (Just p) n)) | not (isUpper (head n)) = Selector (Var (QualIdent Nothing p)) n
          fixer (LitComposite t vals) = LitComposite t (map (second fixer) vals)
          fixer (Selector x s) = Selector (fixer x) s
          fixer (Index x i) = Index (fixer x) (fixer i)
          fixer (Call f args) = Call (fixer f) (map fixer args)
          fixer (BuiltinCall t mt args) = BuiltinCall t mt (map fixer args)
          fixer (Conversion t x) = Conversion t (fixer x)
          fixer (UnOp t x) = UnOp t (fixer x)
          fixer (BinOp t l r) = BinOp t (fixer l) (fixer r)
          fixer x = x


-- returns a free register to be used in an expression.
getReg :: Compiler String
getReg = do
    rs <- gets freeRegs
    ds <- gets dirtyRegs
    case rs of
        [] -> error "Internal compiler error: Ran out of registers to allocate"
        (r:rest) -> do
            case filter (==r) ds of
                [] -> modify $ \s -> s { dirtyRegs = r : ds, freeRegs = rest }
                _  -> modify $ \s -> s { freeRegs = rest }
            return r


freeReg :: String -> Compiler ()
freeReg r = modify $ \s -> s { freeRegs = r : freeRegs s }

-- frees an argument for a sub-value, if it was returned in a register.
freeIfReg :: Arg -> Compiler ()
freeIfReg (Reg r) = freeReg r
freeIfReg _ = return ()


-- turns a function name into a compiler label
mkLabel :: QualIdent -> Compiler String
mkLabel (QualIdent (Just p) s) = return $ mkLabelInternal p s
mkLabel (QualIdent Nothing  s) = do
    pkg <- gets packageName
    return $ mkLabelInternal pkg s


mkLabelInternal :: String -> String -> String
mkLabelInternal p s = "_go10c__" ++ p ++ "_" ++ s


uniqueLabel :: Compiler String
uniqueLabel = do
    s <- get
    let label = this s ++ "_" ++ show (unique s)
    put s { unique = unique s + 1 }
    return label


compileError = error
typeError = error

-- typechecks an expression, returning its Type or printing a type error and exiting.
typeCheck :: Expr -> Compiler Type
typeCheck (LitInt _) = return TypeUint
typeCheck (LitChar _) = return TypeChar
typeCheck (LitBool _) = return TypeBool
typeCheck (LitString _) = return TypeString
typeCheck (LitComposite t _) = return t
typeCheck (Var (QualIdent Nothing "nil")) = return TypeNil
typeCheck (Var i) = lookupSymbol i
typeCheck (Selector x field) = do
    xt <- typeCheck x
    ut <- underlyingType xt
    case ut of
        (TypeStruct fields) -> case filter ((==field) . fst) fields of
            []      -> typeError $ "Struct does not have a field named '" ++ field ++ "'."
            [(_,t)] -> return t
            _       -> error $ "The impossible just happened! Struct with multiple fields named '" ++ field ++ "'."
        (TypePointer (TypeStruct fields)) -> case filter ((==field) . fst) fields of
            []      -> typeError $ "Struct does not have a field named '" ++ field ++ "'."
            [(_,t)] -> return t
            _       -> error $ "The impossible just happened! Struct with multiple fields named '" ++ field ++ "'."
        _ -> typeError $ "Attempt to select a field from a non-struct value " ++ show x ++ ", type " ++ show xt

typeCheck (Index x i) = do
    arrType <- underlyingType =<< typeCheck x
    indexType <- underlyingType =<< typeCheck i
    case (arrType, indexType) of
        (TypeArray t, TypeInt) -> return t
        (TypeArray t, TypeUint) -> return t
        (TypeArray _, t) -> typeError $ "Array index has type " ++ show t ++ ", not an integer."

        (TypeName t@(QualIdent Nothing "string"), TypeInt) -> return TypeChar
        (TypeName t@(QualIdent Nothing "string"), TypeUint) -> return TypeChar
        (TypeName (QualIdent Nothing "string"), t) -> typeError $ "Array index has type " ++ show t ++ ", not an integer."

        (t, _) -> typeError $ "Attempt to index non-array type " ++ show t ++ "."

typeCheck (Call f args) = do
    ft <- typeCheck f
    case ft of
        (TypeFunction argTypes returnType) -> check argTypes returnType
        (TypePointer (TypeFunction argTypes returnType)) -> check argTypes returnType
        _ -> typeError $ "Attempt to call a non-function value " ++ show f ++ " of type " ++ show ft
  where check argTypes returnType = do
            providedArgTypes <- mapM typeCheck args
            let zipped = zip argTypes providedArgTypes
            assigns <- sequence $ map (uncurry assignable) zipped
            let mismatched = map snd $ dropWhile fst $ zip assigns zipped
            case mismatched of
                [] -> return returnType -- function call is legal
                ((expected, actual):_) -> typeError $ "Function call (" ++ show f ++ ") expected an argument of type " ++ show expected ++ " but got " ++ show actual ++ "."


typeCheck (BuiltinCall LNew (Just (TypeArray t)) [n]) = do
    nt <- typeCheck n
    nut <- underlyingType nt
    when (nut /= TypeInt && nut /= TypeUint) $ typeError $ "new() called to create an array type with non-int size of type " ++ show nt
    return (TypeArray t) -- arrays are already pointers, we don't need to create a pointer to them.

typeCheck (BuiltinCall LNew (Just (TypeArray t)) _) = typeError $ "new() called to create an array but no size was provided"
typeCheck (BuiltinCall LNew (Just t) _) = return (TypePointer t) -- otherwise create a pointer to the provided type (TODO does this work for strings?)
typeCheck (BuiltinCall LNew Nothing [Var t]) = do
    ut <- underlyingType (TypeName t)
    case ut of
        (TypeArray _) -> typeError $ "new() called to create an array but no size was provided."
        ty -> return (TypePointer ty)

typeCheck (BuiltinCall LDelete Nothing [x]) = do
    t <- typeCheck x
    ut <- underlyingType t
    when (not (isPointer ut) && not (isArray ut)) $ typeError $ "delete() called with a value that is neither a pointer nor an array. It has type " ++ show t
    return TypeVoid -- delete returns nothing.

typeCheck (BuiltinCall LDelete Nothing _)   = typeError $ "delete() must have an argument provided"
typeCheck (BuiltinCall LPanic _ _) = return TypeVoid -- panic ignores its arguments and HCFs
typeCheck (Conversion t x) = do
    provided <- typeCheck x
    canConvert <- convertible provided t
    case canConvert of
        True -> return t
        False -> typeError $ "Cannot convert from " ++ show provided ++ " to " ++ show t ++ "."

typeCheck (UnOp (LOp op) x) = let
        unopInt = do
            xt <- typeCheck x
            case xt of
                TypeInt -> return TypeInt
                _       -> typeError $ "Unary " ++ op ++ " expects an int, found " ++ show xt
    in  case op of
            "+" -> unopInt
            "^" -> unopInt
            "-" -> case x of
                (LitInt _) -> return TypeInt
                t -> unopInt
            "!" -> do
                xt <- typeCheck x
                case xt of
                    TypeBool -> return TypeBool
                    _        -> typeError $ "Unary ! expects a bool, found " ++ show xt
            "*" -> do
                xt <- typeCheck x
                case xt of
                    TypePointer t -> return t
                    _             -> typeError $ "Unary * expects a pointer, found " ++ show xt
            "&" -> TypePointer <$> typeCheck x

typeCheck (BinOp (LOp op) left right) = do
    leftType <- underlyingType =<< typeCheck left
    rightType <- underlyingType =<< typeCheck right
    case op of
        "+" -> do
            case (leftType, rightType) of
                (TypeString, TypeString) -> return TypeString
                (TypeInt, TypeInt) -> return TypeInt
                (TypeUint, TypeUint) -> return TypeUint
                (TypeInt, TypeUint) -> return TypeUint
                (TypeUint, TypeInt) -> return TypeUint
                _ -> do
                    when (leftType /= TypeInt && leftType /= TypeString) $ typeError $ "Left argument of + must be int or string, but found " ++ show leftType
                    when (rightType /= TypeInt && rightType /= TypeString) $ typeError $ "right argument of + must be int or string, but found " ++ show rightType
                    when (leftType /= rightType) $ typeError $ "Left and right arguments of + do not match.\n\tLeft: " ++ show leftType ++ "\n\tRight: " ++ show rightType
                    typeError "Can't happen: Exhausted cases for typechecking binary +."
        "-" -> typeCheckBinOp "-" leftType TypeInt rightType TypeInt TypeInt
        "*" -> typeCheckBinOp "*" leftType TypeInt rightType TypeInt TypeInt
        "/" -> typeCheckBinOp "/" leftType TypeInt rightType TypeInt TypeInt
        "%" -> typeCheckBinOp "%" leftType TypeInt rightType TypeInt TypeInt
        "|" -> typeCheckBinOp "|" leftType TypeInt rightType TypeInt TypeInt
        "&" -> typeCheckBinOp "&" leftType TypeInt rightType TypeInt TypeInt
        "^" -> typeCheckBinOp "^" leftType TypeInt rightType TypeInt TypeInt
        "&^" -> typeCheckBinOp "&^" leftType TypeInt rightType TypeInt TypeInt
        "<<" -> typeCheckBinOp "<<" leftType TypeInt rightType TypeInt TypeInt
        ">>" -> typeCheckBinOp ">>" leftType TypeInt rightType TypeInt TypeInt

        "<" -> typeCheckBinOp "<" leftType TypeInt rightType TypeInt TypeBool
        ">" -> typeCheckBinOp ">" leftType TypeInt rightType TypeInt TypeBool
        "<=" -> typeCheckBinOp "<=" leftType TypeInt rightType TypeInt TypeBool
        ">=" -> typeCheckBinOp ">=" leftType TypeInt rightType TypeInt TypeBool

        "||" -> typeCheckBinOp "||" leftType TypeBool rightType TypeBool TypeBool
        "&&" -> typeCheckBinOp "&&" leftType TypeBool rightType TypeBool TypeBool

        "==" -> typeCheckEqOp "==" leftType rightType
        "!=" -> typeCheckEqOp "!=" leftType rightType


typeCheckBinOp :: String -> Type -> Type -> Type -> Type -> Type -> Compiler Type
typeCheckBinOp op actLeft expLeft actRight expRight retType = do
    leftAssign <- assignable actLeft expLeft
    rightAssign <- assignable actRight expRight
    when (not leftAssign) $ typeError $ "Left argument of " ++ op ++ " expected " ++ show expLeft ++ " but found " ++ show actLeft ++ "."
    when (not rightAssign) $ typeError $ "Right argument of " ++ op ++ " expected " ++ show expRight ++ " but found " ++ show actRight ++ "."
    return retType

typeCheckEqOp :: String -> Type -> Type -> Compiler Type
typeCheckEqOp op left right = do
    ass <- assignable left right
    when (not ass) $ typeError $ "Arguments to " ++ op ++ " have mismatched types:\n\tLeft: " ++ show left ++ "\n\tRight: " ++ show right
    return TypeBool



-- returns the size in words of a given type.
-- int, uint, bool and char are all 1 word. pointers, including strings and arrays, are also 1 word.
-- structs are the sum of the sizes of their fields
typeSizeInternal :: SymbolTable -> Type -> Int
typeSizeInternal _ TypeBool = 1
typeSizeInternal _ TypeInt  = 1
typeSizeInternal _ TypeUint = 1
typeSizeInternal _ TypeChar = 1
typeSizeInternal _ TypeString = 1
typeSizeInternal _ (TypePointer _) = 1
typeSizeInternal _ (TypeArray _) = 1
typeSizeInternal _ TypeVoid = error "Void type has no size."
typeSizeInternal syms (TypeStruct fields) = sum $ map (typeSizeInternal syms . snd) fields
typeSizeInternal syms (TypeName s) = case M.lookup s syms of
                                        Just t  -> typeSizeInternal syms t
                                        Nothing -> error $ "Cannot resolve type name " ++ show s

typeSize :: Type -> Compiler Int
typeSize t = do
    ts <- gets types
    return $ typeSizeInternal ts t


fieldOffset :: [(String, Type)] -> String -> Compiler Int
fieldOffset fields name = fieldOffset' fields name 0
    where fieldOffset' [] name _ = compileError $ "No field named '" ++ name ++ "' found on struct."
          fieldOffset' ((s, t):rest) name offset
            | s == name = return offset
            | otherwise = do
                size <- typeSize t
                fieldOffset' rest name (offset+size)


isPointer :: Type -> Bool
isPointer (TypePointer _) = True
isPointer _ = False

isArray :: Type -> Bool
isArray (TypeArray _) = True
isArray TypeString = True
isArray _ = False

-- return True if the first type can be converted into the second.
-- a value x of type V is convertible to a type T in any of the following cases:
-- * V is assignable to T
-- * V and T have the same underlying type
-- * V and T are (unnamed) pointer types, and their pointer base types are convertible
-- * V is []char and T is string
-- * V is string and T is []char
--
-- But we need to be more flexible here than in the original spec, to allow better assembly integration. So we allow:
-- * V is integral and T is any pointer or array type
convertible :: Type -> Type -> Compiler Bool
convertible l r = do
    assign <- assignable l r
    uL <- underlyingType l
    uR <- underlyingType r
    case (assign, uL == uR, uL, uR) of
        (True, _, _, _) -> return True -- assignable
        (False, True, _, _) -> return True -- identical underlying types
        (_, _, TypePointer p1, TypePointer p2) -> convertible p1 p2
        --(_, _, TypeInt, TypeString) -> return True -- we don't support this one.
        (_, _, TypeArray TypeChar, TypeString) -> return True
        (_, _, TypePointer TypeChar, TypeString) -> return True
        (_, _, TypeString, TypeArray TypeChar) -> return True
        (_, _, TypeString, TypePointer TypeChar) -> return True
        -- off-spec flexible integral->pointer/array casting.
        (_, _, TypeUint, TypePointer _) -> return True
        (_, _, TypeName (QualIdent Nothing "uint"), TypePointer _) -> return True
        (_, _, TypeUint, TypeArray _) -> return True
        (_, _, TypeName (QualIdent Nothing "uint"), TypeArray _) -> return True
        (_, _, TypeInt, TypePointer _) -> return True
        (_, _, TypeName (QualIdent Nothing "int"), TypePointer _) -> return True
        (_, _, TypeInt, TypeArray _) -> return True
        (_, _, TypeName (QualIdent Nothing "int"), TypeArray _) -> return True
        (_, _, TypeArray p1, TypePointer p2) -> convertible p1 p2
        (_, _, TypePointer p1, TypeArray p2) -> convertible p1 p2
        (_, _, TypeArray p1, TypeArray p2) -> convertible p1 p2
        _ -> return False

-- returns True if a value of the first type can be assigned to a variable of the second type
-- a value x of type V is assignable to a type T in any of these cases:
-- * V is identical to T
-- * V and T have identical underlying types and at least one of V or T is not a named type
-- * x is nil and T is a pointer, function, slice, map, channel or interface type.
assignable :: Type -> Type -> Compiler Bool
assignable from to
    | from == to = return True -- identical types, case 1.
    | otherwise  = do
        uFrom <- underlyingType from
        uTo   <- underlyingType to
        case (uFrom == uTo, from, to) of
            (True, TypeName _, TypeName _) -> return False -- at least one must be non-named
            (True, _, _) -> return True -- at least one is non-named and their base types are identical.
            _ -> case (uFrom, uTo) of
                (TypeInt, TypeUint) -> return True
                (TypeUint, TypeInt) -> return True
                (TypeNil, TypePointer _) -> return True
                (TypeString, TypePointer TypeChar) -> return True
                (TypePointer TypeChar, TypeString) -> return True
                (TypeArray TypeChar, TypeString) -> return True
                (TypeString, TypeArray TypeChar) -> return True
                _ -> return False


underlyingType :: Type -> Compiler Type
underlyingType TypeString = return $ TypeArray TypeChar
underlyingType (TypeName t) = lookupType t >>= underlyingType
underlyingType (TypePointer TypeChar) = return $ TypeArray TypeChar
underlyingType (TypePointer t) = TypePointer <$> underlyingType t
underlyingType (TypeArray t) = TypeArray <$> underlyingType t
underlyingType t = return t


-- look up a type in the compiler environment by name.
lookupType :: QualIdent -> Compiler Type
lookupType i = do
    ts <- gets types
    case M.lookup i ts of
        Nothing -> typeError $ "Unknown named type: " ++ show i
        Just t  -> return t


-- Compiles a Statement into a string of assembly code to perform it.
-- Should not add symbols to the table on a TypeDecl or VarDecl. They should have been added by the scan performed when beginning a block/file.
-- Initializers for variables should still be compiled in place (because they might depend on the values of other variables).
compile :: Statement -> Compiler [Asm]
compile (StmtTypeDecl name t) = return [] -- nothing to compile for typedecls.

-- note that this doesn't add the symbol, but rather expects it to already exist. this does compile initializers, though.
compile (StmtVarDecl name t Nothing) = addSymbol name t >> return [] -- nothing to do for a plain declaration
compile (StmtVarDecl name t (Just x)) = addSymbol name t >> setVar name x
compile (StmtConstDecl name t _) = addSymbol name t >> return [] -- nothing to do in code for constants.
compile (StmtShortVarDecl name x) = do
    t <- typeCheck x
    addSymbol name t
    setVar name x
compile (StmtFunction _ _ _ Nothing) = return []
compile (StmtFunction name args ret (Just body)) = do
    -- so a function. we need a new scope pushed, as well as a new set of locations for local variables and arguments.
    -- here's how the base pointer is handled. we use J as a base pointer. it points at the first local.
    -- the locals are at J+0, J+1, J+2, etc. the old value of J is stored at J+n, and any stack-passed args are in J+n+1, J+n+2, ...
    -- first, harvest the locals from the function body.
    addSymbol name (TypeFunction (map snd args) ret) -- add the function to the symbol table before we grab the state, to allow recursion.
    s <- get
    let allLocals = findLocals body -- [(QualIdent, Type)]
        localCount = length allLocals
        argLocations = [LocReg "A", LocReg "B", LocReg "C"] ++ map LocStack [localCount+2..] -- they're deeper than the locals, and the first three are in registers. +1 because the old PC return address is stored on top of them, and the base pointer is stored on top of that.
        myArgs = zip (map (QualIdent Nothing . fst) args) argLocations -- [(QualIdent, Location)], as necessary
        myLocalLocations = zip allLocals $ map LocStack [0..]
        mySymbols = M.fromList $ map (first (QualIdent Nothing)) args -- locals are not included in the symbol table, they'll be added as their definitions pass by.

    prefix <- mkLabel name

    let s' = s {
            locals = myLocalLocations,
            args = myArgs,
            dirtyRegs = [],
            freeRegs = drop (min 3 (length args)) ["A", "B", "C", "X", "Y", "Z", "I"],
            symbols = mySymbols : symbols s,
            this = prefix }


    put s'

    bodyCode <- concat <$> mapM compile body

    s'' <- get

    -- add preamble and postamble, saving and restoring the dirty registers and quitting.
    let preambleCode  = [LabelDef prefix,
                         SET PUSH (Reg "J"),                -- store the old value of J, the base pointer.
                         SUB SP (Lit localCount),           -- make room for the locals
                         SET (Reg "J") SP] ++               -- and set J to be the new base pointer, pointing at the first local.
                        map (SET PUSH . Reg) (dirtyRegs s'')
        postambleCode = [LabelDef (prefix ++ "_done")] ++
                        map (\r -> SET (Reg r) POP) (reverse (dirtyRegs s'')) ++
                        [ADD SP (Lit localCount),           -- remove the locals
                         SET (Reg "J") POP,                 -- restore the old base pointer.
                         SET PC POP]                        -- and return

    put s -- restore the original state, removing my locals, args and symbols.
    return $ preambleCode ++ bodyCode ++ postambleCode


compile (StmtLabel s) = return [LabelDef s] -- this isn't easy to do re: labeled jumps and breaks and crap. maybe make this a container instead of an ordinary statement.

compile (StmtExpr x@(Call _ _)) = do
    XR code arg <- compileExpr x
    freeIfReg arg
    return code
compile (StmtExpr x@(BuiltinCall _ _ _)) = do
    XR code arg <- compileExpr x
    freeIfReg arg
    return code
compile (StmtExpr x) = error $ "Suspicious code. Expression " ++ show x ++ " has no side effects."

compile (StmtInc x) = compileIncDec ADD x
compile (StmtDec x) = compileIncDec SUB x

compile (StmtAssignment Nothing lvalue rvalue) = do
    when (not $ isLvalue lvalue) $ compileError $ "Attempt to assign to non-lvalue " ++ show lvalue
    lt <- typeCheck lvalue
    rt <- typeCheck rvalue
    assign <- assignable rt lt
    when (not assign) $ typeError $ "Right side of assignment is not assignable to left side.\n\tLeft: " ++ show lt ++ ", " ++ show lvalue ++ "\n\tRight: " ++ show rt ++ ", " ++ show rvalue

    -- if we get down here, then this assignment is legal, so compile it.
    XR exprCode exprVal <- compileExpr rvalue
    case lvalue of
        (Var i) -> do
            locCode <- lookupLocation i >>= compileLocation
            freeIfReg exprVal
            return $ exprCode ++ [SET locCode exprVal]

        (Index x i) -> do
            XR xCode xVal <- compileExpr x
            XR iCode iVal <- compileExpr i
            (TypeArray elementType) <- underlyingType =<< typeCheck x
            size <- typeSize elementType

            case (size, xVal, iVal) of
                (_, Lit nx, Lit ni) -> do
                    freeIfReg exprVal
                    return $ exprCode ++ [SET (AddrLit (nx + size*ni)) exprVal]
                (_, Reg ar, Lit n)  -> do
                    freeReg ar
                    freeIfReg exprVal
                    return $ xCode ++ [ADD (Reg ar) (Lit (size*n)), SET (AddrReg ar) exprVal]
                (_, _, Reg ir) -> do
                    freeIfReg xVal
                    freeIfReg exprVal
                    freeReg ir
                    return $ exprCode ++ xCode ++ iCode ++
                            (if size > 1 then [MUL (Reg ir) (Lit size)] else []) ++
                            [ADD (Reg ir) xVal, SET (AddrReg ir) exprVal]
                (1, Reg ar, _) -> do
                    freeReg ar
                    freeIfReg exprVal
                    return $ exprCode ++ xCode ++ iCode ++ [ADD (Reg ar) iVal, SET (AddrReg ar) exprVal]
                (_, _, _) -> do
                    r <- getReg
                    freeIfReg xVal
                    freeIfReg exprVal
                    return $ exprCode ++ xCode ++ iCode ++ [SET (Reg r) iVal] ++
                            (if size > 1 then [MUL (Reg r) (Lit size)] else []) ++
                            [ADD (Reg r) xVal, SET (AddrReg r) exprVal]


        (Selector x s) -> do
            t <- typeCheck x
            ut <- underlyingType t
            fields <- case ut of
                (TypeStruct fields) -> return fields
                (TypePointer (TypeStruct fields)) -> return fields
                _ -> typeError $ "Attempt to select a field from a non-struct value of type " ++ show t
            offset <- fieldOffset fields s

            XR xCode xVal <- compileExpr x
            freeIfReg exprVal

            case xVal of
                Lit n -> return $ exprCode ++ xCode ++ [SET (AddrLit (n+offset)) exprVal]
                Reg r -> do
                    freeReg r
                    return $ exprCode ++ xCode ++ [SET (AddrRegLit r offset) exprVal]
                _ -> do
                    r <- getReg
                    freeReg r
                    return $ exprCode ++ xCode ++
                        [SET (Reg r) xVal, ADD (Reg r) (Lit offset), SET (AddrReg r) exprVal]

        _ -> error "Not implemented: Assigning to lvalues other than variables, array elements or struct fields."

-- TODO: Optimization: some ops, like += and -=, can be optimized by using ADD instead of computing and setting. Priority low, though, only a couple of instructions wasted.
compile (StmtAssignment (Just op) lvalue rvalue) = compile (StmtAssignment Nothing lvalue (BinOp (LOp op) lvalue rvalue))


compile (StmtIf initializer condition ifbody elsebody) = do
    -- the anatomy of an if-statement:
    -- it begins with the initializer. then the condition is computed. a jump is compiled that if the condition is false jumps to the beginning of the else block or the end
    -- at the end of the body a jump to the end is computed to skip over the else block.
    -- or is it easier to go in reverse?

    -- push the new scope before compiling the initializer, since any variables it defines are scoped in the if.
    modify $ \s -> s { symbols = M.empty : symbols s }
    initCode <- concat <$> mapM compile initializer

    ct <- typeCheck condition
    when (ct /= TypeBool) $ typeError $ "Condition of an if statement must have type bool, found " ++ show ct

    XR condCode condVal <- compileExpr condition
    freeIfReg condVal

    -- get a label prefix for this if, set 'this' appropriately.
    prefix <- uniqueLabel
    let jumpCode = [IFE condVal (Lit 0),
                    SET PC (Label $ if null elsebody then prefix ++ "_endif" else prefix ++ "_else")] -- jump if the condition is false, since we're jumping to the else block.
    ifCode <- concat <$> mapM compile ifbody
    let ifJumpCode = if null elsebody then [] else [SET PC (Label (prefix ++ "_endif"))] -- add a jump to the end of the if body if there's an else to jump over.

    elseCode <- case elsebody of
        [] -> return []
        _  -> do
            elseCode <- concat <$> mapM compile elsebody
            return $ [LabelDef (prefix ++ "_else")] ++ elseCode

    -- TODO I think variables declared in any part of the if are spreading to the rest of it. Mostly harmless.
    -- remove the scope
    modify $ \s -> s { symbols = tail (symbols s) }

    return $ initCode ++ condCode ++ jumpCode ++ ifCode ++ ifJumpCode ++ elseCode ++ [LabelDef (prefix ++ "_endif")]



compile (StmtFor initializer condition incrementer body) = do
    prefix <- uniqueLabel

    -- add the new scope before the initializer
    modify $ \s -> s { symbols = M.empty : symbols s }

    initCode <- concat <$> mapM compile initializer

    let topLabelCode = [LabelDef (prefix ++ "_top")]

    -- have to typecheck after the initializer is compiled, or a condition that references a newly initialized variable will error.
    ct <- typeCheck condition
    when (ct /= TypeBool) $ typeError $ "Loop condition must have type bool, found " ++ show ct


    XR condCode condVal <- compileExpr condition
    let condCheckCode = [IFE condVal (Lit 0), SET PC (Label (prefix ++ "_end"))]
    freeIfReg condVal

    bodyCode <- concat <$> mapM compile body
    incCode <- concat <$> mapM compile incrementer

    let topJumpCode = [SET PC (Label (prefix ++ "_top"))]

    let endLabelCode = [LabelDef (prefix ++ "_end")]

    modify $ \s -> s { symbols = tail (symbols s) } -- remove the new loop scope.

    return $ initCode ++ topLabelCode ++ condCode ++ condCheckCode ++ bodyCode ++ incCode ++ topJumpCode ++ endLabelCode


compile (StmtReturn mx) = do
    exprCode <- case mx of
        Nothing -> return []
        Just x  -> do
            XR code arg <- compileExpr x
            freeIfReg arg
            return $ code ++ [SET (Reg "A") arg]
    t <- gets this
    return $ exprCode ++ [SET PC (Label (t ++ "_done"))]


compile (StmtGoto label) = return [SET PC (Label label)]

compile s = compileError $ "Not implemented: " ++ show s


compileIncDec :: (Arg -> Arg -> Asm) -> Expr -> Compiler [Asm]
compileIncDec opcode (Var i) = do
    xt <- typeCheck (Var i)
    xut <- underlyingType xt
    when (xut /= TypeInt && xut /= TypeUint) $ typeError $ "Attempt to ++ or -- non-int type " ++ show xt
    locCode <- lookupLocation i >>= compileLocation
    return [opcode locCode (Lit 1)]


-- turns a Location into the assembly string representing it
compileLocation :: Location -> Compiler Arg
compileLocation (LocReg r)   = return $ RegP r -- use RegP to prevent compileExpr from overwriting this value.
compileLocation (LocStack n) = return $ AddrRegLit "J" n -- can't use PICK or SP, unknown levels of saved values piled on top. Use J, the frame pointer.
compileLocation (LocLabel s) = return $ AddrLabel s
compileLocation (LocConstant i) = return $ Lit i


-- compiles an expression, storing the result in the given register.
compileExpr :: Expr -> Compiler ExprResult
compileExpr (LitInt n) = return $ XR [] (Lit n)
compileExpr (LitBool b) = return $ XR [] (Lit (if b then 1 else 0))
compileExpr (LitChar c) = return $ XR [] (Lit (ord c))
compileExpr (LitString s) = do
    unique <- uniqueLabel
    modify $ \st -> st { strings = (unique, s) : strings st }
    return $ XR [] (Label unique)

compileExpr (LitComposite _ _) = compileError "Composite literals not implemented"

compileExpr (Var (QualIdent Nothing "nil")) = return $ XR [] (Lit 0)

compileExpr (Var i) = XR [] <$> (compileLocation =<< lookupLocation i)

compileExpr (Selector x s) = do
    t <- typeCheck x
    ut <- underlyingType t
    fields <- case ut of
        (TypeStruct fields) -> return fields
        (TypePointer (TypeStruct fields)) -> return fields
        _ -> typeError $ "Attempt to select field '" ++ s ++ "' from non-struct type " ++ show t
    offset <- fieldOffset fields s
    XR xCode xVal <- compileExpr x
    case xVal of
        Reg r -> return $ XR (xCode ++ [SET (Reg r) (AddrRegLit r offset)]) (Reg r)
        _ -> do
            r <- getReg
            return $ XR (xCode ++ [SET (Reg r) xVal, SET (Reg r) (AddrRegLit r offset)]) (Reg r)


compileExpr (Index arr ix) = do
    arrType <- typeCheck arr
    size <- typeSize arrType
    XR arrCode arrVal <- compileExpr arr
    XR ixCode ixVal <- compileExpr ix

    r <- getReg

    case (size, arrVal, ixVal) of
        (_, Reg ar, Lit n) -> return $ XR (arrCode ++ [SET (Reg ar) (AddrRegLit ar (size*n))]) (Reg ar)
        (_, Reg ar, Reg ir) -> do
            freeReg ir
            return $ XR (arrCode ++ ixCode ++ [
                    MUL (Reg ir) (Lit size),
                    ADD (Reg ar) (Reg ir),
                    SET (Reg ar) (AddrReg ar)])
                (Reg ar)
        (1, Reg ar, _) -> return $ XR (arrCode ++ ixCode ++ [
                    ADD (Reg ar) ixVal,
                    SET (Reg ar) (AddrReg ar)])
                (Reg ar)
        (1, _, _) -> do
            r <- getReg
            freeIfReg arrVal
            return $ XR (arrCode ++ ixCode ++ [
                    SET (Reg r) ixVal,
                    MUL (Reg r) (Lit size),
                    ADD (Reg r) arrVal,
                    SET (Reg r) (AddrReg r)])
                (Reg r)
        (_, _, Lit n) -> do
            r <- getReg
            return $ XR (arrCode ++ [
                    SET (Reg r) arrVal,
                    SET (Reg r) (AddrRegLit r (size*n))])
                (Reg r)
        (_, _, Reg ir) -> return $ XR (arrCode ++ ixCode ++ [
                    MUL (Reg ir) (Lit size),
                    ADD (Reg ir) arrVal,
                    SET (Reg ir) (AddrReg ir)])
                (Reg ir)
        (_, _, _) -> do
            iR <- getReg
            freeReg iR
            freeIfReg arrVal
            return $ XR (arrCode ++ ixCode ++ [
                    SET (Reg iR) ixVal,
                    MUL (Reg iR) (Lit size),
                    ADD (Reg iR) arrVal,
                    SET (Reg iR) (AddrReg iR)])
                (Reg iR)


compileExpr (Call f args) = do
    when (length args >= 4) $ error "Functions with more than 3 arguments are not implemented."

    -- I don't need the type, but I do need to typecheck.
    t <- typeCheck (Call f args)
    tf <- typeCheck f

    retArg <- case tf of
        TypeFunction _ TypeVoid -> return $ Lit 0
        TypePointer (TypeFunction _ TypeVoid) -> return $ Lit 0
        TypeFunction _ r -> Reg <$> getReg
        TypePointer (TypeFunction _ r) -> Reg <$> getReg
        _ -> typeError $ "Attempt to call non-function value of type: " ++ show tf


    -- If the return argument is not a register, ie. this is a void function, just send a fake "_" register so it'll save everything.
    (saveCode, restoreCode) <- saveRegsForCall (case retArg of Reg r -> r; _ -> "_")
    argCode <- concat <$> forM args (\arg -> do
            XR code val <- compileExpr arg
            freeIfReg val
            return $ code ++ [SET PUSH val])
    let popArgsCode = reverse $ zipWith (\r _ -> SET (Reg r) POP) ["A", "B", "C"] args

    jsrCode <- case (tf,f) of
        (TypeFunction _ _, Var name) -> do
            label <- mkLabel name
            return [JSR (Label label)]
        (TypePointer (TypeFunction _ _), x) -> do
            XR ptrCode ptrVal <- compileExpr x
            freeIfReg ptrVal
            return $ ptrCode ++ [JSR ptrVal]
        (t_, f_) -> error $ "Bad case: t = " ++ show t_ ++ ", f = " ++ show f_

    let returnCode = case retArg of
                         Reg r -> [SET (Reg r) (Reg "A")]
                         _     -> [] -- void, no return value to shuffle around.

    return $ XR (saveCode ++ argCode ++ popArgsCode ++ jsrCode ++ returnCode ++ restoreCode) retArg

-- creating an array with a length
compileExpr (BuiltinCall LNew (Just (TypeArray t)) (n:_)) = do
    r <- getReg
    (saveCode, restoreCode) <- saveRegsForCall r
    size <- typeSize t
    XR lenCode lenVal <- compileExpr n
    freeIfReg lenVal
    let argCode = case lenVal of
                      Lit n -> [SET (Reg "A") (Lit (size*n))]
                      _     -> [SET (Reg "A") lenVal, MUL (Reg "A") (Lit size)]
    return $ XR (saveCode ++ lenCode ++ argCode ++ [JSR (AddrLit 9)] ++
             (if r /= "A" then [SET (Reg r) (Reg "A")] else []) ++
             restoreCode) (Reg r)

compileExpr (BuiltinCall LNew (Just t) _) = do
    ut <- underlyingType t
    r <- getReg
    (saveCode, restoreCode) <- saveRegsForCall r
    size <- typeSize ut
    return $ XR (saveCode ++
             [SET (Reg "A") (Lit size),
              JSR (AddrLit 9)] ++ -- pointer is stored in A
             (if r /= "A" then [SET (Reg r) (Reg "A")] else []) ++
             restoreCode) (Reg r)

compileExpr (BuiltinCall LNew Nothing [Var t]) = do
    ut <- underlyingType (TypeName t)
    r <- getReg
    (saveCode, restoreCode) <- saveRegsForCall r
    size <- typeSize ut
    return $ XR (saveCode ++
             [SET (Reg "A") (Lit size),
              JSR (AddrLit 9)] ++ -- pointer is stored in A
             (if r /= "A" then [SET (Reg r) (Reg "A")] else []) ++
             restoreCode) (Reg r)

compileExpr (BuiltinCall LNew _ _) = compileError "new() called with no type as an argument"

compileExpr (BuiltinCall LDelete Nothing [x]) = do
    (saveCode, restoreCode) <- saveRegsForCall "_" -- placeholder
    -- the type has already been checked, so just call it.
    XR expCode expVal <- compileExpr x
    freeIfReg expVal
    return $ XR (saveCode ++ expCode ++ [SET (Reg "A") expVal, JSR (AddrLit 10)] ++ restoreCode) (Lit 0)

compileExpr (BuiltinCall LDelete _ _) = typeError "delete() must be called with exactly one pointer."

compileExpr (BuiltinCall LPanic _ _) = return $ XR [HCF] (Lit 0)


compileExpr (Conversion t x) = do
    xt <- typeCheck x
    canConvert <- convertible xt t
    when (not canConvert) $ typeError $ "Cannot convert from " ++ show xt ++ " to " ++ show t
    -- convertible types don't require any actual changes, there's no change to perform.
    compileExpr x


compileExpr (UnOp (LOp "+") x) = compileExpr x -- nothing to do for +

compileExpr (UnOp (LOp "-") (LitInt n)) = return $ XR [] (Lit (-n))
compileExpr (UnOp (LOp "-") x) = do
    ut <- typeCheck x >>= underlyingType
    when (ut /= TypeInt && ut /= TypeUint) $ typeError $ "Cannot negate non-(u)int value of type " ++ show ut
    XR exprCode exprVal <- compileExpr x
    case exprVal of
        Lit n -> return $ XR [] (Lit (-n))
        Reg r -> return $ XR (exprCode ++ [XOR (Reg r) (Lit 0xffff), ADD (Reg r) (Lit 1)]) (Reg r)
        _     -> do
            r <- getReg
            return $ XR (exprCode ++ [SET (Reg r) exprVal, XOR (Reg r) (Lit 0xffff), ADD (Reg r) (Lit 1)]) (Reg r)

compileExpr (UnOp (LOp "!") x) = do
    ut <- typeCheck x >>= underlyingType
    when (ut /= TypeBool) $ typeError $ "Unary ! can only be applied to bool values."
    XR exprCode exprVal <- compileExpr x
    case exprVal of
        (Lit 0) -> return $ XR [] (Lit 1)
        (Lit 1) -> return $ XR [] (Lit 0)
        (Lit n) -> compileError $ "Can't happen: Boolean literal with value " ++ show n
        (Reg r) -> return $ XR (exprCode ++ [XOR (Reg r) (Lit 1)]) (Reg r)
        _ -> do
            r <- getReg
            return $ XR (exprCode ++ [SET (Reg r) exprVal, XOR (Reg r) (Lit 1)]) (Reg r)

compileExpr (UnOp (LOp "^") x) = do
    ut <- typeCheck x >>= underlyingType
    when (ut /= TypeInt && ut /= TypeUint) $ typeError $ "Unary ^ can only be applied to int and uint values."
    XR exprCode exprVal <- compileExpr x
    case exprVal of
        Reg r -> return $ XR (exprCode ++ [XOR (Reg r) (Lit 0xffff)]) (Reg r)
        _ -> do
            r <- getReg
            return $ XR (exprCode ++ [SET (Reg r) exprVal, XOR (Reg r) (Lit 0xffff)]) (Reg r)

compileExpr (UnOp (LOp "*") x) = do
    t <- typeCheck x
    ut <- underlyingType t
    when (not (isPointer ut)) $ typeError $ "Attempt to dereference non-pointer type " ++ show t
    XR exprCode exprVal <- compileExpr x
    case exprVal of
        Lit n -> return $ XR [] (AddrLit n)
        Reg r -> return $ XR (exprCode ++ [SET (Reg r) (AddrReg r)]) (Reg r)
        _ -> do
            r <- getReg
            return $ XR (exprCode ++ [SET (Reg r) exprVal, SET (Reg r) (AddrReg r)]) (Reg r)

compileExpr (UnOp (LOp "&") x) = do
    when (not (isLvalue x)) $ typeError $ "Cannot take address of non-variable expression."
    case x of
        (Var i) -> do
            loc <- lookupLocation i
            case loc of
                LocReg _ -> typeError $ "Attempt to take address of value contained in a register " ++ show i
                LocConstant _ -> typeError $ "Attempt to take address of constant " ++ show i
                LocStack n -> return $ XR [] (AddrRegLit "J" n)
        (Index x i) -> do
            xt <- typeCheck x
            xut <- underlyingType xt
            when (not (isArray xut)) $ typeError $ "Attempt to index non-array value of type " ++ show xt ++ ", " ++ show xut
            let (TypeArray elementType) = xut
            it <- typeCheck i
            iut <- underlyingType it
            when (iut /= TypeInt && iut /= TypeUint) $ typeError $ "Index into array must be of integer type, but found " ++ show it

            size <- typeSize elementType

            XR xCode xVal <- compileExpr x
            XR iCode iVal <- compileExpr i
            case (size, xVal, iVal) of
                (_, Lit nx, Lit ni) -> return $ XR [] (Lit (nx + size*ni))
                (_, Reg ar, Lit n)  -> return $ XR (xCode ++ [ADD (Reg ar) (Lit (size*n))]) (Reg ar)
                (_, _, Reg ir) -> do
                    freeIfReg xVal
                    return $ XR (xCode ++ iCode ++
                            (if size > 1 then [MUL (Reg ir) (Lit size)] else []) ++
                            [ADD (Reg ir) xVal])
                        (Reg ir)
                (1, Reg ar, _) -> return $ XR (xCode ++ iCode ++ [ADD (Reg ar) iVal]) (Reg ar)
                (_, _, _) -> do
                    r <- getReg
                    freeIfReg xVal
                    return $ XR (xCode ++ iCode ++ [SET (Reg r) iVal] ++
                            (if size > 1 then [MUL (Reg r) (Lit size)] else []) ++
                            [ADD (Reg r) xVal])
                        (Reg r)

        (Selector x s) -> do
            t <- typeCheck x
            ut <- underlyingType t
            fields <- case ut of
                (TypeStruct fields) -> return fields
                _ -> typeError $ "Attempt to access fields of non-struct value. Actual type is " ++ show t
            offset <- fieldOffset fields s
            XR xCode xVal <- compileExpr x
            case xVal of
                Lit n -> return $ XR [] (Lit (n+offset))
                Reg r -> return $ XR (xCode ++ [ADD (Reg r) (Lit offset)]) (Reg r)
                _ -> do
                    r <- getReg
                    return $ XR (xCode ++ [SET (Reg r) xVal, ADD (Reg r) (Lit offset)]) (Reg r)

        x -> typeError "Cannot take the address of this value."


compileExpr (BinOp (LOp "+") left right) = compileIntegralBinOp ADD ADD "+" True  left right
compileExpr (BinOp (LOp "-") left right) = compileIntegralBinOp SUB SUB "-" False left right
compileExpr (BinOp (LOp "*") left right) = compileIntegralBinOp MUL MLI "*" True  left right
compileExpr (BinOp (LOp "/") left right) = compileIntegralBinOp DIV DVI "/" False left right
compileExpr (BinOp (LOp "%") left right) = compileIntegralBinOp MOD MDI "%" False left right
compileExpr (BinOp (LOp "|") left right) = compileIntegralBinOp BOR BOR "|" True  left right
compileExpr (BinOp (LOp "^") left right) = compileIntegralBinOp XOR XOR "^" True  left right
compileExpr (BinOp (LOp "&") left right) = compileIntegralBinOp AND AND "&" True  left right
compileExpr (BinOp (LOp "<<") left right) = compileIntegralBinOp SHL SHL "<<" False left right
compileExpr (BinOp (LOp ">>") left right) = compileIntegralBinOp SHR ASR ">>" False left right

compileExpr (BinOp (LOp "==") left right) = compileEqBinOp IFE "==" left right
compileExpr (BinOp (LOp "!=") left right) = compileEqBinOp IFN "!=" left right

compileExpr (BinOp (LOp "||") left right) = compileLogicalBinOp "||" left right
compileExpr (BinOp (LOp "&&") left right) = compileLogicalBinOp "&&" left right

compileExpr (BinOp (LOp ">") left right) = compileComparisonOp IFG IFA ">" id left right
compileExpr (BinOp (LOp "<") left right) = compileComparisonOp IFL IFU "<" id left right
compileExpr (BinOp (LOp ">=") left right) = compileComparisonOp IFL IFU ">=" swap left right -- >= is < with the arguments swapped
compileExpr (BinOp (LOp "<=") left right) = compileComparisonOp IFG IFA "<=" swap left right -- <= is > with the arguments swapped

compileExpr x = compileError $ "Unknown expression: " ++ show x


-- helper function for integral type binary operations like +, -, | and <<
compileIntegralBinOp :: Opcode -> Opcode -> String -> Bool -> Expr -> Expr -> Compiler ExprResult
compileIntegralBinOp unsignedOp signedOp opName commutes left right = do
    [leftType, rightType] <- mapM (underlyingType <=< typeCheck) [left, right]
    op <- case (leftType, rightType) of
        (TypeInt, TypeInt) -> return signedOp
        (TypeUint, TypeUint) -> return unsignedOp
        (TypeUint, TypeInt) -> return unsignedOp
        (TypeInt, TypeUint) -> return unsignedOp
        (TypeChar, TypeChar) -> return unsignedOp
        _ -> typeError $ opName ++ " can only be used on matching integral or char types, not " ++ show leftType ++ " and " ++ show rightType

    XR leftCode leftVal <- compileExpr left
    XR rightCode rightVal <- compileExpr right
    case (leftVal, rightVal) of
        (Lit l, Lit r) -> return $ XR [] (Lit (l+r))
        (Reg rl, Reg rr) -> do
            freeReg rr
            return $ XR (leftCode ++ rightCode ++ [op (Reg rl) (Reg rr)]) (Reg rl)
        (Reg r, _) -> return $ XR (leftCode ++ [op (Reg r) rightVal]) (Reg r)
        (_, Reg r) | commutes -> return $ XR (rightCode ++ [op (Reg r) leftVal]) (Reg r)
        (_, _) -> do
            r <- getReg
            freeIfReg rightVal
            return $ XR (leftCode ++ rightCode ++ [SET (Reg r) leftVal, op (Reg r) rightVal]) (Reg r)


compileEqBinOp :: Opcode -> String -> Expr -> Expr -> Compiler ExprResult
compileEqBinOp opCode opName left right = do
    [leftType, rightType] <- mapM (underlyingType <=< typeCheck) [left, right]
    ass <- assignable leftType rightType
    when (not ass) $ typeError $ opName ++ " can only be used on matching integral or char types, not " ++ show leftType ++ " and " ++ show rightType

    XR leftCode leftVal <- compileExpr left
    XR rightCode rightVal <- compileExpr right

    freeIfReg leftVal
    freeIfReg rightVal
    r <- getReg
    return $ XR (leftCode ++ rightCode ++ [
            SET EX (Lit 0),
            opCode leftVal rightVal,
            SET EX (Lit 1),
            SET (Reg r) EX])
        (Reg r)


-- These operations do not commute.
-- The right-hand side must not be computed when the left side allows short-circuiting.
compileLogicalBinOp :: String -> Expr -> Expr -> Compiler ExprResult
compileLogicalBinOp opName left right = do
    [leftType, rightType] <- mapM (underlyingType <=< typeCheck) [left, right]
    when (leftType /= TypeBool) $ typeError $ opName ++ " can only be applied to boolean values, but left argument has type " ++ show leftType
    when (rightType /= TypeBool) $ typeError $ opName ++ " can only be applied to boolean values, but right argument has type " ++ show rightType

    XR leftCode leftVal <- compileExpr left
    freeIfReg leftVal -- free this here, because it can be reused in the right-hand side.
    XR rightCode rightVal <- compileExpr right

    -- reuse the rightVal register if possible
    r <- case (leftVal, rightVal) of
            (_, Reg r) -> return r
            _ -> getReg
    freeReg r

    label <- (++ "_logic_op_end") <$> uniqueLabel

    -- This is the value in case of short-circuiting.
    let shortCircuitVal = if opName == "||" then Lit 1 else Lit 0

    return $ XR (leftCode ++ [
            SET (Reg r) shortCircuitVal,
            IFE leftVal shortCircuitVal,
            SET PC (Label label)] ++
            rightCode ++ [
            SET (Reg r) rightVal]) -- If the rightVal was returned in Reg r, this will be optimized out later.
        (Reg r)


-- helper function for comparison operators >, <, <=, >=
compileComparisonOp :: Opcode -> Opcode -> String -> ((Arg, Arg) -> (Arg, Arg)) -> Expr -> Expr -> Compiler ExprResult
compileComparisonOp unsignedOp signedOp opName switchOps left right = do
    [leftType, rightType] <- mapM (underlyingType <=< typeCheck) [left, right]
    op <- case (leftType, rightType) of
        (TypeInt, TypeInt) -> return signedOp
        (TypeUint, TypeUint) -> return unsignedOp
        (TypeInt, TypeUint) -> return unsignedOp
        (TypeUint, TypeInt) -> return unsignedOp
        (TypeChar, TypeChar) -> return unsignedOp
        _ -> typeError $ opName ++ " can only be used on matching integral or char types, not " ++ show leftType ++ " and " ++ show rightType

    XR leftCode leftVal_ <- compileExpr left
    XR rightCode rightVal_ <- compileExpr right

    let (leftVal, rightVal) = switchOps (leftVal_, rightVal_)

    r <- case (leftVal, rightVal) of
        (_, Reg r) -> freeIfReg leftVal >> return r
        (Reg r, _) -> return r
        (_, _)     -> getReg

    return $ XR (leftCode ++ rightCode ++ [
            SET EX (Lit 0),
            op leftVal rightVal,
            SET EX (Lit 1),
            SET (Reg r) EX]) (Reg r)


swap :: (a, b) -> (b, a)
swap (a,b) = (b,a)

-- returns the registers to be saved before making a function call.
-- saves A, B, and C, unless they are unused or the ultimate target for the return value.
regsToSave :: String -> Compiler [String]
regsToSave r = do
    free <- gets freeRegs
    -- so we need to save: all registers A, B, C that are in use, since the function may clobber them, but we shouldn't save one that is the target register for the return
    return $ filter (not . (`elem` (r:free))) ["A", "B", "C"]

-- given the register target for this expression, returns code to save the necessary subset of A, B and C before a call, and restore them afterward
saveRegsForCall :: String -> Compiler ([Asm], [Asm])
saveRegsForCall r = do
    rs <- regsToSave r
    return (map (SET PUSH . Reg) (reverse rs),
            map (\r -> SET (Reg r) POP) rs)



isLvalue :: Expr -> Bool
isLvalue (Var _) = True
isLvalue (Selector x _) = isLvalue x
isLvalue (Index x _) = isLvalue x
isLvalue _ = False


-- Sets the variable with the given name to the given expression, returns code to make it so (including computing the expression).
setVar :: QualIdent -> Expr -> Compiler [Asm]
setVar i x = do
    -- First look up the variable's type and check against the type of the expression.
    xt <- typeCheck x
    t  <- lookupSymbol i
    assign <- assignable xt t
    when (not assign) $ typeError $ "Attempting to set the variable " ++ show i ++ " of type " ++ show t ++ " to incompatible type " ++ show xt
    -- if we get to here then the type is assignable, so compute the expression's value
    XR exprCode exprVal <- compileExpr x
    freeIfReg exprVal

    -- location of the variable
    loc <- lookupLocation i
    storeCode <- case loc of
        LocReg s   -> return [SET (Reg s) exprVal]
        LocStack n -> return [SET (AddrRegLit "J" n) exprVal]
        LocLabel l -> return [SET (AddrLabel l) exprVal]
        LocConstant _ -> typeError $ "Attempt to assign a value to constant " ++ show i

    return $ exprCode ++ storeCode



-- basic assembly optimizer
-- current just removes known no-ops
optimize :: [Asm] -> [Asm]
optimize = map (\a -> if isNoop a then Comment a "noop" else a) --filter (not.isNoop)

-- checks whether a given instruction is a no-op
isNoop :: Asm -> Bool
isNoop (SET b a) | b == a    = True -- setting anything to itself is a no-op
                 | otherwise = False
isNoop (ADD _ (Lit 0)) = True
isNoop (SUB _ (Lit 0)) = True
isNoop (MUL _ (Lit 1)) = True
isNoop (MLI _ (Lit 1)) = True
isNoop (DIV _ (Lit 1)) = True
isNoop (DVI _ (Lit 1)) = True
isNoop _ = False



data Options = Options {
         optOutput :: String
        ,optShowVersion :: Bool
        ,optShowHelp :: Bool
        ,optLibDirs :: [String]
        ,optPackage :: Bool
    } deriving (Show)

defaultOptions = Options "" False False [] False


options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h'] ["help"]
        (NoArg (\opts -> opts { optShowHelp = True }))
        "Display this help message."
    , Option [] ["version"]
        (NoArg (\opts -> opts { optShowVersion = True }))
        "Display version information."
    , Option ['o'] ["output"]
        (ReqArg ((\f opts -> opts { optOutput = f })) "FILE")
        "Set the output file."
    , Option ['L'] ["libdir"]
        (ReqArg ((\d opts -> opts { optLibDirs = optLibDirs opts ++ [d] })) "DIR")
        "Add a directory to the library search path."
    , Option ['p'] ["package"]
        (NoArg (\opts -> opts { optPackage = True }))
        "Include a CubeOS package header."
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv = 
    case getOpt Permute options argv of
        (o,n,[])   -> return (foldl' (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageHeader options))

usageHeader = "Usage: go10cc [OPTION...] inputfile"
versionInfo = "go10cc v0.1  Copyright (C) 2012 Braden Shepherdson"

main = do
    argv <- getArgs
    (opts, remaining) <- compilerOpts argv
    inputFile <- case remaining of
        [r] -> return r
        []  -> error "No input file supplied."
        _   -> error "Multiple input files are not supported. import your libraries and provide a single file on the command line."

    quit <- case (optShowVersion opts, optShowHelp opts) of
        (_, True) -> do
            putStrLn $ usageInfo usageHeader options
            return True
        (True, False) -> do
            putStrLn versionInfo
            return True
        _         -> return False

    when quit $ exitWith ExitSuccess

    -- compile the main input file
    eParseTree <- loadFile inputFile
    parseTree@(SourceFile pkgName _ _) <- case eParseTree of
        Left e  -> error $ "Could not load input file: " ++ inputFile
        Right p -> return p

    code <- doCompile parseTree (optLibDirs opts)
    let code' = optimize code
    outputHandle <- openFile (optOutput opts) WriteMode
    when (optPackage opts) $ hPutStrLn outputHandle $ unlines [
        ":package." ++ pkgName,
        "DAT 0xffab, 0xcdff",
        "DAT \"" ++ pkgName ++ "\", 0"
        ]
    hPutStrLn outputHandle $ unlines $ map show code'
    hClose outputHandle


loadFile :: String -> IO (Either IOException SourceFile)
loadFile name = do
    hE <- try $ openFile name ReadMode
    case hE of
        Left e  -> return $ Left e
        Right h -> do
            str <- hGetContents h
            return . Right $ parseGo (alexScanTokens str)

