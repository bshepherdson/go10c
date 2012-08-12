{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import GoLexer
import GoParser

import Control.Monad
import Control.Monad.State.Strict

import Control.Applicative
import Control.Arrow (first)

import qualified Data.Map as M
import Data.Maybe (isJust, isNothing)

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
    }

data Location = Reg String
              | Stack Int -- places above the stack pointer
              | Label String -- in the given label


newtype Compiler a = Compiler (StateT CompilerState IO a)
    deriving (Functor, Applicative, Monad, MonadState CompilerState, MonadIO)

emptyCS = CS [] [] [] [] M.empty [] [] []

runCompiler :: Compiler a -> CompilerState -> IO a
runCompiler (Compiler a) s = fst <$> runStateT a s


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
findLocals (StmtVarDecl i _ _ : rest) = QualIdent Nothing i : findLocals rest
findLocals (StmtShortVarDecl i _ : rest) = QualIdent Nothing i : findLocals rest
findLocals (_:rest) = findLocals rest


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


-- turns a function name into a compiler label
mkLabel :: String -> String
mkLabel s = "_go10c_" ++ s


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
typeCheck (LitInt _) = return TypeInt
typeCheck (LitChar _) = return TypeChar
typeCheck (LitBool _) = return TypeBool
typeCheck (LitString _) = return TypeString
typeCheck (LitComposite t _) = return t
typeCheck (Var i) = lookupSymbol i
typeCheck (Selector x field) = do
    xt <- typeCheck x
    case xt of
        (TypeStruct fields) -> case filter ((==field) . fst) fields of
            []      -> typeError $ "Struct does not have a field named '" ++ field ++ "'."
            [(_,t)] -> return t
            _       -> error $ "The impossible just happened! Struct with multiple fields named '" ++ field ++ "'."
        _ -> typeError $ "Attempt to select a field from a non-struct value " ++ show x ++ ", type " ++ show xt

typeCheck (Index x i) = do
    arrType <- typeCheck x
    indexType <- typeCheck i
    case (arrType, indexType) of
        (TypeArray t, TypeInt) -> return t
        (TypeArray _, t) -> typeError $ "Array index has type " ++ show t ++ ", not an integer."
        (t, _) -> typeError $ "Attempt to index non-array type " ++ show t ++ "."

typeCheck (Call f args) = do
    ft <- typeCheck f
    case ft of
        (TypeFunction argTypes returnType) -> do
            providedArgTypes <- mapM typeCheck args
            let zipped = zip argTypes providedArgTypes
                mismatched = dropWhile (uncurry (==)) zipped
            case mismatched of
                [] -> return returnType -- function call is legal
                ((expected, actual):_) -> typeError $ "Function call expected an argument of type " ++ show expected ++ " but got " ++ show actual ++ "."
        _ -> typeError $ "Attempt to call a non-function value " ++ show f ++ " of type " ++ show ft

typeCheck (BuiltinCall LNew (Just (TypeArray t)) [n]) = do
    nt <- typeCheck n
    nut <- underlyingType nt
    when (nut /= TypeInt) $ typeError $ "new() called to create an array type with non-int size of type " ++ show nt
    return (TypeArray t) -- arrays are already pointers, we don't need to create a pointer to them.

typeCheck (BuiltinCall LNew (Just (TypeArray t)) _) = typeError $ "new() called to create an array but no size was provided"
typeCheck (BuiltinCall LNew (Just t) _) = return (TypePointer t) -- otherwise create a pointer to the provided type (TODO does this work for strings?)
typeCheck (BuiltinCall LNew Nothing _)  = typeError $ "new() must have a type provided as the first argument."
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
            "-" -> unopInt
            "^" -> unopInt
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
    leftType <- typeCheck left
    rightType <- typeCheck right
    case op of
        "+" -> do
            case (leftType, rightType) of
                (TypeString, TypeString) -> return TypeString
                (TypeInt, TypeInt) -> return TypeInt
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
    when (actLeft /= expLeft) $ typeError $ "Left argument of " ++ op ++ " expected " ++ show expLeft ++ " but found " ++ show actLeft ++ "."
    when (actRight /= expRight) $ typeError $ "Right argument of " ++ op ++ " expected " ++ show expRight ++ " but found " ++ show actRight ++ "."
    return retType

typeCheckEqOp :: String -> Type -> Type -> Compiler Type
typeCheckEqOp op left right = do
    when (left /= right) $ typeError $ "Arguments to " ++ op ++ " have mismatched types:\n\tLeft: " ++ show left ++ "\n\tRight: " ++ show right
    return TypeBool


-- return True if the first type can be converted into the second.
-- a value x of type V is convertible to a type T in any of the following cases:
-- * V is assignable to T
-- * V and T have the same underlying type
-- * V and T are (unnamed) pointer types, and their pointer base types are convertible
-- * V is int or []char and T is string
-- * V is string and T is []char
convertible :: Type -> Type -> Compiler Bool
convertible l r = do
    assign <- assignable l r
    uL <- underlyingType l
    uR <- underlyingType r
    case (assign, uL == uR, l, r) of
        (True, _, _, _) -> return True -- assignable
        (False, True, _, _) -> return True -- identical underlying types
        (_, _, TypePointer p1, TypePointer p2) -> convertible p1 p2
        (_, _, TypeInt, TypeString) -> return True
        (_, _, TypeArray TypeChar, TypeString) -> return True
        (_, _, TypePointer TypeChar, TypeString) -> return True
        (_, _, TypeString, TypeArray TypeChar) -> return True
        (_, _, TypeString, TypePointer TypeChar) -> return True
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
            _ -> return False
-- TODO: Handle nil


underlyingType :: Type -> Compiler Type
underlyingType (TypeName t) = lookupType t >>= underlyingType
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
compile :: Statement -> Compiler [String]
compile (StmtTypeDecl name t) = error "TypeDecls are not to be compiled."

-- note that this doesn't add the symbol, but rather expects it to already exist. this does compile initializers, though.
compile (StmtVarDecl name t Nothing) = addSymbol (QualIdent Nothing name) t >> return [] -- nothing to do for a plain declaration
compile (StmtVarDecl name t (Just x)) = addSymbol (QualIdent Nothing name) t >> setVar (QualIdent Nothing name) x
compile (StmtShortVarDecl name x) = do
    t <- typeCheck x
    addSymbol (QualIdent Nothing name) t
    setVar (QualIdent Nothing name) x
compile (StmtFunction _ _ _ Nothing) = return []
compile (StmtFunction name args ret (Just body)) = do
    -- so a function. we need a new scope pushed, as well as a new set of locations for local variables and arguments.
    -- here's how the base pointer is handled. we use J as a base pointer. it points at the first local.
    -- the locals are at J+0, J+1, J+2, etc. the old value of J is stored at J+n, and any stack-passed args are in J+n+1, J+n+2, ...
    -- first, harvest the locals from the function body.
    addSymbol (QualIdent Nothing name) (TypeFunction (map snd args) ret) -- add the function to the symbol table before we grab the state, to allow recursion.
    s <- get
    let allLocals = findLocals body -- [(QualIdent, Type)]
        localCount = length allLocals
        argLocations = [Reg "A", Reg "B", Reg "C"] ++ map Stack [localCount+2..] -- they're deeper than the locals, and the first three are in registers. +1 because the old PC return address is stored on top of them, and the base pointer is stored on top of that.
        myArgs = zip (map (QualIdent Nothing . fst) args) argLocations -- [(QualIdent, Location)], as necessary
        myLocalLocations = zip allLocals $ map Stack [0..]
        mySymbols = M.fromList $ map (first (QualIdent Nothing)) args -- locals are not included in the symbol table, they'll be added as their definitions pass by.
        s' = s {
            locals = myLocalLocations,
            args = myArgs,
            dirtyRegs = [],
            freeRegs = drop (min 3 (length args)) ["A", "B", "C", "X", "Y", "Z", "I"],
            symbols = mySymbols : symbols s,
            this = name }

        prefix = mkLabel name

    put s'

    bodyCode <- concat <$> mapM compile body

    s'' <- get

    -- add preamble and postamble, saving and restoring the dirty registers and quitting.
    let preambleCode  = [":" ++ prefix,
                         "SET PUSH, J",                     -- store the old value of J, the base pointer.
                         "SUB SP, " ++ show localCount,     -- make room for the locals
                         "SET J, SP"] ++                    -- and set J to be the new base pointer, pointing at the first local.
                        map ("SET PUSH, " ++) (dirtyRegs s'')
        postambleCode = [":" ++ prefix ++ "_done"] ++
                        map (\r -> "SET " ++ r ++ ", POP") (reverse (dirtyRegs s'')) ++
                        ["ADD SP, " ++ show localCount,     -- remove the locals
                         "SET J, POP",                      -- restore the old base pointer.
                         "SET PC, POP"]                     -- and return

    put s -- restore the original state, removing my locals, args and symbols.
    return $ preambleCode ++ bodyCode ++ postambleCode


compile (StmtLabel s) = return [":" ++ s] -- this isn't easy to do re: labeled jumps and breaks and crap. maybe make this a container instead of an ordinary statement.

compile (StmtExpr x@(Call _ _)) = do
    r <- getReg
    code <- compileExpr x r
    freeReg r
    return code
compile (StmtExpr x@(BuiltinCall _ _ _)) = do
    r <- getReg
    code <- compileExpr x r
    freeReg r
    return code
compile (StmtExpr x) = error $ "Suspicious code. Expression " ++ show x ++ " has no side effects."

compile (StmtInc x) = compileIncDec "ADD " x
compile (StmtDec x) = compileIncDec "SUB " x

compile (StmtAssignment Nothing lvalue rvalue) = do
    when (not $ isLvalue lvalue) $ compileError $ "Attempt to assign to non-lvalue " ++ show lvalue
    lt <- typeCheck lvalue
    rt <- typeCheck rvalue
    assign <- assignable rt lt
    when (not $ assign) $ typeError $ "Right side of assignment is not assignable to left side.\n\tLeft: " ++ show lt ++ "\n\tRight: " ++ show rt

    -- if we get down here, then this assignment is legal, so compile it.
    r <- getReg
    exprCode <- compileExpr rvalue r
    case lvalue of
        (Var i) -> do
            locCode <- lookupLocation i >>= compileLocation
            return $ exprCode ++ ["SET " ++ locCode ++ ", " ++ r]
        _ -> error "Not implemented: Assigning to lvalues other than straight variables." -- TODO implement

-- TODO: Optimization: some ops, like += and -=, can be optimized by using ADD instead of computing and setting. Priority low, though, only a couple of instructions wasted.
compile (StmtAssignment (Just op) lvalue rvalue) = compile (StmtAssignment Nothing lvalue (BinOp (LOp op) lvalue rvalue))


compile (StmtIf initializer condition ifbody elsebody) = do
    -- the anatomy of an if-statement:
    -- it begins with the initializer. then the condition is computed. a jump is compiled that if the condition is false jumps to the beginning of the else block or the end
    -- at the end of the body a jump to the end is computed to skip over the else block.
    -- or is it easier to go in reverse?

    ct <- typeCheck condition
    when (ct /= TypeBool) $ typeError $ "Condition of an if statement must have type bool, found " ++ show ct

    -- push the new scope before compiling the initializer, since any variables it defines are scoped in the if.
    modify $ \s -> s { symbols = M.empty : symbols s }
    initCode <- concat <$> mapM compile initializer
    r <- getReg
    condCode <- compileExpr condition r

    -- get a label prefix for this if, set 'this' appropriately.
    prefix <- uniqueLabel
    let jumpCode = ["IFE " ++ r ++ ", 0",
                    "SET PC, " ++ if null elsebody then prefix ++ "_endif" else prefix ++ "_else"] -- jump if the condition is false, since we're jumping to the else block.
    freeReg r -- don't need this reserved anymore, because the jump is now compiled.
    ifCode <- concat <$> mapM compile ifbody
    let ifJumpCode = if null elsebody then [] else ["SET PC, " ++ prefix ++ "_endif"] -- add a jump to the end of the if body if there's an else to jump over.

    elseCode <- case elsebody of
        [] -> return []
        _  -> do
            elseCode <- concat <$> mapM compile elsebody
            return $ [":" ++ prefix ++ "_else"] ++ elseCode

    -- TODO I think variables declared in any part of the if are spreading to the rest of it. Mostly harmless.
    -- remove the scope
    modify $ \s -> s { symbols = tail (symbols s) }

    return $ initCode ++ condCode ++ jumpCode ++ ifCode ++ ifJumpCode ++ elseCode ++ [":" ++ prefix ++ "_endif"]



compile (StmtFor initializer condition incrementer body) = do
    ct <- typeCheck condition
    when (ct /= TypeBool) $ typeError $ "Loop condition must have type bool, found " ++ show ct

    prefix <- uniqueLabel

    -- add the new scope before the initializer
    modify $ \s -> s { symbols = M.empty : symbols s }

    initCode <- concat <$> mapM compile initializer
    let topLabelCode = [":" ++ prefix ++ "_top"]

    r <- getReg
    condCode <- compileExpr condition r
    let condCheckCode = ["IFE " ++ r ++ ", 0", "SET PC, " ++ prefix ++ "_end"]
    freeReg r

    bodyCode <- concat <$> mapM compile body
    incCode <- concat <$> mapM compile incrementer

    let topJumpCode = ["SET PC, " ++ prefix ++ "_top"]

    let endLabelCode = [":" ++ prefix ++ "_end"]

    modify $ \s -> s { symbols = tail (symbols s) } -- remove the new loop scope.

    return $ initCode ++ topLabelCode ++ condCode ++ condCheckCode ++ bodyCode ++ incCode ++ topJumpCode ++ endLabelCode


compile (StmtReturn mx) = do
    exprCode <- case mx of
        Nothing -> return []
        Just x  -> do
            r <- getReg
            code <- compileExpr x r
            freeReg r
            return $ code ++ ["SET A, " ++ r]
    t <- gets this
    return $ exprCode ++ ["SET PC, " ++ t ++ "_done"]


compile (StmtGoto label) = return ["SET PC, " ++ label]

compile _ = compileError "Not implemented"


compileIncDec :: String -> Expr -> Compiler [String]
compileIncDec opcode (Var i) = do
    xt <- typeCheck (Var i)
    xut <- underlyingType xt
    case xut of
        TypeInt -> do
            locCode <- lookupLocation i >>= compileLocation
            return [opcode ++ locCode ++ ", 1"]
        _ -> typeError $ "Attempt to ++ or -- non-int type " ++ show xt


-- turns a Location into the assembly string representing it
compileLocation :: Location -> Compiler String
compileLocation (Reg r) = return r
compileLocation (Stack n) = return $ "PICK " ++ show n
compileLocation (Label s) = return $ "[" ++ s ++ "]"


-- compiles an expression, storing the result in the given register.
compileExpr :: Expr -> String -> Compiler [String]
compileExpr (LitInt n) r = return ["SET " ++ r ++ ", " ++ show n
compileExpr (LitBool b) r = return ["SET " ++ r ++ ", " ++ if b then "1" else "0"]
compileExpr (LitChar c) r = return ["SET " ++ r ++ ", " ++ ord c]
compileExpr (LitString s) r = do
    unique <- uniqueLabel
    modify $ \st -> st { strings = (unique, s) : strings st }
    return ["SET " ++ r ++ ", " ++ unique]

compileExpr (LitComposite _) _ = error "Composite literals not implemented"

compileExpr (Var i) r = do
    loc <- lookupLocation i >>= compileLocation
    return ["SET " ++ r ++ ", " ++ loc]

compileExpr (Selector _ _) _ = error "Selectors not implemented"

compileExpr (Index arr ix) r = do
    arrType <- typeCheck arr
    size <- typeSize arrType
    arrCode <- compileExpr arr r
    ixR <- getReg
    ixCode <- compileExpr ix ixR
    freeReg ixR
    return $ arrCode ++ ixCode ++ 
             (if size > 1 then ["MUL " ++ ixR ++ ", " ++ show size] else []) ++ -- if the size of the array elements is > 1, multiply from the index to the offset. TODO optimization trivial: shift for sizes that are powers of 2.
             ["ADD " ++ r ++ ", " ++ ixR, "SET " ++ r ++ ", [" ++ r ++ "]"]

compileExpr (Call (Var f) args) r = do
    when (length args >= 4) $ error "Functions with more than 3 arguments are not implemented."
    (saveCode, restoreCode) <- saveRegsForCall r

    argCode <- concat <$> mapM (fmap concat . mapM (uncurry compileExpr)) (zip args ["A", "B", "C"])
    let jsrCode = ["JSR " ++ mkLabel f]
        returnCode = ["SET " ++ r ++ ", A"]

    return $ saveCode ++ argCode ++ jsrCode ++ returnCode ++ restoreCode

compileExpr (Call _ _) _ = error "Function pointers are not supported, the function used in a call must be an identifier."

-- creating an array with a length
compileExpr (BuiltinCall LNew (Just (TypeArray t)) (n:_)) r = do
    (saveCode, restoreCode) <- saveRegsForCall r
    size <- typeSize t
    return $ saveCode ++
             ["SET A, " ++ show (n*size),
              "JSR heap.alloc"] ++ -- pointer is stored in A
             (if r /= "A" then ["SET " ++ r ++ ", A"] else []) ++
             restoreCode

compileExpr (BuiltinCall LNew (Just t) _) r = do
    (saveCode, restoreCode) <- saveRegsForCall r
    size <- typeSize t
    return $ saveCode ++
             ["SET A, " ++ show size,
              "JSR heap.alloc"] ++ -- pointer is stored in A
             (if r /= "A" then ["SET " ++ r ++ ", A"] else []) ++
             restoreCode

compileExpr (BuiltinCall LNew _ _) _ = compileError "new() called with no type as an argument"

compileExpr (BuiltinCall LDelete Nothing [x]) _ = do
    (saveCode, restoreCode) <- saveRegsForCall "_" -- placeholder
    -- the type has already been checked, so just call it.
    expCode <- compileExpression x "A"
    return $ saveCode ++ expCode ++ ["JSR heap.free"] ++ restoreCode





-- returns the registers to be saved before making a function call.
-- saves A, B, and C, unless they are unused or the ultimate target for the return value.
regsToSave :: String -> Compiler [String]
regsToSave r = do
    free <- gets freeRegs
    -- so we need to save: all registers A, B, C that are in use, since the function may clobber them, but we shouldn't save one that is the target register for the return
    return $ filter (not . (`elem` (r:free))) ["A", "B", "C"]

-- given the register target for this expression, returns code to save the necessary subset of A, B and C before a call, and restore them afterward
saveRegsForCall :: String -> Compiler ([String], [String])
saveRegsForCall r = do
    rs <- regsToSave r
    return (map ("SET PUSH, " ++) rs,
            map (\r -> "SET " ++ r ++ ", POP") (reverse rs))



isLvalue :: Expr -> Bool
isLvalue (Var _) = True
isLvalue (Selector x _) = isLvalue x
isLvalue (Index x _) = isLvalue x
isLvalue _ = False


{-
data Type = TypeName QualIdent
          | TypeBool
          | TypeChar
          | TypeInt
          | TypeString
          | TypeArray Type
          | TypeStruct [(String, Type)] -- a struct with its parameters and their types.
          | TypePointer Type
          | TypeFunction [Type] Type -- a function giving its argument types and (singular, in my Go) return type.
          | TypeVoid -- nonexistent type used for functions without return values
  deriving (Eq, Show)

data Expr = LitInt Int
          | LitBool Bool
          | LitChar Char
          | LitString String
          | LitComposite Type [(Key, Expr)]
          | Var QualIdent
          | Selector Expr String
          | Index Expr Expr -- array expression and index expression
          | Call Expr [Expr] -- function expression and argument expressions
          | BuiltinCall Token (Maybe Type) [Expr] -- token of builtin, maybe a type as the first arg, and a list of parameter expressions
          | Conversion Type Expr
          | UnOp Token Expr -- unary operator and expression
          | BinOp Token Expr Expr -- binary operator and operands
  deriving (Show)

data QualIdent = QualIdent { package :: Maybe String, name :: String }
  deriving (Eq, Ord)

-}

-- Sets the variable with the given name to the given expression, returns code to make it so (including computing the expression).
setVar :: QualIdent -> Expr -> Compiler [String]
setVar i x = do
    -- First look up the variable's type and check against the type of the expression.
    xt <- typeCheck x
    t  <- lookupSymbol i
    assign <- assignable xt t
    when (not assign) $ typeError $ "Attempting to set the variable " ++ show i ++ " of type " ++ show t ++ " to incompatible type " ++ show xt
    -- if we get to here then the type is assignable, so compute the expression's value
    r <- getReg
    exprCode <- compileExpr x r

    -- location of the variable
    loc <- lookupLocation i
    storeCode <- case loc of
        Reg s -> return ["SET " ++ s ++ ", " ++ r]
        Stack n -> return ["SET PICK " ++ show n ++ ", " ++ r]
        Label l -> return ["SET [" ++ l ++ "], " ++ r]

    freeReg r
    return $ exprCode ++ storeCode


main = do
    str <- getContents
    print $ parseGo (alexScanTokens str)


