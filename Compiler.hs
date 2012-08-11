{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import GoLexer
import GoParser

import Control.Monad
import Control.Monad.State.Strict

import Control.Applicative

import qualified Data.Map as M
import Data.Maybe (isJust)

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
    ,dirtyRegs :: [Char] -- registers that have been dirtied in a function and need cleaning up.
    ,usedRegs :: [Char] -- registers that are currently in use and can't be used now.
    ,strings :: [(String,String)] -- a collection of string literals to be written into the binary. pairs are (symbol name, content).
    ,types :: SymbolTable
    }


newtype Compiler a = Compiler (StateT CompilerState IO a)
    deriving (Functor, Applicative, Monad, MonadState CompilerState, MonadIO)

emptyCS = CS [] [] [] [] M.empty

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

typeCheck (BuiltinCall LNew (Just (TypeArray t)) _) = return (TypeArray t) -- arrays are already pointers, we don't need to create a pointer to them.
typeCheck (BuiltinCall LNew (Just t) _) = return (TypePointer t) -- otherwise create a pointer to the provided type (TODO does this work for strings?)
typeCheck (BuiltinCall LNew Nothing _)  = typeError $ "new() must have a type provided as the first argument."
typeCheck (BuiltinCall LDelete Nothing [x]) = return TypeVoid -- delete returns nothing
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


main = do
    str <- getContents
    print $ parseGo (alexScanTokens str)


