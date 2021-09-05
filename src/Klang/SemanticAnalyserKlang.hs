{-# LANGUAGE DataKinds #-}
module Klang.SemanticAnalyserKlang where

import Klang.KlangSets
import Klang.TokensKlang

import Klang.IRBuilderKlang
import Data.Maybe (isJust, fromJust, isNothing)
import Data.Bifunctor ( Bifunctor(second) )

startSemanticAnalysis symbolTable 
    (Routine (timesToken, timesValue) (StartBlockToken, _) ptr) 
    | timesToken == IntegerToken = startSemanticAnalysis nst ptr
    | timesToken == IdentifierToken && isNothing existentIdentifier  
    = error 
    ("Error identifier: " ++ timesValue ++ "not declared.")
    | timesToken == IdentifierToken && existentIdentifierValue == IntegerToken 
    = startSemanticAnalysis nst ptr
    | otherwise = error 
        ("Error execution times in loop must be an integer. " 
        ++ show existentIdentifierValue ++ "received.")
    where
        nst = symbolTable ++ [("_GET_CURR_INDEX", (IntegerToken, "0"))]
        existentIdentifier = lookup timesValue symbolTable
        existentIdentifierValue = fst (fromJust existentIdentifier)
startSemanticAnalysis symbolTable (If (ifToken, ifValue) expr openblock ptr) 
    | snd parsedExp + 1 > 0 = startSemanticAnalysis symbolTable ptr
        where
            parsedExp = parseExpr symbolTable expr
startSemanticAnalysis symbolTable (Show (idToken, identifier) expr ptr)
        | snd parsedExp + 1 > 0 = startSemanticAnalysis symbolTable ptr
        where
            parsedExp = parseExpr symbolTable expr
startSemanticAnalysis symbolTable (Assign
    (idToken, identifier) (Str value) ptr) =
        startSemanticAnalysis nSymbolTable ptr
        where
            nSymbolTable = symbolTable ++ [(identifier, value)]
startSemanticAnalysis symbolTable (Assign
    (idToken, identifier) (Expr expr) ptr)
    | isJust existentIdentifier  =
        error ("Identifier " ++ identifier ++ " already declared")
    | otherwise = startSemanticAnalysis nSymbolTable ptr
    where
        existentIdentifier = lookup identifier symbolTable
        nSymbolTable = symbolTable ++ [(identifier, nValue)]
        parsedExp = parseExpr symbolTable expr
        nValue = second show parsedExp
startSemanticAnalysis 
    symbolTable (CloseBlock (closeToken, closeValue) ptr) =
        startSemanticAnalysis symbolTable ptr
startSemanticAnalysis st ptr = st

parseExpr st (Integer (IntegerToken, value) op)
    | length existentIdentifierValue' == length value = (IntegerToken, parseOperator st (read value :: Float) op)
    | otherwise = error
    ("Arithmetic operators can only be performed in numeric tokens. in: "
    ++ value)
    where
        existentIdentifierValue' = filter (`elem` floats) value
parseExpr st (Integer (IdentifierToken, value) op)
    | isJust existentIdentifier && 
    length existentIdentifierValue' == length existentIdentifierValue =
        (IntegerToken,
        parseOperator st (read existentIdentifierValue :: Float) op)
    | isNothing existentIdentifier = 
        error ("Error identifier: " ++ value ++" not declared")
    where
        existentIdentifier = lookup value st
        existentIdentifierValue = snd (fromJust existentIdentifier)
        existentIdentifierValue' = filter (`elem` floats) existentIdentifierValue
parseExpr st (ComparativeExpr expr comparative expr') 
    | parseComparativeRules st expr expr' = exprFinal
    | otherwise = error "Cannot compare values from different types"
    where
        exprFinal = (token, floatVal + snd (parseExpr st expr')) 
        (token, floatVal) = parseExpr st expr
parseExpr st expr = (EmptyToken, 0);

parseComparativeRules st 
        (Integer (exprTk, value) _) 
        (Str strContent) 
        | exprTk == IdentifierToken && 
        fst existentIdentifierValue == fst strContent = True
        | otherwise = False
          where
            existentIdentifier = lookup value st
            existentIdentifierValue = fromJust existentIdentifier
parseComparativeRules st 
            (Str strContent) 
            (Integer (exprTk, value) _) 
            | exprTk == IdentifierToken 
            && fst existentIdentifierValue == fst strContent = True
            | otherwise = False
              where
                existentIdentifier = lookup value st
                existentIdentifierValue = fromJust existentIdentifier
parseComparativeRules st (Integer _ _) (Integer _ _) = True
parseComparativeRules st (Str _ ) (Str _ ) = True
parseComparativeRules _ _ _ = False

parseOperator st prevVal (Operator (_,"+") valueExp) =
    prevVal + snd (parseExpr st valueExp)
parseOperator st prevVal (Operator (_,"/") (Integer (IdentifierToken, value) expr ))
    | isJust existentIdentifier && numValue /= 0 =
        prevVal / snd (parseExpr st (Integer (IdentifierToken, value) expr))
    | otherwise = error "Cannot perform division by zero"
    where
        existentIdentifier = lookup value st
        existentIdentifierValue = snd (fromJust existentIdentifier)
        numValue = read existentIdentifierValue :: Float
parseOperator st prevVal (Operator (_,"/") (Integer (IntegerToken, value) expr ))
    | numValue /= 0 =
        prevVal / snd (parseExpr st (Integer (IntegerToken, value) expr))
    | otherwise = error "Cannot perform division by zero"
    where
        numValue = read value :: Float
parseOperator st prevVal (Operator (_,"-") valueExp) =
    prevVal - snd (parseExpr st valueExp)
parseOperator st prevVal (Operator (_,"*") valueExp) =
    prevVal * snd (parseExpr st valueExp)
parseOperator st prevVal EndExpr = prevVal
parseOperator st prevVal v = error ("Value not expected" ++ show v)