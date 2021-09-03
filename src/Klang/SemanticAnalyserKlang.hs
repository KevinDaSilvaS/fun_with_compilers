{-# LANGUAGE DataKinds #-}
module Klang.SemanticAnalyserKlang where

import Klang.KlangSets
import Klang.TokensKlang

import Klang.IRBuilderKlang
import Data.Maybe (isJust, fromJust, isNothing)
import Data.Bifunctor ( Bifunctor(second) )

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
    | otherwise = error 
        ("Arithmetic operators can only be performed in numeric tokens. in: "
        ++ value)
    where
        existentIdentifier = lookup value st
        existentIdentifierValue = snd (fromJust existentIdentifier)
        existentIdentifierValue' = filter (`elem` floats) existentIdentifierValue
parseExpr st expr = (IdentifierToken, 1);


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






--startSemanticAnalysis st ptr = error ""

{- startSemanticAnalysis :: ([[Char]], [[Char]]) -> ParsingTree -> ([[Char]], [[Char]])
startSemanticAnalysis symbolTable (Program ptr ptr') = output
    where
        nSymbolTable = startSemanticAnalysis symbolTable ptr
        output = startSemanticAnalysis nSymbolTable ptr'
startSemanticAnalysis symbolTable
    (Arithmetic (tokenOperator, operator) (tokenValue, value) ptr)
        | tokenValue == IdentifierToken && notElem value identifiersList =
            error ("Identifier " ++ show value ++ " not defined.")
        where
            (identifiersList, valuesList) = symbolTable
startSemanticAnalysis symbolTable (Assign (_, identifier) (tokenValue, value) ptr)
    | tokenValue == IdentifierToken && notElem value identifiersList =
        error ("Identifier " ++ show value ++ " not defined.")
    | otherwise = startSemanticAnalysis nSymbolTable ptr
    where
        (identifiersList, valuesList) = symbolTable
        nSymbolTable = (identifier:identifiersList, value:valuesList)
startSemanticAnalysis st EndNode = st
startSemanticAnalysis _ _ = error "Unexpected error in semantic analysis" -}

