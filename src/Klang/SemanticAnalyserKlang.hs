{-# LANGUAGE DataKinds #-}
module Klang.SemanticAnalyserKlang where

import Klang.KlangSets
import Klang.TokensKlang

import Klang.IRBuilderKlang
import Data.Maybe (isJust, fromJust)
import Data.Bifunctor ( Bifunctor(second) ) 

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

parseExpr st (Integer (IntegerToken, value) op) =
    (IntegerToken, parseOperator st (read value :: Float) op)
parseExpr st (Integer (IdentifierToken, value) op)
    | isJust existentIdentifier =
        (IntegerToken,
        parseOperator st (read existentIdentifierValue :: Float) op)
    | otherwise = error ("Error identifier: " ++ value ++" not declared")
    where
        existentIdentifier = lookup value st
        existentIdentifierValue = snd (fromJust existentIdentifier)
parseExpr st expr = (IdentifierToken, 1);


parseOperator st prevVal (Operator (_,"+") valueExp) =
    prevVal + snd (parseExpr st valueExp)
parseOperator st prevVal (Operator (_,"/") valueExp) =
    prevVal / snd (parseExpr st valueExp)
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

