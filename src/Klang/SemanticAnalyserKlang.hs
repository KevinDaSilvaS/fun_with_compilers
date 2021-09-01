{-# LANGUAGE DataKinds #-}
module Klang.SemanticAnalyserKlang where

import Klang.KlangSets
import Klang.TokensKlang

import Klang.IRBuilderKlang
import Data.Maybe (isJust)

startSemanticAnalysis symbolTable (Assign 
    (idToken, identifier) (Expr expr) ptr)
    | isJust existentIdentifier  = 
        error ("Identifier " ++ identifier ++ " already declared") 
    | otherwise = startSemanticAnalysis nSymbolTable ptr
    where
        existentIdentifier = lookup identifier symbolTable
        nSymbolTable = symbolTable ++ [(identifier, parseExpr symbolTable expr)]
startSemanticAnalysis st ptr = st


parseExpr st (Integer (IntegerToken, value) op) = 0;
parseExpr st expr = 1;
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

