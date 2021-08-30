module Klang.SemanticAnalyserKlang where

import Klang.IRBuilder
import Klang.TokensKlang

startSemanticAnalysis :: ([[Char]], [[Char]]) -> ParsingTree -> ([[Char]], [[Char]])
startSemanticAnalysis symbolTable (Program ptr ptr') = output
    where
        nSymbolTable = startSemanticAnalysis symbolTable ptr
        output = startSemanticAnalysis nSymbolTable ptr'
startSemanticAnalysis symbolTable (Assign (_, identifier) (tokenValue, value) ptr)
    | tokenValue == IdentifierToken && notElem value identifiersList =
        error ("Identifier " ++ show value ++ " not defined.")
    | otherwise = startSemanticAnalysis nSymbolTable ptr
    where
        (identifiersList, valuesList) = symbolTable
        nSymbolTable = (identifier:identifiersList, value:valuesList)
startSemanticAnalysis st EndNode = st
