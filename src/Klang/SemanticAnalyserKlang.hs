module Klang.SemanticAnalyserKlang where

import Klang.IRBuilder
import Klang.TokensKlang
import Klang.SintaticAnalyserKlang

startSemanticAnalysis symbolTable (Program ptr ptr') = nSymbolTable'
    where
        (nSymbolTable)  = startSemanticAnalysis symbolTable ptr
        (nSymbolTable') = startSemanticAnalysis nSymbolTable ptr'
startSemanticAnalysis symbolTable (Assign (token, identifier) value ptr)
    | fst value == IdentifierToken && notElem value' symbolTable =
        error ("Identifier " ++ show value' ++ " not defined.")
    | otherwise = startSemanticAnalysis nSymbolTable ptr
    where
        value' = snd value
        nSymbolTable = identifier:symbolTable
startSemanticAnalysis st EndNode = st
