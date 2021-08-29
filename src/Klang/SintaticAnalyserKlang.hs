module Klang.SintaticAnalyserKlang where

import Klang.LexicalAnalyserKlang ( startAutomaton )
import Klang.TokensKlang

{- newtype SymbolTable = Var ([Char], (Tokens, [Char]))
symbolTable = [] -}

{- 
    [LET GRAMMAR]
    S -> LIAVS
    L -> let 
    I -> identifier 
    A -> := 
    V -> int or string
-}

startSintaticAnalysis [] _ _ = []
startSintaticAnalysis program line col
    | fst token == LetToken  = token:_identifier remain nline ncol
    | otherwise = error ("Expected let. Received:" ++ show token)
    where
        (token, remain, nline, ncol) = startAutomaton program line col []

_identifier [] line col = error "Unexpected EOF"
_identifier program line col 
    | fst token == IdentifierToken = token:_assignment remain nline ncol
    | otherwise = error ("Expected identifier. Received:" ++ show token)
    where
        (token, remain, nline, ncol) = startAutomaton program line col [] 

_assignment [] line col = error "Unexpected EOF"
_assignment program line col 
    | fst token == AssignToken = token:_value remain nline ncol
    | otherwise = error ("Expected AssignToken. Received:" ++ show token)
    where
        (token, remain, nline, ncol) = startAutomaton program line col [] 

_value [] line col = error "Unexpected EOF"
_value program line col 
    | fst token == IntegerToken || fst token == StringToken = 
        token:startSintaticAnalysis remain nline ncol
    | otherwise = error ("Expected identifier. Received:" ++ show token)
    where
        (token, remain, nline, ncol) = startAutomaton program line col [] 