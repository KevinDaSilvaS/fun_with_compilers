module Klang.SintaticAnalyserKlang where

import Klang.LexicalAnalyserKlang ( startAutomaton )
import Klang.TokensKlang
import Klang.KlangSets

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
    | fst token == LetToken = token:_identifier remain nline ncol
    | fst token == ShowToken = token:_value remain nline ncol
    | snd token `elem` arithmeticOperators = token:_value remain nline ncol
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
    | fst token == AssignToken = _value remain nline ncol
    | otherwise = error ("Expected AssignToken. Received:" ++ show token)
    where
        (token, remain, nline, ncol) = startAutomaton program line col [] 

_value [] line col = error "Unexpected EOF"
_value program line col 
    | token == StringToken 
    && nextTokenValue `elem` arithmeticOperators = 
        error ("Cannot perform arithmetic operation:" 
        ++ nextTokenValue ++ " in a non integer token.") 
    | token == IntegerToken 
    || token == StringToken 
    || token == IdentifierToken = 
        (token, value):startSintaticAnalysis remain nline ncol
    | otherwise = error ("Expected identifier. Received:" ++ show (token, value))
    where
        ((token, value), remain, nline, ncol) = startAutomaton program line col [] 
        ((_, nextTokenValue), _, _, _) = startAutomaton remain line col [] 