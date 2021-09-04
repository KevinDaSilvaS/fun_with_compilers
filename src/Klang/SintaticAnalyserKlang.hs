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

startSintaticAnalysis [] _ _ 0 = []
startSintaticAnalysis program line col closingBlocks
    | fst token == LetToken  = token:_identifier remain nline ncol closingBlocks
    | fst token == ShowToken = token:_value remain nline ncol closingBlocks
    | fst token == StartBlockToken && closingBlocks >= 0 = 
        token:startSintaticAnalysis remain nline ncol (closingBlocks+1)
    | fst token == CloseBlockToken && closingBlocks >= 0 = 
        token:startSintaticAnalysis remain nline ncol (closingBlocks-1)
    | fst token == StartBlockToken || fst token == CloseBlockToken = 
        error "Unbalanced open close blocks check for inconsistencies with the :(open block) and ; (close block) symbols"
    | fst token == ConditionalToken = 
        token:_value remain nline ncol closingBlocks
    | snd token `elem` arithmeticOperators = 
        token:_value remain nline ncol closingBlocks
    | snd token `elem` comparativeOperators = 
        token:_value remain nline ncol closingBlocks
    | fst token == EmptyToken = []
    | otherwise = error ("Expected let. Received:" ++ show token)
    where
        (token, remain, nline, ncol) = startAutomaton program line col []

_identifier [] line col _ = error "Unexpected EOF"
_identifier program line col closingBlocks
    | fst token == IdentifierToken = 
        token:_assignment remain nline ncol closingBlocks
    | otherwise = error ("Expected identifier. Received:" ++ show token)
    where
        (token, remain, nline, ncol) = startAutomaton program line col [] 

_assignment [] line col _ = error "Unexpected EOF"
_assignment program line col closingBlocks
    | fst token == AssignToken = _value remain nline ncol closingBlocks
    | otherwise = error ("Expected AssignToken. Received:" ++ show token)
    where
        (token, remain, nline, ncol) = startAutomaton program line col [] 

_value [] line col _ = error "Unexpected EOF"
_value program line col closingBlocks
    | token == StringToken 
    && nextTokenValue `elem` arithmeticOperators = 
        error ("Cannot perform arithmetic operation:" 
        ++ nextTokenValue ++ " in a non integer token.") 
    | token == IntegerToken 
    || token == StringToken 
    || token == IdentifierToken = 
        (token, value):startSintaticAnalysis remain nline ncol closingBlocks
    | otherwise = error ("Expected identifier. Received:" ++ show (token, value))
    where
        ((token, value), remain, nline, ncol) = startAutomaton program line col [] 
        ((_, nextTokenValue), _, _, _) = startAutomaton remain line col [] 