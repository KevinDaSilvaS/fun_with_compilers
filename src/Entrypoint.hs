module Entrypoint where

import Sets
import Tokens 

entrypoint :: [Char] -> [Char] -> [(Token, [Char])] -> Int -> [(Token, [Char])]
entrypoint ('\n':xs) [] tokens line = entrypoint xs [] tokens (line+1)
entrypoint (' ':xs) [] tokens line = entrypoint xs [] tokens line
entrypoint (x:xs) reading tokens line 
    | x `elem` numbers = numbersDFA xs (reading ++ [x]) tokens line
    | x `elem` identifiersHead = identifiersDFA xs (reading ++ [x]) tokens line
    | x `elem` assignments = assignmentDFA xs (reading ++ [x]) tokens line
    | otherwise = error ("Non recognized symbol " ++ [x] ++ " at " ++ reading ++ "\"" ++ [x] ++ "\" " ++ " in line " ++ show line)
entrypoint [] [] tokens _ = tokens
entrypoint _ _ _ line = error ("Unknown error in line: " ++ show line)

numbersDFA :: [Char] -> [Char] -> [(Token, [Char])] -> Int -> [(Token, [Char])]
numbersDFA ('\n':xs) reading tokens line = 
    entrypoint xs [] (tokens ++ [token]) (line+1)
    where token = (NumberToken, reading)
numbersDFA (' ':xs) reading tokens line = 
    entrypoint xs [] (tokens ++ [token]) line
    where token = (NumberToken, reading)
numbersDFA (x:xs) reading tokens line 
    | null xs && x `elem` numbers = entrypoint [] [] (tokens ++ [token]) line
    | x `elem` numbers = numbersDFA xs (reading ++ [x]) tokens line
    | x `elem` assignments = assignmentDFA xs [x] (tokens ++ [(NumberToken, reading)]) line
    | otherwise = error ("Number malformed at " ++ reading ++ [x] ++ " in line " ++ show line)
    where token = (NumberToken, reading ++ [x])
numbersDFA [] reading tokens line = entrypoint [] [] (tokens ++ [token]) line
    where token = (NumberToken, reading)

identifiersDFA :: [Char] -> [Char] -> [(Token, [Char])] -> Int -> [(Token, [Char])]
identifiersDFA ('\n':xs) reading tokens line = 
    entrypoint xs [] (tokens ++ [token]) (line+1)
    where token = (IdentifierToken, reading)
identifiersDFA (' ':xs) reading tokens line = 
    entrypoint xs [] (tokens ++ [token]) line
    where token = (IdentifierToken, reading)
identifiersDFA (x:xs) reading tokens line 
    | null xs && x `elem` identifiersTail = entrypoint [] [] (tokens ++ [token]) line
    | x `elem` identifiersTail = identifiersDFA xs (reading ++ [x]) tokens line
    | x `elem` assignments = assignmentDFA xs [x] (tokens ++ [(IdentifierToken, reading)]) line
    | otherwise = error ("Identifier malformed at " ++ reading ++ [x] ++ " in line " ++ show line)
    where token = (IdentifierToken, reading ++ [x])
identifiersDFA [] reading tokens line = entrypoint [] [] (tokens ++ [token]) line
    where token = (IdentifierToken, reading)

assignmentDFA :: [Char] -> [Char] -> [(Token, [Char])] -> Int -> [(Token, [Char])]
assignmentDFA ('=':xs) ['='] tokens line = entrypoint xs [] (tokens ++ [(EqualityToken, "==")]) line
assignmentDFA ('=':xs) ['>'] tokens line = entrypoint xs [] (tokens ++ [(GreaterEqualToken, ">=")]) line
assignmentDFA ('=':xs) ['<'] tokens line = entrypoint xs [] (tokens ++ [(LessEqualToken, "<=")]) line
assignmentDFA ('=':xs) ['!'] tokens line = entrypoint xs [] (tokens ++ [(NotEqualToken, "!=")]) line
assignmentDFA ('>':xs) [] tokens line = assignmentDFA xs ['>'] tokens line
assignmentDFA ('<':xs) [] tokens line = assignmentDFA xs ['<'] tokens line
assignmentDFA ('=':xs) [] tokens line = assignmentDFA xs ['='] tokens line
assignmentDFA ('!':xs) [] tokens line = assignmentDFA xs ['!'] tokens line
assignmentDFA xs ['<'] tokens line = entrypoint xs [] (tokens ++ [(LessnessToken, "<")]) line
assignmentDFA xs ['>'] tokens line = entrypoint xs [] (tokens ++ [(GreatnessToken, ">")]) line
assignmentDFA xs ['='] tokens line = entrypoint xs [] (tokens ++ [(AttributionToken, "=")]) line
assignmentDFA xs ['!'] tokens line = entrypoint xs [] (tokens ++ [(NotToken, "!")]) line
assignmentDFA _ _ _ line = error ("Unknown error in line: " ++ show line)
