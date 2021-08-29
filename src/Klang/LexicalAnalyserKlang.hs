module Klang.LexicalAnalyserKlang where

import Klang.TokensKlang

import Klang.KlangSets

startAutomaton :: [Char] -> Int -> Int -> [Char] -> ((Tokens, [Char]), [Char], Int, Int)
startAutomaton (':':xs) line col [] = assignAutomaton xs line col ":"
startAutomaton ('"':xs) line col [] = stringAutomaton xs line col "\""
startAutomaton ('l':xs) line col [] = letAutomaton xs line col "l"
startAutomaton (x:xs) line col reading 
    | x `elem` integers   = integerAutomaton xs line col [x]
    | x `elem` lineBreaks = startAutomaton xs (line+1) col []
    | x `elem` spaces     = startAutomaton xs line (col+1) []
startAutomaton [] line col reading = ((EmptyToken, ""), [], line, col)
startAutomaton xs line col []      = identifierAutomaton xs line col []
startAutomaton _ line col reading  = 
    error ("Unexpected token in line:" 
    ++ show line ++ " col:" ++ show col ++ " while reading:" ++ show reading)

letAutomaton ('e':xs) line col "l"   = letAutomaton xs line (col+1) "le"
letAutomaton ('t':xs) line col "le"  = letAutomaton xs line (col+1) "let"
letAutomaton (' ':xs) line col "let" = ((LetToken, "let"), xs, line, col+1)
letAutomaton [] line col "let"       = ((LetToken, "let"), [], line, col)
letAutomaton xs line col reading     = 
    identifierAutomaton xs line col reading

identifierAutomaton (x:xs) line col reading
    | x `elem` endInput = 
        ((IdentifierToken, reading), (x:xs), line, col)
    | x `elem` identifierSet = 
        identifierAutomaton xs line (col+1) (reading++[x])
    | otherwise = error ("Unexpected token " ++ show x ++ " in line:" 
    ++ show line ++ " col:" ++ show col ++ " while reading:" ++ show reading)
identifierAutomaton [] line col reading = 
    ((IdentifierToken, reading), [], line, col)

assignAutomaton ('=':xs) line col ":" = 
    ((AssignToken, ":="), xs, line, col+1)
assignAutomaton _ line col reading   = 
    error ("Unexpected token in line:" 
    ++ show line ++ " col:" ++ show col ++ " while reading:" ++ show reading)

integerAutomaton (x:xs) line col reading
    | x `elem` integers = integerAutomaton xs line (col+1) (reading++[x])
    | x `elem` endInput = ((IntegerToken, reading), (x:xs), line, col)
    | otherwise = error ("Unexpected token " ++ show x ++ " in line:" 
    ++ show line ++ " col:" ++ show col ++ " while reading:" ++ show reading)
integerAutomaton [] line col reading = ((IntegerToken, reading), [], line, col)

stringAutomaton [] line col reading = error ("Unexpected EOF in line:" 
    ++ show line ++ " col:" ++ show col ++ " while reading:" ++ show reading)
stringAutomaton (x:xs) line col reading
    | x == '"'  = ((StringToken, reading), [], line, col+1)
    | otherwise = stringAutomaton xs line (col+1) (reading++[x])
