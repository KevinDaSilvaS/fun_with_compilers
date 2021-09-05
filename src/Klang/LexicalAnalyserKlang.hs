module Klang.LexicalAnalyserKlang where

import Klang.TokensKlang

import Klang.KlangSets

startAutomaton :: [Char] -> Int -> Int -> [Char] -> ((Tokens, [Char]), [Char], Int, Int)
startAutomaton (':':xs) line col [] = assignAutomaton xs line col ":"
startAutomaton ('"':xs) line col [] = stringAutomaton xs line col "\""
startAutomaton ('l':xs) line col [] = letAutomaton xs line col "l"
startAutomaton ('s':xs) line col [] = showAutomaton xs line col "s"
startAutomaton ('i':xs) line col [] = ifAutomaton xs line col "i"
startAutomaton ('r':xs) line col [] = routineAutomaton xs line col "r"
startAutomaton (';':xs) line col [] = ((CloseBlockToken, ";"), xs, line, col+1)
startAutomaton ('+':xs) line col [] = ((SumToken, "+"), xs, line, col+1)
startAutomaton ('-':xs) line col [] = ((SubToken, "-"), xs, line, col+1)
startAutomaton ('*':xs) line col [] = ((MultToken, "*"), xs, line, col+1)
startAutomaton ('/':xs) line col [] = ((DivisionToken, "/"), xs, line, col+1)
startAutomaton ('>':xs) line col [] = comparisonAutomaton xs line col ">"
startAutomaton ('<':xs) line col [] = comparisonAutomaton xs line col "<"
startAutomaton ('!':xs) line col [] = comparisonAutomaton xs line col "!"
startAutomaton ('=':xs) line col [] = comparisonAutomaton xs line col "="
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
assignAutomaton xs line col reading   = ((StartBlockToken, ":"), xs, line, col+1)

integerAutomaton (x:xs) line col reading
    | x `elem` integers = integerAutomaton xs line (col+1) (reading++[x])
    | x `elem` endInput = ((IntegerToken, reading), (x:xs), line, col)
    | otherwise = error ("Unexpected token " ++ show x ++ " in line:" 
    ++ show line ++ " col:" ++ show col ++ " while reading:" ++ show reading)
integerAutomaton [] line col reading = ((IntegerToken, reading), [], line, col)

stringAutomaton [] line col reading = error ("Unexpected EOF in line:" 
    ++ show line ++ " col:" ++ show col ++ " while reading:" ++ show reading)
stringAutomaton (x:xs) line col reading
    | x == '"'  = ((StringToken, reading++"\""), xs, line, col+1)
    | otherwise = stringAutomaton xs line (col+1) (reading++[x])

showAutomaton ('h':xs) line col "s"   = showAutomaton xs line (col+1) "sh"
showAutomaton ('o':xs) line col "sh"  = showAutomaton xs line (col+1) "sho"
showAutomaton ('w':xs) line col "sho" = showAutomaton xs line (col+1) "show"
showAutomaton (' ':xs) line col "show" = ((ShowToken, "show"), xs, line, col+1)
showAutomaton [] line col "show"       = ((ShowToken, "show"), [], line, col)
showAutomaton xs line col reading     = 
        identifierAutomaton xs line col reading

ifAutomaton ('f':xs) line col "i" = ((ConditionalToken, "if"), xs, line, col+1)
ifAutomaton xs line col reading   = identifierAutomaton xs line col reading

routineAutomaton ('o':xs) line col "r" = 
    routineAutomaton xs line (col+1) "ro"
routineAutomaton ('u':xs) line col "ro" = 
    routineAutomaton xs line (col+1) "rou"
routineAutomaton ('t':xs) line col "rou" = 
    routineAutomaton xs line (col+1) "rout"
routineAutomaton ('i':xs) line col "rout" = 
    routineAutomaton xs line (col+1) "routi"
routineAutomaton ('n':xs) line col "routi" = 
    routineAutomaton xs line (col+1) "routin"
routineAutomaton ('e':xs) line col "routin" = 
    ((RoutineToken, "routine"), xs, line, col+1)
routineAutomaton xs line col reading   = 
    identifierAutomaton xs line col reading

comparisonAutomaton ('=':xs) line col "<" = 
    ((LessThanToken, "<="), xs, line, col+1)
comparisonAutomaton xs line col "<"       = 
    ((LessToken, "<"), xs, line, col+1)
comparisonAutomaton ('=':xs) line col ">" = 
    ((GreaterThanToken, ">="), xs, line, col+1)
comparisonAutomaton xs line col ">"       = 
    ((GreaterToken, ">"), xs, line, col+1)
comparisonAutomaton ('=':xs) line col "=" = 
    ((EqualityToken, "=="), xs, line, col+1)
comparisonAutomaton ('=':xs) line col "!" = 
    ((NotEqualToken, "!="), xs, line, col+1)
comparisonAutomaton _ line col reading    = error 
    ("Error unexpected symbol: " ++ " in line: " 
    ++ show line ++ " col: " ++ show col ++ "while reading: " ++ reading)