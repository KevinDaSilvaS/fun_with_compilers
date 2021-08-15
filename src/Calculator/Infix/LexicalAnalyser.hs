module Calculator.Infix.LexicalAnalyser where

import Calculator.Infix.CalculatorInfixTokens ( CalcTokens(..) )

integers :: [Char]
integers = ['0'..'9']

mainAutomaton :: [Char] -> ([Char], (CalcTokens, [Char]))
mainAutomaton ('*':xs) = (xs, (MultiplicationToken, "*"))
mainAutomaton ('/':xs) = (xs, (DivisionToken, "/"))
mainAutomaton ('-':xs) = (xs, (MinusToken, "-"))
mainAutomaton ('+':xs) = (xs, (SumToken, "+"))
mainAutomaton (' ':xs) = mainAutomaton xs
mainAutomaton xs = integerAutomaton xs []

integerAutomaton :: [Char] -> [Char] -> ([Char], (CalcTokens, [Char]))
integerAutomaton [] reading = ([], (IntegerToken, reading))
integerAutomaton (x:xs) reading
    | x `elem` integers = integerAutomaton xs (reading ++ [x])
    | otherwise         = ((x:xs), (IntegerToken, reading))