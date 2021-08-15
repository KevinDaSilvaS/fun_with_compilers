module Calculator.Infix.SintaticAnalyser where

import Calculator.Infix.CalculatorInfixTokens ( CalcTokens(..) )

import Calculator.Infix.LexicalAnalyser ( mainAutomaton )

data ParseTree a = Value a | Calc (ParseTree a) a (ParseTree a)

--data LL a = Value1 a (LL a) 

startSintaticAnalysis = getAllTokens

getAllTokens [] = []
getAllTokens program = token : getAllTokens ls
    where
       (ls, token) = mainAutomaton program