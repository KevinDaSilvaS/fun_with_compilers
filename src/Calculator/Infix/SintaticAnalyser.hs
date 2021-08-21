module Calculator.Infix.SintaticAnalyser where

import Calculator.Infix.CalculatorInfixTokens 

import Calculator.Infix.LexicalAnalyser ( mainAutomaton )

--data ParseTree a = Value a | Calc (ParseTree a) a (ParseTree a)

--data LL a = Value1 a (LL a) 

operators :: [[Char]]
operators = ["+", "/", "*", "-"]

startSintaticAnalysis :: [Char] -> List [Char]
startSintaticAnalysis [] = error "Unexpect empty" 
startSintaticAnalysis program 
    | (snd token) `elem` operators = error ("Should start with number" ++ show token)
    | otherwise = Node (snd token) (_O ls)
    where
       (ls, token) = mainAutomaton program
    
    --getAllTokens
_O :: [Char] -> List [Char]
_O [] = Empty
_O program  
    | (snd token) `elem` operators = Node (snd token) (_N ls)
    | otherwise = error "Unexpected number"
    where
       (ls, token) = mainAutomaton program

_N :: [Char] -> List [Char]
_N [] = error "Expecting number, but got eof"
_N program 
    | (snd token) `elem` operators = error ("Unexpected operator"++ show token)
    | otherwise = Node (snd token) (_O ls) 
    where
        (ls, token) = mainAutomaton program

getAllTokens :: [Char] -> [(CalcTokens, [Char])]
getAllTokens [] = []
getAllTokens program = token : getAllTokens ls
    where
       (ls, token) = mainAutomaton program