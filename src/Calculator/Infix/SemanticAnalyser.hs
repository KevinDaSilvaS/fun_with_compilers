module Calculator.Infix.SemanticAnalyser where

import Calculator.Infix.CalculatorInfixTokens 

semanticAnalysis :: List [Char] -> Float -> Float
semanticAnalysis (Node "*" nxt) previous = semanticAnalysis nnxt (intX * intY)
    where
        intX = previous
        (Node y nnxt) = nxt
        intY = read y :: Float
semanticAnalysis (Node "/" nxt) previous 
    | intY == 0 = error "Arithmetic error cant perform division by zero"
    | otherwise = semanticAnalysis nnxt (intX / intY)
    where
        intX = previous
        (Node y nnxt) = nxt
        intY = read y :: Float
semanticAnalysis (Node "-" nxt) previous = 
    previous - semanticAnalysis nxt previous
semanticAnalysis (Node "+" nxt) previous = 
    previous + semanticAnalysis nxt previous
semanticAnalysis (Node x nxt) previous   = semanticAnalysis nxt intX
    where
        intX = read x :: Float
semanticAnalysis Empty previous = previous
