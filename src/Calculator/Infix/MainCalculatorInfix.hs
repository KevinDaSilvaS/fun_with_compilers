module Calculator.Infix.MainCalculatorInfix where

import Calculator.Infix.SintaticAnalyser ( startSintaticAnalysis )
import Calculator.Infix.SemanticAnalyser ( semanticAnalysis )

main = do
    let list = startSintaticAnalysis "30+1*2+3/3-1"
    let final = semanticAnalysis list 0
    print list
    print final