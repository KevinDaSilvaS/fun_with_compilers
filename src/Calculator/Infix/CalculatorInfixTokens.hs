module Calculator.Infix.CalculatorInfixTokens where

data CalcTokens = IntegerToken           |
                  SumToken               |
                  MinusToken             |
                  MultiplicationToken    |
                  DivisionToken   deriving(Show)  