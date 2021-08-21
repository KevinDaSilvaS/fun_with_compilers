module Calculator.Infix.CalculatorInfixTokens where

data CalcTokens = IntegerToken           |
                  SumToken               |
                  MinusToken             |
                  MultiplicationToken    |
                  DivisionToken   deriving(Show)  

data List a = Node a (List a) | Empty deriving (Show)