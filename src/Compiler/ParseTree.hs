module Compiler.ParseTree where

import Compiler.TokensKlang
import Compiler.KlangSets
    ( arithmeticOperators, comparativeOperators )

data Expr = Value (TokensKlang, String) |
            Arithmetic  Expr (TokensKlang, String) Expr |
            Comparative Expr (TokensKlang, String) Expr |
            EndExpr deriving(Show, Eq)

data ParsingTree = Assign (TokensKlang, String) Expr             |
                   If Expr (TokensKlang, String)                 |
                   CloseBlock (TokensKlang, String)              |
                   Routine (TokensKlang, String)
                        (TokensKlang, String) ParsingTree        |
                   Show (TokensKlang, String) Expr               |
                   EndNode deriving(Show, Eq)


createParseTree (x:xs)
     | fst x == LetToken  =
          Assign identifierName expr     : createParseTree remain
     | fst x == ShowToken = Show x expr' : createParseTree remain'
     | fst x == IfToken   = If expr' openBlock : createParseTree (tail remain')
     | fst x == CloseBlockToken = CloseBlock x : createParseTree remain'
     | otherwise = [EndNode]
     where
          identifierName = head xs
          (expr, remain) = parseExpr (tail xs)
          (expr', remain') = parseExpr xs
          openBlock = head remain'
createParseTree _ = [EndNode]

parseExpr []  = (EndExpr, [])
parseExpr [x] = (Value x, [])
parseExpr (x:xs)
     | snd nextToken `elem` arithmeticOperators =
          (Arithmetic (Value x) nextToken expr, remain)
     | snd nextToken `elem` comparativeOperators =
          (Comparative (Value x) nextToken expr, remain)
     | otherwise = (Value x, xs)
     where
          nextToken = head xs
          (expr, remain) = parseExpr (tail xs)
