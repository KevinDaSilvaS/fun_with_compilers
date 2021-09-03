{-# LANGUAGE DataKinds #-}

module Klang.IRBuilderKlang where

import Klang.TokensKlang

data Expr = Expr Expr  |
            Integer (Tokens, String) Expr  |
            Str (Tokens, String)           |
            Operator (Tokens, String) Expr |
            EndExpr deriving (Show, Eq)

data ParsingTree = Program ParsingTree |
                   Assign (Tokens, String) Expr ParsingTree |
                   Show (Tokens, String) Expr ParsingTree   |
                   EndNode
                   deriving (Show, Eq)

createKlangParseTree [] = EndNode
createKlangParseTree ((LetToken, value):xs)  = createKlangParseTree xs
createKlangParseTree ((ShowToken, value):xs) = 
    Show (ShowToken, value) expr (createKlangParseTree remain)
    where
        (expr, remain) = parseExpression xs
createKlangParseTree ((IdentifierToken, value):xs)
    | nextToken == IdentifierToken || nextToken == IntegerToken =
        Assign (IdentifierToken, value) 
            (Expr (Integer (nextToken, nextValue) expr))
        (createKlangParseTree remain)
    | nextToken == StringToken = Assign 
        (IdentifierToken, value) (Str (nextToken, nextValue))
        (createKlangParseTree (tail xs))
    where
        (nextToken, nextValue) = head xs
        (expr, remain) = parseExpression (tail xs)
createKlangParseTree xs = EndNode

parseExpression ((IntegerToken, value):xs)    =
    (Integer (IntegerToken, value) expr, remain)
    where
        (expr, remain) = parseExpression xs
parseExpression ((IdentifierToken, value):xs) =
    (Integer (IdentifierToken, value) expr, remain)
    where
        (expr, remain) = parseExpression xs
parseExpression ((t, "*"):xs) = (Operator (t, "*") expr, remain)
    where
        (expr, remain) = parseExpression xs
parseExpression ((t, "/"):xs) = (Operator (t, "/") expr, remain)
    where
        (expr, remain) = parseExpression xs
parseExpression ((t, "+"):xs) = (Operator (t, "+") expr, remain)
    where
        (expr, remain) = parseExpression xs
parseExpression ((t, "-"):xs) = (Operator (t, "-") expr, remain)
    where
        (expr, remain) = parseExpression xs
parseExpression ((StringToken, value):xs) = 
    (Str (StringToken, value), remain)
    where
        (expr, remain) = parseExpression xs
parseExpression xs = (EndExpr, xs)

