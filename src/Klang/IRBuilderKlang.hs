{-# LANGUAGE DataKinds #-}

module Klang.IRBuilderKlang where

import Klang.TokensKlang
import Klang.KlangSets

data Expr = Expr Expr  |
            Integer (Tokens, String) Expr  |
            Str (Tokens, String)           |
            Operator (Tokens, String) Expr |
            ComparativeExpr Expr (Tokens, String) Expr |
            EndExpr deriving (Show, Eq)

data ParsingTree = Program ParsingTree |
                   Assign (Tokens, String) Expr ParsingTree |
                   Show (Tokens, String) Expr ParsingTree   |
                   If (Tokens, String)
                        Expr (Tokens, String) ParsingTree   |
                   CloseBlock (Tokens, String) ParsingTree     |
                   EndNode
                   deriving (Show, Eq)

createKlangParseTree [] = EndNode
createKlangParseTree ((LetToken, value):xs) = createKlangParseTree xs
createKlangParseTree ((StartBlockToken, value):xs) =
    error "Unexpected open block symbol"
createKlangParseTree ((CloseBlockToken, ";"):xs) =
    CloseBlock (CloseBlockToken, ";") (createKlangParseTree xs)
createKlangParseTree ((ConditionalToken, value):xs) =
    If (ConditionalToken, value)
        expr'
        openBlock
        (createKlangParseTree (tail remain'))
    where
        (expr, remain) = parseExpression xs
        (expr', remain') = parseComparativeExpressions expr remain
        openBlock = head remain'
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
        (Integer (IdentifierToken, value) expr,
        remain)
    where
        (expr, remain) = parseExpression xs
parseExpression ((t, "*"):xs) =
        (Operator (t, "*") expr,
        remain)
    where
        (expr, remain) = parseExpression xs
parseExpression ((t, "/"):xs) =
    (Operator (t, "/") expr,
    remain)
    where
        (expr, remain) = parseExpression xs
parseExpression ((t, "+"):xs) =
    (Operator (t, "+") expr,
    remain)
    where
        (expr, remain) = parseExpression xs
parseExpression ((t, "-"):xs) =
    (Operator (t, "-") expr,
    remain)
    where
        (expr, remain) = parseExpression xs
parseExpression ((StringToken, value):xs) =
    parseComparativeExpressions
    (Str (StringToken, value))
    remain
    where
        (expr, remain) = parseExpression xs
parseExpression xs = (EndExpr, xs)

parseComparativeExpressions expr ((token, ">="):xs) =
    (ComparativeExpr expr (token, ">=") expr', remain')
    where
        (expr', remain) = parseExpression xs
        remain' = sanitizeExtraComparativeExpression remain
parseComparativeExpressions expr ((token, ">"):xs)  =
    (ComparativeExpr expr (token, ">") expr', remain')
    where
        (expr', remain) = parseExpression xs
        remain' = sanitizeExtraComparativeExpression remain
parseComparativeExpressions expr ((token, "<="):xs) =
    (ComparativeExpr expr (token, "<=") expr', remain')
    where
        (expr', remain) = parseExpression xs
        remain' = sanitizeExtraComparativeExpression remain
parseComparativeExpressions expr ((token, "<"):xs)  =
    (ComparativeExpr expr (token, "<") expr', remain')
    where
        (expr', remain) = parseExpression xs
        remain' = sanitizeExtraComparativeExpression remain
parseComparativeExpressions expr ((token, "!="):xs) =
    (ComparativeExpr expr (token, "!=") expr', remain')
    where
        (expr', remain) = parseExpression xs
        remain' = sanitizeExtraComparativeExpression remain
parseComparativeExpressions expr ((token, "=="):xs) =
    (ComparativeExpr expr (token, "==") expr', remain')
    where
        (expr', remain) = parseExpression xs
        remain' = sanitizeExtraComparativeExpression remain
parseComparativeExpressions expr xs = (expr, xs)

sanitizeExtraComparativeExpression ((token, value):xs)
    | value `elem` comparativeOperators = 
        error 
        ("Cannot compare String or Int type with Boolean comparison: " 
        ++ value ++ " in: " ++ show xs) 
sanitizeExtraComparativeExpression xs = xs