{-# LANGUAGE DataKinds #-}

module Klang.IRBuilder where

import Klang.SintaticAnalyserKlang
import Klang.TokensKlang

import Klang.KlangSets

data ParsingTree = Program ParsingTree ParsingTree | 
                   Assign (Tokens, [Char]) (Tokens, [Char]) ParsingTree |
                   Arithmetic (Tokens, [Char]) (Tokens, [Char]) ParsingTree |
                   EndNode
                   deriving (Show, Eq)

                   {- Program 
                    (Assign (IdentifierToken,"v") (IntegerToken,"32") EndNode) 
                    (Arithmetic 
                        (SumToken,"+") (IntegerToken,"2") 
                        (Arithmetic (SubToken,"-") (IntegerToken,"1") 
                            (Arithmetic (MultToken,"*") (IntegerToken,"3") 
                   (Program (Assign (IdentifierToken,"a") (IdentifierToken,"v") EndNode) 
                    (Arithmetic (DivisionToken,"/") (IntegerToken,"2") EndNode))))) -}

createParseTree [] = EndNode
createParseTree ((IdentifierToken, value):xs) 
    | nextTokenValue `elem` arithmeticOperators 
    && fst token == IntegerToken = Program assign program
    | nextTokenValue `elem` arithmeticOperators 
    && fst token /= IntegerToken = error ("Cannot perform arithmetic operation:" 
        ++ nextTokenValue ++ " in a non integer identifier: "++ value) 
    | otherwise = Program assign program
    where
        assign          = Assign (IdentifierToken, value) token EndNode
        program         = createParseTree remain
        (token, remain) = extractValue xs
        ((_, nextTokenValue), _) = extractValue xs
createParseTree ((token, value):xs) 
    | value `elem` arithmeticOperators = 
        Arithmetic (token, value) (nToken, nValue) (createParseTree remain)
    | otherwise = error "Expecting let flow"
    where
        ((nToken, nValue), remain) = extractValue xs

extractValue ((SumToken, value):xs)        = ((SumToken, value), xs)
extractValue ((SubToken, value):xs)        = ((SubToken, value), xs)
extractValue ((MultToken, value):xs)       = ((MultToken, value), xs)
extractValue ((DivisionToken, value):xs)   = ((DivisionToken, value), xs)
extractValue ((IntegerToken, value):xs)    = ((IntegerToken, value), xs)
extractValue ((StringToken, value):xs)     = ((StringToken, value), xs)
extractValue ((IdentifierToken, value):xs) = ((IdentifierToken, value), xs)
extractValue _  = error "Expecting value for IR"