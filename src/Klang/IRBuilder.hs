{-# LANGUAGE DataKinds #-}

module Klang.IRBuilder where

import Klang.SintaticAnalyserKlang
import Klang.TokensKlang

data ParsingTree = Program ParsingTree ParsingTree | 
                   Assign (Tokens, [Char]) (Tokens, [Char]) ParsingTree |
                   EndNode
                   deriving (Show)

createParseTree [] = EndNode
createParseTree ((IdentifierToken, value):xs) = Program assign program
    where
        assign  = Assign (IdentifierToken, value) token EndNode
        program = createParseTree remain
        (token, remain) = extractValue xs
createParseTree _ = error "Expecting let flow"

extractValue ((IntegerToken, value):xs)    = ((IntegerToken, value), xs)
extractValue ((StringToken, value):xs)     = ((StringToken, value), xs)
extractValue ((IdentifierToken, value):xs) = ((IdentifierToken, value), xs)
extractValue _  = error "Expecting value for IR"