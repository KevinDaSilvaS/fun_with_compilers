module Tokens where

data Token = NumberToken | 
            IdentifierToken |
            EqualityToken |
            GreaterEqualToken |
            LessEqualToken |
            NotEqualToken |
            GreatnessToken |
            LessnessToken |
            NotToken |
            AttributionToken
            deriving(Show)