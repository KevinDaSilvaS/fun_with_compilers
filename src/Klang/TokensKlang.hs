module Klang.TokensKlang where

data Tokens = LetToken         | 
              IdentifierToken  |
              AssignToken      |
              SumToken         |
              DivisionToken    |
              MultToken        |
              SubToken         |
              LessToken        |
              LessThanToken    |
              GreaterToken     |
              GreaterThanToken |
              EqualityToken    |
              NotEqualToken    |
              ConditionalToken |
              StartBlockToken  |
              CloseBlockToken  |
              OpenParenToken   |
              CloseParenToken  |
              OpenArrayToken   |
              CloseArrayToken  |
              SeparatorToken   |
              IntegerToken     |
              StringToken      |
              ShowToken        deriving (Show, Eq)