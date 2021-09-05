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
              RoutineToken     |
              StartBlockToken  |
              CloseBlockToken  |
              OpenParenToken   |
              CloseParenToken  |
              OpenArrayToken   |
              CloseArrayToken  |
              SeparatorToken   |
              IntegerToken     |
              StringToken      |
              ShowToken        |
              EmptyToken       deriving (Show, Eq)