module JsonIR where

import TokensJson

data Obj = Obj {
    ref :: Int,
    keys :: [(TokenJson, [Char])]
} deriving (Show)

buildIr xs = map irBuilder xs

irBuilder (OpenObjToken, x) = x
irBuilder (IdentifierKeyToken, x) = (init $ init $ drop 1 x) ++ [last x]
irBuilder (StringToken, x) = x
irBuilder (SeparatorToken, x) = x
irBuilder (CloseObjToken, x) = x
irBuilder x = error ("Unexpected " ++ show x)