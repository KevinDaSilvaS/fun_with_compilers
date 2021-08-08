module TokensJson where

data TokenJson = 
            OpenObjToken | 
            CloseObjToken | 
            IdentifierKeyToken |
            StringToken |
            NumberToken |
            OpenArrayToken |
            CloseArrayToken |
            SeparatorToken
            deriving(Show)