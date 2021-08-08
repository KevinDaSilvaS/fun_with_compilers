module JsonSintaticAnalyser where

import TokensJson

_A [] 0 = True
_A ((CloseObjToken, x):xs) stackObjs
    | null xs = _A xs (stackObjs-1)
    | snd nextToken == "," = _E xs (stackObjs-1)
    | snd nextToken == "}" = _A xs (stackObjs-1)
    | otherwise = error ("Unexpected token " ++ show nextToken)
    where
        nextToken = head xs
_A ((OpenObjToken, x):xs) stackObjs
    | null xs = error "Unexpected empty _A2"
    | snd nextToken == "}" = _E xs (stackObjs+1)
    | last(snd nextToken) == ':' = _K xs (stackObjs+1)
    | otherwise = error ("Unexpected token " ++ show nextToken)
    where
        nextToken = head xs
_A xs v = error ("unexpected condition _A " ++ show xs ++ " and " ++ show v)

_K ((IdentifierKeyToken, x):xs) stackObjs = _T xs stackObjs
_K xs stackObjs = error "Expecting identifier"

_T ((StringToken, x):xs) stackObjs  = _E xs stackObjs 
_T ((OpenObjToken, x):xs) stackObjs = _A ((OpenObjToken, x):xs) stackObjs 
_T (x:_) _ = error ("Unexpected Token _T " ++ show x ++ " Value or Object expected")
_T xs _ = error ("Unexpected condition _T "++ show xs)

_E ((SeparatorToken, x):xs) stackObjs = _K xs stackObjs
_E ((CloseObjToken, x):xs) stackObjs = _A ((CloseObjToken, x):xs) stackObjs
_E [] stackObjs = _A [] stackObjs
_E xs _ = error ("Unexpected condition _E "++ show xs)