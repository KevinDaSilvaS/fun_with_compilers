module JsonSemanticAnalyser where

import TokensJson

start tokens = thd
    where
        (_, _, thd) = lookKeys tokens [] []

lookKeys [] _ warnings = ([], [], warnings)
lookKeys ((OpenObjToken, _):xs) scope warnings = lookKeys first scope thd
    where 
        (first, _, thd) = lookKeys xs [] warnings
lookKeys ((CloseObjToken, _):xs) scope warnings = (xs, scope, warnings)
lookKeys ((IdentifierKeyToken, x):xs) scope warnings 
    | x `elem` scope = lookKeys xs scope (warnings++[x++" is declared twice"])
    | otherwise = lookKeys xs (scope++[x]) warnings
lookKeys (_:xs) scope warnings = lookKeys xs scope warnings