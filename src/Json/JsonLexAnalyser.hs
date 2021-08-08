module JsonLexAnalyser where

import TokensJson
--rainbow
numbers = ['0'..'9']

entrypoint ('\n':xs) reading tokens line = entrypoint xs reading tokens (line+1)
entrypoint (' ':xs) reading tokens line = entrypoint xs reading tokens line
entrypoint ('{':xs) [] tokens line = entrypoint xs [] (tokens++[token]) line
    where
        token = (OpenObjToken, "{")
entrypoint ('}':xs) [] tokens line = entrypoint xs [] (tokens++[token]) line
    where
        token = (CloseObjToken, "}")
entrypoint (',':xs) [] tokens line = entrypoint xs [] (tokens++[token]) line
        where
            token = (SeparatorToken, ",")
entrypoint ('"':xs) [] tokens line = keyAutomata xs ['"'] tokens line
{- entrypoint ('t':xs) [] tokens line = booleanAutomata xs ['t'] tokens line
entrypoint ('f':xs) [] tokens line = booleanAutomata xs ['f'] tokens line -}
entrypoint [] [] tokens _ = tokens
entrypoint [] reading _ line = error ("Invalid closing symbol in line:"++ show line)
{- entrypoint (x:xs) reading tokens line 
    | x `elem` numbers = numberAutomata xs [x] tokens line
    | otherwise = error $ ("Cant recognize symbol: " ++ [x] ++ " in line:"++ show line) -}
entrypoint _ _ _ line = error ("Unknown error in line: " ++ show line)

keyAutomata('\n':xs) reading tokens line = error ("Cant have line break in json token" ++ reading ++ " in line:"++ show line)
keyAutomata ('"':xs) reading tokens line = keyAutomataSnd xs (reading++['"']) tokens line
keyAutomata (x:xs) reading tokens line = keyAutomata xs (reading++[x]) tokens line
keyAutomata [] reading tokens line = entrypoint [] reading tokens line

keyAutomataSnd [] reading tokens line = entrypoint [] [] (tokens ++ [token]) line
    where
        token = (StringToken, reading)
keyAutomataSnd(':':xs) reading tokens line = entrypoint xs [] (tokens ++ [token]) line
    where
        finalReading = reading++[':']
        token = (IdentifierKeyToken, finalReading)
keyAutomataSnd(x:xs) reading tokens line = entrypoint (x:xs) [] (tokens ++ [token]) line
    where
        token = (StringToken, reading)



