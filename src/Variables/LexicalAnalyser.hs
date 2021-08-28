module Variables.LexicalAnalyser where

import Variables.Tokens 

integers :: [Char]
integers = ['0'..'9']

identifiers :: [Char]
identifiers = '_':['a'..'z']
    
mainAutomaton :: [Char] -> ([Char], (VarTokens, [Char]))
mainAutomaton ('=':xs) = (xs, (AssignToken, "="))
mainAutomaton ('_':xs) = mainAutomaton xs
mainAutomaton xs = integerAutomaton xs []
    
integerAutomaton :: [Char] -> [Char] -> ([Char], (VarTokens, [Char]))
integerAutomaton [] reading = ([], (NumberToken, reading))
integerAutomaton (x:xs) reading
    | x `elem` integers = integerAutomaton xs (reading ++ [x])
    | otherwise         = ((x:xs), (NumberToken, reading))

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x   