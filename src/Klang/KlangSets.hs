module Klang.KlangSets where

--import Klang.LexicalAnalyserKlang

{- getAllTokens [] _ _ = []
getAllTokens program line col = token:getAllTokens remain nline ncol
    where
        (token, remain, nline, ncol) = startAutomaton program line col [] -}

identifierSet :: [Char]
identifierSet = ['A'..'z'] ++ ['_']

spaces :: [Char]
spaces = [' ']

lineBreaks :: [Char]
lineBreaks = ['\n', '\r']

endInput :: [Char]
endInput = spaces ++ lineBreaks

integers :: [Char]
integers = ['0'..'9']

arithmeticOperators = ["+", "-", "/", "*"]
        