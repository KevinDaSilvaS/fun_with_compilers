module Klang.CodeGenerationKlang where

import System.IO

makeFile ir = do
    let contentList = getContext ir
    let contents = concat contentList
    writeFile "./klang_parsed.js" contents
    return ()

getContext :: ([[Char]], [[Char]]) -> [[Char]]
getContext (xs, ys)
    | null xs = []
    | otherwise = 
        const:getContext (init xs, init ys)
    where
        x = last xs
        y = last ys
        const = "const " ++ x ++ " = " ++ y ++ ";\n"