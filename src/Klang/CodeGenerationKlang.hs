module Klang.CodeGenerationKlang where

import System.IO
import Klang.IRBuilderKlang
import Data.Maybe (isJust, fromJust)

makeFile st ir = do
    writeFile "./klang_parsed.js" (generateFile st ir)
    return ()

generateFile st (Assign (_, id) _ ptr) = const ++ generateFile st ptr
    where
        (Just (var, value)) = lookup id st
        const = "const " ++ id ++ " = " ++ value ++ ";\n"
generateFile st _ = ""

getContext :: ([[Char]], [[Char]]) -> [[Char]]
getContext (xs, ys)
    | null xs = []
    | otherwise = 
        const:getContext (init xs, init ys)
    where
        x = last xs
        y = last ys
        const = "const " ++ x ++ " = " ++ y ++ ";\n"