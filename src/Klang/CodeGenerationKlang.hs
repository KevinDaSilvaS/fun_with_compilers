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
generateFile st (Show (_, _) expr ptr) = consoleLog ++ generateFile st ptr
    where
    consoleLog = "console.log( " ++ getContextExpr expr ++ " );\n"
generateFile st (If _ expr openblock ptr) = ifCondition
    where
        ifCondition = "if ( " ++ getContextExpr expr ++" ) { \n" 
            ++ generateFile st ptr ++ "\n"
generateFile st (CloseBlock _ ptr) = "}\n" ++ generateFile st ptr
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

getContextExpr :: Expr -> [Char]
getContextExpr EndExpr = ""
getContextExpr (Integer (_, value) expr) = value ++ getContextExpr expr
getContextExpr (Operator (_, value) expr) = 
    " " ++ value ++ " " ++ getContextExpr expr
getContextExpr (ComparativeExpr expr' (_, value) expr) = 
    "(" ++ getContextExpr expr' ++ ") " ++ value 
    ++ " (" ++ getContextExpr expr ++ ")"
getContextExpr (Str (_, value)) = value
getContextExpr _ = error "Type of Expr not expected"