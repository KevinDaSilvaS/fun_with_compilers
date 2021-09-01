module Klang.RunProject where

import Klang.SemanticAnalyserKlang

import Klang.IRBuilder
import Klang.SintaticAnalyserKlang
import Klang.CodeGenerationKlang
import Klang.LexicalAnalyserKlang
import Klang.IRBuilderKlang

main = do
    let t = startSintaticAnalysis "let v := 32 let a := v let r := \"oi\"" 1 0 
    let pt = createParseTree t
    --let ir = startSemanticAnalysis ([], []) pt
    --makeFile pt
    print "end"

sintaticAnalysisOnly = 
    startSintaticAnalysis "let v := 32 + 2 - 1 * 3 let a := v / 2" 1 0 

sintaticAndIr = do
    let t = startSintaticAnalysis "let v := 32 + 2 - 1 * 3 let a := v / 2" 1 0
    let pt = createKlangParseTree t
    print pt
    let ir = startSemanticAnalysis [] pt
    print $ show ir

getAllTokens = do 
    let t = getToken "let v := 32 + 2 - 1 * 3 let a := v / 2 let id := \"oi\" + 2" 1 0 
    print t

getToken [] l c = []
getToken xs l c = token:getToken program line col
    where
        (token, program, line, col) = startAutomaton xs line col []
