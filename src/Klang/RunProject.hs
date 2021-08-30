module Klang.RunProject where

import Klang.SemanticAnalyserKlang

import Klang.IRBuilder
import Klang.SintaticAnalyserKlang
import Klang.CodeGenerationKlang
import Klang.LexicalAnalyserKlang

main = do
    let t = startSintaticAnalysis "let v := 32 let a := v" 1 0 
    let pt = createParseTree t
    let ir = startSemanticAnalysis ([], []) pt
    makeFile ir

getAllTokens = do 
    let t = getToken "let v := 32 + 2 - 1 * 3 let a := v / 2" 1 0 
    print t

getToken [] l c = []
getToken xs l c = token:getToken program line col
    where
        (token, program, line, col) = startAutomaton xs line col []
