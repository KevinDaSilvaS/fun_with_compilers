module Klang.RunProject where

import Klang.SemanticAnalyserKlang

import Klang.IRBuilder
import Klang.SintaticAnalyserKlang
import Klang.CodeGenerationKlang

main = do
    let t = startSintaticAnalysis "let v := 32 let a := v" 1 0 
    let pt = createParseTree t
    let ir = startSemanticAnalysis ([], []) pt
    makeFile ir