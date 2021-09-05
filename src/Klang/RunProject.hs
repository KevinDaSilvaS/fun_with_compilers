module Klang.RunProject where

import Klang.SemanticAnalyserKlang

import Klang.IRBuilder
import Klang.SintaticAnalyserKlang
import Klang.CodeGenerationKlang
import Klang.LexicalAnalyserKlang
import Klang.IRBuilderKlang
import Klang.TokensKlang

main = do
    let t = startSintaticAnalysis "let v := 32 let a := v let r := \"oi\"" 1 0 0
    let pt = createParseTree t
    --let ir = startSemanticAnalysis ([], []) pt
    --makeFile pt
    print "end"

sintaticAnalysisOnly = 
    startSintaticAnalysis "let v := 32 + 2 - 1 * 3 show v + a let a := v / 2" 1 0 

mainJson = do
        contents <- readFile "./test.klang"
        if null contents then
          print ""
        else do
          let result = startSintaticAnalysis (head contents : tail contents) 1 0 0
          let pt = createKlangParseTree result
          let st = startSemanticAnalysis [("_GET_CURR_INDEX", (IntegerToken, "0"))] pt
          print pt
          makeFile st pt
          return ()

sintaticAndIr = do
    let t = startSintaticAnalysis " routine 20 : ; let v_ := \"32\" if v_ < \"2 + 3\" : if 6 > 2 : show 5 ; show 4 ; let v := 32 + 2 - 1 * 3 let a := v / 2 let r := \"oi\" show 9 > 2 show v + a show \"ola\" let s := v + a" 1 0 0
        {- let v := 32 + 2 - 1 * 3 let a := v / 2 let r := \"oi\" show 9 + 2 show v + a show \"ola\" let s := v + a -}
    let pt = createKlangParseTree t
    print pt
    let st = startSemanticAnalysis [("_GET_CURR_INDEX", (IntegerToken, "0"))] pt
    print $ show st
    makeFile st pt
    --print t

getAllTokens = do 
    let t = getToken "routine 20 : ; if id : ;  > < >= != == <= let v := 32 + 2 - 1 * 3 show v  let a := v / 2 let id := \"oi\" + 2" 1 0 
    print t

getToken [] l c = []
getToken xs l c = token:getToken program line col
    where
        (token, program, line, col) = startAutomaton xs line col []