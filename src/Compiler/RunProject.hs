module Compiler.RunProject where

import Compiler.LexicalAnalyser ( startAutomaton )
import Compiler.TokensKlang
import Compiler.SintaticAnalyser
import Compiler.ParseTree

getAllTokens = do
    --let t = parseResult (startAutomaton "#let $a := 12 + * - / \"oi\"" 1 0 [])
    let t = getToken "let a := 12 + * - / \"oi\" : ; >= if > < != == <= routine if_ t5" 1 0
    print t

getToken [] l c = []
getToken xs l c = (token, value):getToken program line col
    where
        ((token, value), line, col, program) =  startAutomaton xs line col []

sintaticAnalysis = do
    let program = "let a := 12 / 2 - readLine if a <= 12-2 : routine 3*2 : show 42 ; ;"
        --"let a := 12 + * - / \"oi\" : ; >= if > < != == <= routine if_ t5"
    let r = startSintaticAnalysis program 1 0 0
    print r

parseTree = do
    let program = "let a := 12 / 2 - readLine * 3 + 2 let r := \"oi\" if a > c : show a + 23 ;"
    let r = startSintaticAnalysis program 1 0 0
    let tree = createParseTree r
    print tree