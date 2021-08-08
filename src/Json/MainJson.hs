module MainJson where

import Control.Monad.Trans (liftIO)
import System.IO
import JsonLexAnalyser ( entrypoint )
import JsonSintaticAnalyser ( _A )
import JsonSemanticAnalyser ( start )
import JsonIR
import JsonCodeGenJs
import JsonCodeGenYml

mainJson :: IO ()
mainJson = do
  contents <- readFile "../../test.json"
  if null contents then
    print ""
  else do
    let result = entrypoint (head contents : tail contents) [] [] 1
    let v = _A result 0
    let a = start result
    print ("V IS:", show v)
    print ("Semantic is:", show a)
    let ir = buildIr result
    --print ("IR", buildIr result)
    genJS ir
    genYml ir
    --print result
    return ()