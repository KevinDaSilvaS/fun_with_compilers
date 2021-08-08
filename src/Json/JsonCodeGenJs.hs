module JsonCodeGenJs where
import System.IO

genJS ir = do 
    let lb = map addLB ir
    let contents = "module.exports = "
                    ++ concat lb
                    ++ ";"
    writeFile "../../test.js" contents

addLB x
    | x == "{" || x == "," =  x++"\n"
    | x == "}" = "\n"++x
    | otherwise = x