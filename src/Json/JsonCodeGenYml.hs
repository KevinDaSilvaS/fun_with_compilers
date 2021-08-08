module JsonCodeGenYml where
import System.IO

genYml ir = do 
    let lb = formatYml ir ""
    writeFile "../../test.yml" lb

formatYml (x:xs) spacing
    | x == "{" = formatYml xs (spacing++"   ")
    | x == "}" = "\n"++formatYml xs (drop 1 spacing)
    | last x == ':' = "\n"++spacing++x++formatYml xs spacing
    | x == "," = "\n"++formatYml xs spacing
    | otherwise = " "++x++formatYml xs spacing
formatYml [] _ = ""