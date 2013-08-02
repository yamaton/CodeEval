import Data.Text (replace, pack, unpack)

repl' :: String -> String -> String -> String
repl' from to xs = unpack $ replace from' to' xs'
  where [from', to', xs'] = map pack [from, to, xs]

removeLinkedIn = repl' " Share on LinkedIn" ""
fixInput = repl' "Input sample:\n\n" "Input sample\n------------\n" 
fixOutput = repl' "Output sample:\n\n" "Output sample\n-------------\n" 
fixTitle s = unlines $ x:underline:xs
  where (x:xs) = lines s
        underline = replicate (length x) '='
fixDescription = repl' "Challenge Description:\n" "Challenge Description\n---------------------" 

commentOut xs = "{-\n" ++ xs ++ "\n-}\n\n"

snippet xs = xs ++ "\nimport System.Environment (getArgs)\n\nmain = do\n" 
                ++ "  f:_ <- getArgs\n  contents <- readFile f\n"
                ++ "  let input = lines contents\n\n\n"

main = do 
  input <- getContents
  putStr $ ( snippet . commentOut . fixOutput . fixInput . fixDescription . fixTitle . removeLinkedIn) input

