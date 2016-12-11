import Data.List
import Data.Bool
import Data.Ord
import System.IO
import Control.Monad


-- Longest String
longest :: [String] -> String
longest xss = snd $ maximum $ [(length xs, xs) | xs <- xss]

-- Dank Match
-- [First subsection] [Second subsection] [Name]
dankMatch :: [String] -> String -> String -> Bool
dankMatch _ _ []    = False
dankMatch n fs ss   = fs `isInfixOf` head n && ss `isInfixOf` (head . tail) n || dankMatch n (fs ++ [head ss]) (tail ss)

-- Dank Iter
-- [Name] [Word List]
dankIter :: [String] -> String -> String
dankIter n wl = 
    if dankMatch n [head wl] (tail wl)
    then wl
    else ""

-- Dank Apply 
-- [Name] -> [Word List] -> [Matched Words]
dankApply :: [String] -> [String] -> [String]
dankApply n = map (dankIter n) 

main :: IO ()
main = do
    handle <- openFile "enable1.txt" ReadMode
    contents <- hGetContents handle
    let wordlist = words contents
    let memes = dankApply ["donald", "knuth"] wordlist 
    print $ longest memes
    hClose handle
