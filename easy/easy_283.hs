import System.Environment
import Data.List
import Data.List.Split
import Data.Char

anagramIs :: [String] -> String
anagramIs [a, b] = concat [a, out (parseWord a == parseWord b), b]
    where parseWord = sort . map toLower . filter isLetter

out :: Bool -> String
out True = "is an anagram of"
out _ = "is NOT an anagram of"

main :: IO ()
main = interact $  unlines . map (anagramIs . splitOn "?") . lines