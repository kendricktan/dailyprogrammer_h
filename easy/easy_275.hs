import System.Environment
import Data.Char
import Data.List

-- Checks for period
periodicChecker :: String -> String -> Bool 
periodicChecker s [x] = x `elem` s 
periodicChecker s (x:xs) = 
    case elemIndex x s of
        Nothing     -> False
        Just val    -> periodicChecker (drop (val+1) s) xs 

main :: IO ()
main = do
    [ls, ss] <- getArgs
    let a = periodicChecker (map toLower ls) (map toLower ss)
    print a