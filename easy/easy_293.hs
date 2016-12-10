import Text.Regex.Posix
import Data.Bool
import Data.Char

-- Rules
-- white -> ! white | ! black
-- red -> green
-- black -> ! white | ! green | ! orange
-- orange -> red | black
-- green -> orange | white
-- purple -> ! purple | ! green | ! orange ! | white
bombExplode :: String -> Bool
bombExplode s = s =~ "w[w|b]|r[^g]|b[w|g|o]|o[^r|^b]|g[^o|^w]|p[p|g|o|w]" :: Bool 

-- Prettify sequence of inputs
prettifySeq :: [String] -> String
prettifySeq = map (toLower . head)

main :: IO()
main = do 
    let explode = bombExplode $ prettifySeq ["White", "Red", "Green", "white", "White"]
    if explode 
        then putStrLn "Bomb exploded"
    else putStrLn "Bomb diffused safely"
