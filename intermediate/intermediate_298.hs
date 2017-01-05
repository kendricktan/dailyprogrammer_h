import System.Environment

swapBrackets :: Char -> Char 
swapBrackets x
  | x == ')' = '('
  | x == '(' = ')'
  | otherwise = x

lastOr :: String -> Char -> Bool
lastOr x y = if null x then True else (last x) == y

badBrackets :: String -> String -> String
badBrackets [] _ = ""
badBrackets (x:xs) y
  | x == ')' && lastOr y ')'   = "**)**" ++ xs
  | x == ')' && lastOr y '('   = [x] ++ badBrackets xs (if null y then "" else init y)
  | x == '('                   = [x] ++ badBrackets xs (y ++ "(")
  | otherwise                  = [x] ++ badBrackets xs y

flipWord :: String -> String
flipWord s = map swapBrackets $ reverse s

main :: IO ()
main = do
  s <- getArgs
  if (badBrackets (s!!0) "") /= (s!!0)
    then print $ badBrackets (s!!0) ""
    else 
      print $ flipWord (badBrackets (flipWord (s!!0)) "")
  
