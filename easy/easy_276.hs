import System.Environment

-- Duplicate strings and concatenates them
duplicate :: Int -> String -> String
duplicate n s = concat $ replicate n s

-- Flip the horizontal component, yay or nay
shitpostFlip :: Int -> Bool -> Bool
shitpostFlip n f 
    | odd n && not f = True 
    | even n && f = True 
    | otherwise = False

-- Constructs the horizontal component of the shitpost
shitpostHorizontal :: Int -> Bool -> String -> String
shitpostHorizontal n f s 
    | n == 0 = 
        if f then head s : "\n"
        else last s : "\n"        
    | n > 0 = 
        if shitpostFlip n f then [s !! x | x <- [0..length s - 2]] ++ shitpostHorizontal (n-1) f s
        else [s !! x | x <- reverse [1..length s - 1]] ++ shitpostHorizontal (n-1) f s
        
-- Contructs the vertical component of the shitpost
shitpostVertical :: Int -> Int -> Bool -> String -> String
shitpostVertical c n f s 
    | n == 0 = 
        if not f then (s !! (length s -1 -c)) : "\n"
        else (s !! c) : "\n"        
    | n > 0 =
        if not $ shitpostFlip n f then [s !! (length s - 1 - c)] ++ duplicate (length s - 2) " " ++ shitpostVertical c (n-1) f s
        else [s !! c] ++ duplicate (length s - 2) " " ++ shitpostVertical c (n-1) f s

-- Contructs the rectangle
shitposting :: Int -> Int -> Int -> Int -> Bool -> String -> String
shitposting w h cw ch f s
    | ch == 0 = shitpostHorizontal w f s ++ shitposting w h cw (ch+1) f s
    | ch > 0 && ch <= h =
        if cw == length s-1 then shitpostHorizontal w (not f) s ++ shitposting w h 1 (ch+1) (not f) s
        else shitpostVertical cw w f s ++ shitposting w h (cw+1) ch f s
    | ch == (h+1) = "\n"

-- Abstracts the rectangle
shitpost :: Int -> Int -> String -> String
shitpost w h = shitposting w h 1 0 True

main :: IO ()
main = do
    [w, h, s] <- getArgs
    let a = shitpost (read w::Int) (read h::Int) s
    putStrLn a