-- Greatest common denominator
gcd_ :: Int -> Int -> Int
gcd_ a 0 = a 
gcd_ a b = gcd_ b (a `mod` b)

main :: IO ()
main = do
    putStrLn "Number one: "
    a <- getLine
    putStrLn "Number two: "
    b <- getLine
    let c = gcd_ (read a::Int) (read b::Int)
    putStrLn $ show((read a::Int) `div` c) ++ ", " ++ show((read b::Int) `div` c)