factorial :: Int -> Int
factorial n = if n == 0 then 1 else n * factorial (n-1)

binomial :: Int -> Int -> Int
binomial n k
  | (0 <= k) && (k <= n) = factorial n `div` factorial k `div` factorial (n-k)
  | otherwise = 0

seatings :: Int -> Int
seatings n = max 0 $ sum [(-2)^k * binomial n k * factorial (2 * n - k - 1) | k <- [0..n]]

