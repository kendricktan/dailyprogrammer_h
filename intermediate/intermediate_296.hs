data Point = Point Float Float deriving (Show, Read, Eq, Ord)
instance Num Point where
  (Point x y) + (Point u v) = Point (x+u) (y+v)
  (Point x y) * (Point u v) = Point (x*u) (y*v)
  (Point x y) - (Point u v) = Point (x-u) (y-v)
  abs           (Point x y) = Point (abs x) (abs y)
  signum        (Point x y) = Point (signum x) (signum y)
  fromInteger i             = Point (fromInteger i) (fromInteger i)

sqPoint :: Point -> Float
sqPoint (Point x y) = x * y

lenPoint :: Point -> Float
lenPoint (Point x y) = abs $ x - y

intersection :: Point -> Point -> Point -> Point -> Float
intersection a1 a2 b1 b2 = sqPoint (abs $ max a1 a2 - min b1 b2)

main :: IO ()
main = do
  let l = intersection (Point (-4) 4) (Point (-0.5) 2) (Point 0.5 1) (Point (3.5) (3))
  putStrLn (show l)
