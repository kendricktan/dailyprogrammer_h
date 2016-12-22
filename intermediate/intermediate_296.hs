data Rectangle = Rectangle Float Float Float Float deriving (Show, Read, Eq, Ord)
instance Num Rectangle where
  (Rectangle x1 y1 x2 y2) + (Rectangle u1 v1 u2 v2) = Rectangle (x1+u1) (y1+v1) (x2+u2) (y2+v2)
  (Rectangle x1 y1 x2 y2) * (Rectangle u1 v1 u2 v2) = Rectangle (x1*u1) (y1*v1) (x2*u2) (y2*v2)
  (Rectangle x1 y1 x2 y2) - (Rectangle u1 v1 u2 v2) = Rectangle (x1-u1) (y1-v1) (x2-u2) (y2-v2)
  abs                       (Rectangle x1 y1 x2 y2) = Rectangle (abs x1) (abs y1) (abs x2) (abs y2)
  signum                    (Rectangle x1 y1 x2 y2) = Rectangle (signum x1) (signum y1) (signum x2) (signum y2)
  fromInteger i                                     = Rectangle (fromInteger i) (fromInteger i) (fromInteger i) (fromInteger i)

-- | Finds area of Rectangle data type
rectArea :: Rectangle -> Float
rectArea (Rectangle x1 y1 x2 y2) = abs (x2-x1) * abs (y2-y1)

-- | Finds the intersecting bound between the two rectangles
bounds :: Rectangle -> Rectangle -> Rectangle
bounds (Rectangle x1 y1 x2 y2) (Rectangle u1 v1 u2 v2)
  | min x1 x2 > max u1 u2 = Rectangle 0 0 0 0
  | min x1 x2 > max u1 u2 = Rectangle 0 0 0 0
  | otherwise = Rectangle (max (min x1 x2) (min u1 u2)) (max (min y1 y2) (min v1 v2)) (min (max x1 x2) (max u1 u2)) (min (max y1 y2) (max v1 v2))

main :: IO ()
main = do
  let area = rectArea $ bounds (Rectangle (-4) 4 (-0.5) 2) (Rectangle 0.5 1 (3.5) (3))
  print area
