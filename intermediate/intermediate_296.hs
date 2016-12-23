data Rectangle = Rectangle Double Double Double Double deriving (Show, Read)

-- | Remove comma from a string
removeComma = map (\c -> if c==',' then ' '; else c)

-- | Creates a Rectangle from a list
makeRect :: [Double] -> Rectangle
makeRect x = Rectangle (head x) (x!!1) (x!!2) (x!!3)

-- | Finds area of Rectangle data type
rectArea :: Rectangle -> Double
rectArea (Rectangle x1 y1 x2 y2) = abs (x2-x1) * abs (y2-y1)

-- | Finds the intersecting bound between the two rectangles
bounds :: Rectangle -> Rectangle -> Rectangle
bounds (Rectangle x1 y1 x2 y2) (Rectangle u1 v1 u2 v2)
  | min x1 x2 > max u1 u2 = Rectangle 0 0 0 0
  | max x1 x2 < min u1 u2 = Rectangle 0 0 0 0
  | min y1 y2 > max v1 v2 = Rectangle 0 0 0 0
  | max y1 y2 < min v1 v2 = Rectangle 0 0 0 0
  | otherwise = Rectangle (max (min x1 x2) (min u1 u2)) (max (min y1 y2) (min v1 v2)) (min (max x1 x2) (max u1 u2)) (min (max y1 y2) (max v1 v2))

-- | Converts input into rectangles
inRectangles :: String -> [Rectangle]
inRectangles s = map (makeRect . map (read::String->Double) . words) $ lines $ removeComma s

-- | Finds overlap between a list of Rectangles
boundingRects :: [Rectangle] -> Double
boundingRects x = rectArea $ foldl bounds (head x) (tail x)

main :: IO ()
main = interact (show . boundingRects . inRectangles)
