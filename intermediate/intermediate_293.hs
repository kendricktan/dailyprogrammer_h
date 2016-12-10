-- Rules
-- Start: White/Red
-- 1.
-- White THEN White | Orange
-- Red THEN Black | Red
-- 2.
-- If Red -> Red THEN Rule 1
-- If White -> White THEN Black | Red
-- 3.
-- If White -> White -> Red THEN Rule 1
-- If White -> White -> Orange THEN Green | Orange | Black
-- 4.
-- If White -> White -> Orange -> (Green | Orange) THEN choose other one to difuse bomb
-- If ... -> Black THEN Rule 4

-- to run program, do print "white\nblue\ngreen" | ./bin_file

import Data.Char

capitalise :: String -> String
capitalise (x:xs) = toUpper x:xs

data State = Start | Exit | Error | S Int deriving (Show)
data Wire = White | Red | Green | Orange | Black deriving (Read, Show)

next :: State -> Wire -> State
next Start  White   = S 1
next Start  Red     = S 2
next (S 1)  White   = S 2
next (S 1)  Orange  = S 3
next (S 2)  Red     = Start
next (S 2)  Black   = S 3
next (S 3)  Black   = S 3
next (S 3)  Orange  = S 4
next (S 3)  Green   = S 5
next (S 4)  Green   = Error
next (S 5)  Orange  = Exit
next _      _       = Error

out :: State -> String
out Exit    = "Defused"
out _       = "Boom!"

main :: IO ()
main = interact $ out . foldl next Start . map (read. capitalise) . lines 