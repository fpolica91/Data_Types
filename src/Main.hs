module Main where

main :: IO ()
main = do
  putStrLn "hello world"


data Geometry = Rectangle Double Double | Square Double | Circle Double  deriving (Show)


perimeter:: Geometry -> Double

perimeter(Rectangle width  height) = 2 * width + 2 * height

perimeter (Square width) = width * 4

perimeter (Circle radius) = 2 * pi * radius


enlarge :: Geometry -> Double -> Geometry
enlarge (Rectangle a b) d = Rectangle (d*a) (d*b)
enlarge (Square a) _ = Square (2*a)
enlarge (Circle r) d = Circle (r*d)


data Animal = Cat
  | Dog
  | Mouse
  | Elephant
  | Rabbit


data Coordinates = Coordinates Double Double deriving (Show)

data LocatedShape = LocatedShape Geometry Coordinates deriving (Show)
-- located = LocatedShape (Square 2) (Coordinates 2 3)

containedIn :: Coordinates -> LocatedShape -> Bool

(Coordinates x y) `containedIn` (LocatedShape shape (Coordinates cx cy)) =
  case shape of 
    (Circle r) -> (cx - x)^2 + (cy - y)^2 <= r^2
    (Rectangle a b) -> abs (cx - x) <= a / 2 && abs (cy - y) <= b / 2
    (Square a) -> abs (cx - x) + abs (cy - y) <= a


data Vector = Vector Double Double deriving (Show)
move :: LocatedShape -> Vector -> LocatedShape

move (LocatedShape shape (Coordinates x y)) (Vector a b) 
  = LocatedShape shape (Coordinates(x + a) (y + b))


