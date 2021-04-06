module Lib
    ( lib
    ) where

data Point = Point {
    x :: Int,
    y :: Int
} deriving Show

data Line = Line (Point, Point) deriving Show

data Slope = Slope Int
data Intercept = Intercept Int

data StraightLine = StraightLine { 
    slope :: Slope,
    intercept :: Intercept
}

instance Show Slope where
    show (Slope m) = show m

instance Show Intercept where
    show (Intercept b) = show b

instance Show StraightLine where
    show (StraightLine m b) = "y=" ++ (show m) ++ "x+" ++ (show b)

calculateSlope :: Line -> Slope
calculateSlope (Line (a, b)) = Slope $ (y b - y a) `div` (x b - x a) 

calculateIntercept :: Point -> Slope -> Intercept
calculateIntercept p (Slope m) = Intercept $ (y p) - (m * (x p))

computeY :: Int -> Slope -> Intercept -> Int
computeY x (Slope m) (Intercept b) = m*x+b

lib :: IO ()
lib = do 
    let p1 = Point {x = 1, y = 1}
    let p2 = Point {x = 2, y = 2}

    [0..100].map(\i -> Point {x=i, y=i})

    let slope = calculateSlope $ Line (p1, p2)
    let intercept = calculateIntercept p1 slope

    let straightLine = StraightLine {
        slope=slope,
        intercept=intercept
    }

    putStrLn . show $ straightLine
    putStrLn . show $ computeY 4 slope intercept
