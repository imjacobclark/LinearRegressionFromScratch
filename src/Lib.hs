module Lib ( lib ) where

---------------
-- Domain
---------------

data Point = Point {
    x :: Double,
    y :: Double
} deriving Show

data Line = Line (Point, Point) deriving Show

data Slope = Slope Double
data Intercept = Intercept Double

data StraightLine = StraightLine { 
    slope       :: Slope,
    intercept   :: Intercept
}

instance Show Slope where
    show (Slope m) = show m

instance Show Intercept where
    show (Intercept b) = show b

instance Show StraightLine where
    show (StraightLine m b) = formatEquation m b

---------------
-- Equation of a straight line
---------------

formatEquation :: Slope -> Intercept -> String
formatEquation (Slope m) (Intercept b) = do
    if (isInfinite m) == True && (isInfinite b) == True
        then "y=x"
        else if (isInfinite m) == True
        then "y=x+" ++ (show b)
            else "y=" ++ (show m) ++ "x+" ++ (show b)

calculateSlope :: Line -> Slope
calculateSlope (Line (a, b)) = Slope $ (y b - y a) / (x b - x a) 

calculateIntercept :: Point -> Slope -> Intercept
calculateIntercept p (Slope m) = Intercept $ (y p) - (m * (x p))

computeY :: Double -> Slope -> Intercept -> Double
computeY x (Slope m) (Intercept b) = m*x+b

lengthAsDouble :: [Double] -> Double
lengthAsDouble xs = fromIntegral $ length xs

---------------
-- Linear Regression
---------------

mean :: [Double] -> Double
mean xs = sum xs / lengthAsDouble xs

getX :: [Point] -> [Double]
getX = map (\p -> x p)

getY :: [Point] -> [Double]
getY = map (\p -> y p)

delta :: [Double] -> Double -> Double
delta xs x = x - mean xs

estimateSlope :: [Double] -> [Double] -> Slope
estimateSlope xs ys = Slope (numerator / denominator) where
    numerator = sum $ zipWith (\x y -> (delta xs x) * (delta ys y)) xs ys
    denominator = (sum $ map (delta xs) xs) ** 2

estimateIntercept :: Double -> Double -> Slope -> Intercept
estimateIntercept yMean xMean (Slope m)  = Intercept (yMean - m * xMean)

lib :: IO ()
lib = do 
    let points = [ (Point {x = 1, y = 1}), (Point {x = 2, y = 2}), (Point {x = 3, y = 3}), (Point {x = 4, y = 4}), (Point {x = 5, y = 5}), (Point {x = 6, y = 6}), (Point {x = 7, y = 7}), (Point {x = 8, y = 8}), (Point {x = 9, y = 9}), (Point {x = 10, y = 10})]
    let xPoints = getX points
    let yPoints = getY points

    let estimatedSlope = estimateSlope xPoints yPoints
    let estimatedIntercept = estimateIntercept (mean yPoints) (mean xPoints) estimatedSlope

    let estimatedLine = StraightLine {
        slope=estimatedSlope,
        intercept=estimatedIntercept
    }

    print $ estimatedLine

    ---------------
    -- ONLY USEFUL FOR A STRAIGHT LINE
    ---------------

    let slope = calculateSlope $ Line ((points !! 0), (points !! 1))
    let intercept = calculateIntercept (points !! 0) slope

    let straightLine = StraightLine {
        slope=slope,
        intercept=intercept
    }

    print $ straightLine
    print $ computeY 4 slope intercept
