module LinearRegression
  (
   delta,
   deltas,
   y_line,
   y_data,
   x_data,
   sumOfSquares,
   cost,
   Coord (..),
   dcdm,
   dcdb,
   m_new,
   b_new,
   tuple2Coord,
   updated,
   readInput,
   inputfile,
   linregress
  
)
  where

-- datatype for storing coordinate points
----------------------------------------
----------------------------------------

data Coord = Point Float Float deriving (Show, Read)

-- converting tuple to Coord
---------------------------
tuple2Coord :: (Float,Float) -> Coord
tuple2Coord (a,b) = Point a b

-- Determining the cost
------------------------
------------------------



-- The cost function
--------------------
cost :: Float -> Float -> [Coord] -> Float
cost m b datapoints = (1/(fromIntegral $length $ deltas m b datapoints)) * sumOfSquares m b datapoints

-- The sum of the squared difference between datapoints and the regression line
---------------

sumOfSquares :: Float -> Float -> [Coord] -> Float
sumOfSquares m b datapoints  = sum $ map square $ deltas m b datapoints

-- distance of a specific datapoint to the regression line
----------------
delta :: Float -> Float -> Coord -> Float
delta m b (Point x y) = y_line m b (Point x y) - y_data (Point x y)

-- all distances of a list of datapoints to the regression line
--------------
deltas :: Float -> Float -> [Coord] -> [Float]
deltas m b datapoints = map (delta m b) datapoints 

-- the regression line
--------------------
y_line :: Float -> Float -> Coord -> Float
y_line m b (Point x _) = m*x + b

-- getting the y value out of a coordinate point
----------------
y_data :: Coord -> Float
y_data (Point _ y) = y

-- getting the x value out of a coordinate point
----------------
x_data :: Coord -> Float
x_data (Point x _ ) = x

-- definition of square
------------------
square :: Float -> Float
square x = x*x


-- The partial derivatives
-------------------------
-------------------------

-- for m
---------
dcdb :: Float -> Float -> [Coord] -> Float
dcdb m b datapoints = (2/(fromIntegral $length $ deltas m b datapoints)) * sum (deltas m b datapoints)

-- for b
-------
dcdm :: Float -> Float -> [Coord] -> Float
dcdm m b datapoints = (2/(fromIntegral $length $ deltas m b datapoints)) * (sum $ zipWith (*) (deltas m b datapoints) (map x_data datapoints))

-- Gradient descent
------------------
------------------

-- update rules
---------------

-- for m
---------
m_new :: Float -> Float -> Float -> [Coord] -> Float
m_new alpha m b datapoints = m - alpha * (dcdm m b datapoints)

--for b
-------
b_new :: Float -> Float -> Float -> [Coord] -> Float
b_new alpha m b datapoints = b - alpha *( dcdb  m b datapoints)

-- the updating function
-------------------------
updated :: Float -> Float -> Float -> [Coord] -> Int -> (Float, Float)
updated alpha m b datapoints 0 = (m,b)
updated alpha m b datapoints iterations = updated alpha (m_new alpha m b datapoints) (b_new alpha m b datapoints) datapoints (iterations-1)
 




--reading and parsing the input
-------------------------------
-- a text file provided for demonstration
-----------------------------------
inputfile = "../input.txt"

readInput filename = do
  contents <-readFile filename
  let trainingdata = map readTuples $ lines contents
  let dpoints = map tuple2Coord trainingdata
  return dpoints

readTuples :: String -> (Float,Float)
readTuples = read 

linregress :: IO ()
linregress = do
  datapoints <- readInput inputfile
  let alpha = 0.01 -- the learning rate
  let m = 1 --initial guess for m
  let b = 1 -- initial guess for b
  let iterations = 1000 -- number of iterations to run through
  let output = updated alpha m b datapoints iterations
  putStrLn "The regression coefficients are:"
  print output
  
