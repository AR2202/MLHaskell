module KNN
(euklideanDist,
tuple2ClassifiedPoint,
euklideanDistLabelled,
euklideanDistToAll,
knn,
mostFrequentLabel,
knnClassify

)
    where
        
import Data.List        
import Data.Ord

-- This implementation assumes data with 2 numeric features

--------------------------------------------------------------

-- datatype for storing coordinate points
----------------------------------------
----------------------------------------

type CoordPoint = (Float,Float)
type ClassifiedPoint = (CoordPoint,String)
type EDist = Float

-- Parsing the input data, 
-- which are in the format: (x,y,"class")
---------------------------
tuple2ClassifiedPoint :: (Float,Float, String) -> ClassifiedPoint
tuple2ClassifiedPoint (x,y,string) = ((x,y),string)


-- determining euklidean Distance of an unlabelled point to the labelled data
----------------------------------

euklideanDist :: CoordPoint -> ClassifiedPoint -> EDist
euklideanDist (x1,y1) ((x2,y2),_) = sqrt $ (x1-x2)**2 + (y1-y2)**2

euklideanDistLabelled :: CoordPoint -> ClassifiedPoint -> (EDist,String)
euklideanDistLabelled point (coordPoint, label) = (euklideanDist point classifiedPoint,label)
    where classifiedPoint = (coordPoint, label)

euklideanDistToAll :: CoordPoint -> [ClassifiedPoint] -> [(EDist,String)]
euklideanDistToAll point = map (euklideanDistLabelled point)

-- determining the label of the k nearest neighbours
-----------------------------------------
knn :: Int -> CoordPoint -> [ClassifiedPoint] -> [String]
knn k  point pointlist = map snd $ take k $ sortOn fst $ euklideanDistToAll point pointlist

mostFrequentLabel :: [String] -> String
mostFrequentLabel = head . maximumBy (comparing length) . group . sort 

readInput :: String -> IO [ClassifiedPoint]
readInput filename = do
  contents <-readFile filename
  let trainingdata = map readTuplesWithLabel $ lines contents
  let labelledpoints = map tuple2ClassifiedPoint trainingdata
  return labelledpoints


readTuplesWithLabel :: String -> (Float,Float,String)
readTuplesWithLabel = read 

-- a testpoint to be classified
----------------------------------
testpoint :: CoordPoint
testpoint = (3,8)

testpoint2 :: CoordPoint
testpoint2 = (6,4)


-- some input data are provided in a file called 'input2.txt'
--------------------------------------------------------------
inputfile = "../input2.txt"

-- classify the two testpoints as either "green" or "red"
---------------------------------

knnClassify :: IO ()
knnClassify = do
  datapoints <- readInput inputfile
  let n = 3
  
  let output  = mostFrequentLabel $ knn 3 testpoint datapoints
  let output2 = mostFrequentLabel $ knn 3 testpoint2 datapoints

  putStrLn "The first point is classified as:"
  print output
  putStrLn "The second point is classified as:"
  print output2
