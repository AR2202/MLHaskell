{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

module GaussianNaiveBayes
  (Gender(..),
   Human(..),
   Testp(..),
   countAll,
   pMale,
   mean,
   classifyGNB,
   trainingsdata,
   person1,
   person2
  )
where

--Gaussian Naive Bayes classifier
--the implementation is similar as for the Bernoulli Naive Bayes classifier, but predictors are numerical variables and probabilities are estimated from a gaussian probability distribution


--Types for storing Training and Testing Datasets
------------------------------------------------
data Gender = Male | Female
  deriving(Show, Read, Eq)

data Human a = Human {height :: a, weight :: a, footsize :: a}
  deriving(Show, Read, Eq, Functor, Foldable)


type Testp = (Human Float, Gender)

--creating some Trainingdata
------------------
testp1 :: Testp
testp1 = (Human 1.7 65 41, Male)

testp2 :: Testp
testp2 = (Human 1.5 50 38, Female)


testp3 :: Testp
testp3 = (Human 1.8 75 45, Male)

testp4 :: Testp
testp4 = (Human 1.6 57 36, Female)

testp5 :: Testp
testp5 = (Human 1.9 105 44, Male)

testp6 :: Testp
testp6 = (Human 1.7 67 39, Female)

testp7 :: Testp
testp7 = (Human 1.7 75 43, Male)

testp8 :: Testp
testp8 = (Human 1.6 67 37, Female)

trainingsdata = [testp1, testp2, testp3, testp4, testp5, testp6, testp7, testp8]


--some unlabelled testdata
---------------------------
person1 :: Human Float
person1 = Human 1.6 52 37

person2 :: Human Float
person2 = Human 1.8 82 42

--accessing the data
--------------------

getGender :: Testp -> Gender
getGender = snd

getData :: Testp -> Human Float
getData = fst

countAll :: [a] -> Float
countAll = fromIntegral . length

countFiltered :: (a -> Bool) -> [a] -> Float
countFiltered filterfunc list = countAll (filter filterfunc list)

isFemale :: Testp -> Bool
isFemale person = getGender person == Female

isMale :: Testp -> Bool
isMale person = getGender person == Male

isGender :: Gender -> Testp -> Bool
isGender gender person = getGender person == gender

--determining prior probabilities
----------------------------------
pMale :: [Testp] -> Float
pMale [] = 0
pMale personlist = countFiltered isMale personlist/countAll personlist


pFemale :: [Testp] -> Float
pFemale [] = 0
pFemale personlist = countFiltered isFemale personlist/countAll personlist

--determining mean and variance of the data
------------------------------------------
mean :: (Human Float -> Float) -> [Testp] -> Float
mean field list = (foldl (+) 0 $ map (field.getData) list)/countAll list 

squaredErr :: (Human Float -> Float) -> [Testp] -> Float -> Float
squaredErr field list x = square $ x - mean field list

sumSquErr :: (Human Float -> Float) -> [Testp] -> Float
sumSquErr field list = sum $ map (squaredErr field list) $ map (field.getData) list

var :: (Human Float -> Float) -> [Testp] -> Float
var _ [] = 0
var field list = sumSquErr field list/countAll list

var_female :: (Human Float -> Float) -> [Testp] -> Float
var_female _ [] = 0
var_female field list = sumSquErr field (femalelist list)/countAll (femalelist list)

var_male :: (Human Float -> Float) -> [Testp] -> Float
var_male _ [] = 0
var_male field list = sumSquErr field (malelist list)/countAll (malelist list)

femalelist :: [Testp] -> [Testp]
femalelist list = filter isFemale list

malelist :: [Testp] -> [Testp]
malelist list = filter isMale list

square :: Float -> Float
square x = x*x 


--determining the likelihood
------------------------------------------------------------

-- The conditional probabilities are determined by the gaussian probability density function

p_field_male :: Float -> (Human Float -> Float) -> [Testp] -> Float
p_field_male fieldval field list = 1/(2* pi * var_male field list) * exp (-square (fieldval - mean field (malelist list))/(2 * var_male field list))

p_field_female :: Float -> (Human Float -> Float) -> [Testp] -> Float
p_field_female fieldval field list = 1/(2* pi * var_female field list) * exp (- square (fieldval - mean field (femalelist list))/(2 * var_female field list))


--The denominator is the same for male and female dataset, so it is irrelevant to the task of classification and can be left out


--posterior probabilities (without the denominator)
---------------------------------------------

p_female_posterior :: Human Float -> [Testp] -> Float
p_female_posterior testperson list = pFemale list * p_field_female (height testperson) height list *p_field_female (weight testperson) weight list * p_field_female (footsize testperson) footsize list 

p_male_posterior :: Human Float -> [Testp] -> Float
p_male_posterior testperson list = pMale list * p_field_male (height testperson) height list *p_field_male (weight testperson) weight list * p_field_male (footsize testperson) footsize list 

--classification of a new person as male or female

----------------------------------------------
classifyGNB :: Human Float -> [Testp] -> IO ()
classifyGNB  person traindata = putStrLn $ "Prediction: " ++ outcome ++ " P(female): "++ (show female)++", P(male): "++ (show male)
  where outcome
          |male == female = "cannot be determined"
          |male > female = "male"
          |otherwise = "female"
        male   = p_male_posterior person traindata
        female = p_female_posterior person traindata



--to test this classifier on the provided example data, run the following functions in a repl:
------------------------------------------------------------------------------------------------
--   classifyGNB person1 trainingsdata
--   classifyGNB person2 trainingsdata
