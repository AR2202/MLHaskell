module BernoulliNaiveBayes
  (Person(..),
   countCold,
   countField,
   countField',
   p_field_cold,
   p_cold_person,
   p_cold_person',
   p_noCold_person,
   p_noCold_person',
   traindata,
   testperson,
   testperson2,
   classifyNB
   )
  
  where

--A Bernoulli Naive Bayes classifier 
-------------------------------------
--This is an implementation assuming a binary classification and boolean variables as predictors

--a Naive Bayes classifier is implemented on an example of classifying a person as having a cold or not having a cold

-- Bayes Theorem
--------------------------------------


--   P(B|A) = P(A|B) * P(B)/P(A)





--Datatype for the training/testing data
----------------------------------------------
data Person = Person {headache :: Bool,
                      runnyNose :: Bool,
                      cough :: Bool,
                      cold :: Maybe Bool}
     
            deriving (Eq,Show,Read)

--creating some training data
-----------------------
trainperson1 = Person {headache = True, runnyNose = False, cough = False, cold = Just False}
trainperson2 = Person {headache = True, runnyNose = False, cough = True, cold = Just True}
trainperson3 = Person {headache = True, runnyNose = True, cough = False, cold = Just True}
trainperson4 = Person {headache = False, runnyNose = False, cough = False, cold = Just False}
trainperson5 = Person {headache = False, runnyNose = True, cough = False, cold = Just True}
trainperson6 = Person {headache = False, runnyNose = True, cough = True, cold = Just True}
trainperson7 = Person {headache = False, runnyNose = False, cough = True, cold = Just False}
trainperson8 = Person {headache = True, runnyNose = True, cough = False, cold = Just True}
trainperson9 = Person {headache = True, runnyNose = False, cough = False, cold = Just True}
trainperson10 = Person {headache = False, runnyNose = True, cough = False, cold = Just False}

traindata :: [Person]
traindata = [trainperson1,trainperson2,trainperson3,trainperson4,trainperson5,trainperson6,trainperson7,trainperson8,trainperson9,trainperson10]

--Creating testpersons without a label for 'cold'
--------------------------------------------------
testperson :: Person
testperson = Person {headache = False, runnyNose = True, cough = True, cold = Nothing}

testperson2 :: Person
testperson2 = Person {headache = False, runnyNose = False, cough = False, cold = Nothing}




--Functions for counting occurrances and determining prior probabilities
-----------------------------------------------------------------------
--Counting cold 
-------------------

countCold :: [Person] -> Int
countCold personlist = length.filter(==Just True)$ map cold personlist

--Counting any field in Persons in a list
-----------------------------------------------

countField :: (Person -> Bool) -> [Person] -> Int
countField field personlist = length.filter(==True)$ map field personlist


countField' :: (Person -> Bool) -> Bool -> [Person] -> Int
countField' field value personlist = length.filter(==value)$ map field personlist



--Probability of any field being True in the whole dataset
----------------------------------------------------------
p_field :: (Person -> Bool) -> [Person] -> Float
p_field field personlist  = fromIntegral (countField field personlist)/(fromIntegral $length personlist)

--Probability of any field being of a certain value in the whole dataset
----------------------------------------------------------
p_field' :: (Person -> Bool) -> Bool -> [Person] -> Float
p_field' field value personlist  = fromIntegral (countField' field value personlist)/(fromIntegral $length personlist)

--Prior probability of having a cold
--------------------------------------
p_cold :: [Person] -> Float
p_cold personlist = fromIntegral (countCold personlist)/(fromIntegral $length personlist)

p_noCold :: [Person] -> Float
p_noCold personlist = 1 - (p_cold personlist)

--Probability of any field being a certain value in persons with a cold
----------------------------------------------------------
p_field_cold :: (Person -> Bool) -> Bool -> [Person] -> Float
p_field_cold field value personlist = fromIntegral (countField' field value (filter (\p -> cold p ==Just True) personlist) )/(fromIntegral $length $filter (\p -> cold p ==Just True) personlist)

--Probability of any field being a certain value in persons with a cold
----------------------------------------------------------
p_field_noCold :: (Person -> Bool) -> Bool -> [Person] -> Float
p_field_noCold field value personlist = fromIntegral (countField' field value (filter (\p -> cold p ==Just False) personlist) )/(fromIntegral $length $filter (\p -> cold p ==Just False) personlist)


--conditional probabilities multiplied
--('naive' assumption of independence of the predictors)
---------------------------------------

p_cond :: Person -> [Person] -> Float
p_cond person traindata = p_field_cold headache (headache person) traindata * p_field_cold runnyNose (runnyNose person) traindata * p_field_cold cough (cough person) traindata

p_cond_noCold :: Person -> [Person] -> Float
p_cond_noCold person traindata =  p_field_noCold headache (headache person) traindata * p_field_noCold runnyNose (runnyNose person) traindata * p_field_noCold cough (cough person) traindata



--prior probabilities multiplied
--------------------------------
p_priors :: Person -> [Person] -> Float
p_priors person traindata = p_field' headache (headache person) traindata * p_field' runnyNose (runnyNose person) traindata * p_field' cough (cough person) traindata


--probability of a person having a cold given a certain combination of symptoms
-------------------------------------------------------------------------------
p_cold_person :: Person -> [Person] -> Float
p_cold_person person traindata = p_cond person traindata * p_cold traindata/(p_priors person traindata)



--probability of a person having not a cold given a certain combination of symptoms
-------------------------------------------------------------------------------
p_noCold_person :: Person -> [Person] -> Float
p_noCold_person person traindata = p_cond_noCold person traindata * p_noCold traindata/(p_priors person traindata)


--The denominator is often ommited as it is the same in both calculations, and we only care about which prediction has the higher probability.
-----------------------------------------------------------------------------------
p_cold_person' :: Person -> [Person] -> Float
p_cold_person' person traindata = p_cond person traindata * p_cold traindata

p_noCold_person' :: Person -> [Person] -> Float
p_noCold_person' person traindata = p_cond_noCold person traindata * p_noCold traindata

--Classifying a new Person as having a cold or not having a cold
----------------------------------------------------------------
--(run this function on 'testperson' and 'traindata' or on 'testperson2' and 'traindata' to see the result)
------------------------------------------------------------------------------------------------------------

classifyNB :: Person -> [Person] -> IO()
classifyNB person traindata = putStrLn $ "Prediction: " ++ outcome ++ " P(no cold): "++(show p_no)++", P(cold): "++ (show p_yes)
  where outcome
          |p_no>=p_yes = "no cold"
          |otherwise = "cold"
        p_no = p_noCold_person' person traindata
        p_yes = p_cold_person' person traindata
