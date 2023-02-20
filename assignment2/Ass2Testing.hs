--ass2Testing
module Ass2Testing where

import Assignment2
import Test.HUnit

{-scoreMatch :: Integer
scoreMatch = 0
scoreMismatch :: Integer
scoreMismatch = -1
scoreSpace :: Integer
scoreSpace = -1
-}

string1 :: [Char]
string1 = "writers"
string2 :: [Char]
string2 = "vintner"


--similarityScore string1 string2

similarityScoreTest :: Test
similarityScoreTest = 
        test [
          similarityScore string1 string2
            ~?= -5
     ]
  
ass2Testing = runTestTT $
    test [
      "substitute" ~: Ass2Testing.similarityScoreTest
    ]