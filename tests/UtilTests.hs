module UtilTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Hyahtzee2.Util as Util (modifyNth, generateRandomValues)

tests = testGroup "Util" [modifyNthTests]

modifyNthTests = testGroup "modifyNth"
  [ testCase "should change the first element" $
  modifyNth 0 (1+) [1,2,3] @?= [2,2,3]

  , testCase "should change an element in the middle" $
  modifyNth 1 (1+) [1,2,3] @?= [1,3,3]

  , testCase "should change the last element" $
  modifyNth 2 (1+) [1,2,3] @?= [1,2,4]

  , testCase "should return [] when passed []" $
  modifyNth 0 (1+) [] @?= []
  ]
