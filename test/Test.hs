{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import qualified Game            as G

main :: IO ()
main = runTests 
  [ 
    testAI
  ]

testAI ::  Score -> TestTree
testAI sc = testGroup "AI" 
  [ scoreTest ((\_ -> G.calculateAIScoreAt G.calculateAIScoreAtListHard G.test1 0 0),  (), 0, 10, "test-1")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
