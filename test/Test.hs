{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import qualified Game            as G
import qualified Types as G
import qualified Game as G

main :: IO ()
main = runTests 
  [ 
    testAI,
    testLogic
  ]

out1 :: [Int]
out1 = [0,0,0,0,0,0,0,0,0,0,1,2,3,4,3,2,1,0,0,2,4,6,8,6,4,2,0,0,3,6,39,42,39,6,3,0,0,4,8,42,16,42,8,4,0,0,3,6,39,42,39,6,3,0,0,2,4,6,8,6,4,2,0,0,1,2,3,4,3,2,1,0,0,0,0,0,0,0,0,0,0]

out2 :: [Int]
out2 = [0,0,0,0,0,0,0,0,0,0,1,2,3,4,3,2,1,0,0,2,4,6,8,6,4,2,0,0,3,6,309,3012,409,106,103,0,0,4,8,612,16,812,8,104,0,0,3,6,909,12,9,206,103,0,0,2,4,606,8,706,104,2,0,0,1,2,303,3004,303,2,1,0,0,0,0,0,0,0,0,0,0]

out3 :: [Int]
out3 = [0,0,0,0,0,0,0,0,0,0,1,2,3,4,3,2,1,0,0,2,4,6,8,6,4,2,0,0,3,6,9,12,9,6,3,0,0,4,8,312,316,3012,8,4,0,0,3,306,609,12,309,6,3,0,0,302,604,6,608,306,4,2,0,0,301,2,603,304,3,2,1,0,0,3000,300,300,0,0,0,0,0]
testAI ::  Score -> TestTree
testAI sc = testGroup "AI" 
  [ scoreTest ((\_ -> G.calculateAIScoreAt G.calculateAIScoreAtListHard G.test1 0 0),  (), 0, 10, "test-AI-1"),
  scoreTest ((\_ -> G.calculateAIScoreAt G.calculateAIScoreAtListHard G.test1 4 3),  (), 312, 10, "test-AI-2"),
  scoreTest ((\_ -> G.calculateAIScoreAt G.calculateAIScoreAtListHard G.test2 4 3),  (), 612, 10, "test-AI-3"),
  scoreTest ((\_ -> G.calculateAIScoreAt G.calculateAIScoreAtListHard G.test3 5 3),  (), 609, 10, "test-AI-4"),
  scoreTest ((\_ -> G.calculateAIScore G.test1 G.Easy),  (), out1, 10, "test-AI-5"),
  scoreTest ((\_ -> G.calculateAIScore G.test2 G.Hard),  (), out2, 10, "test-AI-6"),
  scoreTest ((\_ -> G.calculateAIScore G.test3 G.Hard),  (), out3, 10, "test-AI-7")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

logicOut1 :: [G.Cell]
logicOut1 = [G.Empty,G.Empty,G.Empty,G.Empty,G.Empty,G.Empty,G.Empty,G.Empty,G.Empty]

testLogic ::  Score -> TestTree
testLogic sc = testGroup "Logic test" 
  [ scoreTest ((\_ -> G.putAITesting G.test3 G.Hard),  (), (4, 5), 10, "test-Logic-1"),
    scoreTest ((\_ -> G.getRowAt G.test3 1),  (), logicOut1, 10, "test-Logic-2")

  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
