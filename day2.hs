-- NOTE:https://adventofcode.com/2017/day/2
-- Using QuickCheck
import AdventOfCodeAPI
import Data.ByteString.Lazy qualified as LBS
import Data.Char (digitToInt)
import Data.Either (fromLeft, fromRight)
import Data.List (sort)
import Data.List.Split (splitOn)
import System.Environment (getArgs, withArgs)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Read (readMaybe)

part1 :: [Char] -> Int
part2 :: [Char] -> Int
-- first part

preprocessing :: [Char] -> [[Int]]
preprocessing xs = [[read x :: Int | x <- xs] | xs <- map (splitOn "\t") (splitOn "\n" xs)]
-- list comprehension
part1 xs = sum [maximum x - minimum x | x <- preprocessing xs]

-- solution problem
---- some bad performance solution
solution xs = head [x `div` y | x <- sortedXs, y <- sortedXs, x /= y, x `mod` y == 0]
  where
    sortedXs = reverse (sort xs)

part2 xs = sum [solution x | x <- preprocessing xs]

-- second part

testGroupPart1 :: TestTree
testGroupPart1 =
  testGroup
    "Part1"
    [ testCase "Test 1" (assertEqual "" 18 (part1 "5\t1\t9\t5\n7\t5\t3\n2\t4\t6\t8")),
      testCase "Test 2" (assertEqual "" 1014 (part1 "52\t23\t923\t54\n73\t15\t23\n2\t24\t46\t58"))
    ]

testGroupPart2 :: TestTree
testGroupPart2 =
  testGroup
    "Part2"
    [ testCase "Test 1" (assertEqual "" 9 (part2 "5\t9\t2\t8\n9\t4\t7\t3\n3\t8\t6\t5"))
    ]

testGroupPart :: TestTree
testGroupPart =
  testGroup
    "All Parts"
    [ testGroupPart1,
      testGroupPart2
    ]

-- Main test suite
main :: IO ()
main = do
  args <- getArgs
  if not (null args) && head args == "test"
    then do
      withArgs [] $ -- This line provides an empty list of arguments to defaultMain
      -- This line provides an empty list of arguments to defaultMain
        defaultMain $
          testGroup
            "AdventCodeTest"
            [testGroupPart]
    else do
      input <- getInput 2017 2
      let result1 = case input of
            Right val -> part1 val
            Left err -> error err
      let result2 = case input of
            Right val -> part2 val
            Left err -> error err
      putStrLn $ "The result of part1 is: " ++ show result1
      putStrLn $ "The result of part2 is: " ++ show result2
