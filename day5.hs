-- NOTE:https://adventofcode.com/2017/day/5
-- Using QuickCheck
import AdventOfCodeAPI (getInput)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (digitToInt)
import Data.Either (fromLeft, fromRight)
import Data.List
  ( drop,
    head,
    length,
    lines,
    map,
    take,
    (!!),
    (++),
  )
import Data.Vector qualified as V
import System.Environment (getArgs, withArgs)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Text.Read (read)
import Prelude hiding (read)

-- part1 :: Array Int Int -> Int -> Int -> Int
-- part1 data index steps
--   | index < 0 || index >= (snd $ bounds data) = steps
--   | otherwise = part1 (data // [(index, (data ! index) + 1)]) (index + (data ! index)) (steps + 1)

part1 :: String -> Int
part1 s =
  let data' = map read $ lines s
      (steps, _, _) = until (\(_, index, _) -> index < 0 || index >= length data') (\(steps, index, data') -> (steps + 1, index + data' !! index, take index data' ++ [data' !! index + 1] ++ drop (index + 1) data')) (0, 0, data')
   in steps

part2 :: String -> Int
part2 s =
  let data' = V.fromList $ map read $ lines s
      (steps, _, _) = until (\(_, index, _) -> index < 0 || index >= V.length data') (\(steps, index, data') -> let oldIndex = data' V.! index in (steps + 1, index + oldIndex, data' V.// [(index, if oldIndex >= 3 then oldIndex - 1 else oldIndex + 1)])) (0, 0, data')
   in steps

testGroupPart1 :: TestTree
testGroupPart1 =
  testGroup
    "Part1"
    [ testCase "Test 1" (assertEqual "" 5 (part1 "0\n3\n0\n1\n-3")),
      testCase "Test 2" (assertEqual "" 1 (part1 "-34\n-28\n-16")),
      testCase "Test 3" (assertEqual "" 1 (part1 "-10\n2\n4\n2"))
    ]

testGroupPart2 :: TestTree
testGroupPart2 =
  testGroup
    "Part2"
    [ testCase "Test 1" (assertEqual "" 10 (part2 "0\n3\n0\n1\n-3"))
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
  if not (Prelude.null args) && head args == "test"
    then do
      withArgs [] $ -- This line provides an empty list of arguments to defaultMain
        defaultMain $
          testGroup
            "AdventCodeTest"
            [testGroupPart]
    else do
      input <- getInput 2017 5
      let result1 = case input of
            Right val -> part1 val
            Left err -> error err
      let result2 = case input of
            Right val -> part2 val
            Left err -> error err
      putStrLn $ "The result of part1 is: " ++ show result1
      putStrLn $ "The result of part2 is: " ++ show result2
