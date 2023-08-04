-- NOTE:https://adventofcode.com/2017/day/6
-- Using QuickCheck
import AdventOfCodeAPI (getInput)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import System.Environment (getArgs, withArgs)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

redistribute :: [Int] -> [Int]
redistribute blocks =
  let maxBlock = maximum blocks
      maxIndex = fromJust $ elemIndex maxBlock blocks
      blocks' = take maxIndex blocks ++ [0] ++ drop (maxIndex + 1) blocks
      blocksLen = length blocks
      distribute n xs = let (front, x:back) = splitAt n xs in front ++ [(x + 1)] ++ back
      redistribute' m xs index
          | m > 0     = redistribute' (m - 1) (distribute index xs) ((index + 1) `mod` blocksLen)
          | otherwise = xs
  in redistribute' maxBlock blocks' ((maxIndex + 1) `mod` blocksLen)

redistributeUntilRepeat :: ([Int], Map.Map [Int] Int) -> ([Int], Map.Map [Int] Int)
redistributeUntilRepeat (blocks, history)
  | Map.member blocks history = (blocks, history)
  | otherwise = redistributeUntilRepeat (redistribute blocks, Map.insert blocks (Map.size history) history)

getCycle :: ([Int], Map.Map [Int] Int) -> Int
getCycle (blocks, history) =
  let firstAppearance = fromJust $ Map.lookup blocks history
   in Map.size history - firstAppearance

part1 :: String -> Int
part1 str =
  let blocks = map read $ splitOn "\t" str
      finalState = redistributeUntilRepeat (blocks, Map.empty)
   in Map.size $ snd finalState

part2 :: String -> Int
part2 str =
  let blocks = map read $ splitOn "\t" str
      finalState = redistributeUntilRepeat (blocks, Map.empty)
   in getCycle finalState

testGroupPart1 :: TestTree
testGroupPart1 =
  testGroup
    "Part1"
    [ testCase "Test 1" (assertEqual "" 5 (part1 "0\t2\t7\t0"))
    ]

testGroupPart2 :: TestTree
testGroupPart2 =
  testGroup
    "Part2"
    [ testCase "Test 1" (assertEqual "" 4 (part2 "0\t2\t7\t0"))
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
      input <- getInput 2017 6
      let result1 = case input of
            Right val -> part1 val
            Left err -> error err
      let result2 = case input of
            Right val -> part2 val
            Left err -> error err
      putStrLn $ "The result of part1 is: " ++ show result1
      putStrLn $ "The result of part2 is: " ++ show result2
