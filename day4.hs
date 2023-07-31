-- NOTE:https://adventofcode.com/2017/day/4
-- Using QuickCheck
import AdventOfCodeAPI
import Data.ByteString.Lazy qualified as LBS
import Data.Char (digitToInt)
import Data.Either (fromLeft, fromRight)
import Data.List (group, nub, sort)
import Data.List.Split (splitOn)
import System.Environment (getArgs, withArgs)
import Test.Tasty
import Test.Tasty.HUnit

part1 :: [Char] -> Int
part2 :: [Char] -> Int
-- first part

---https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
--- nub

---O(NlogN)
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort


setdups :: (Ord a) => [[a]] -> [[a]]
setdups  =  map head . group . sort . map sort

--- NOTE: too ugly code .
-- preprocessing :: [Char] -> [[[Char]]]
-- preprocessing xs = map (splitOn " ") (splitOn "\n" xs)

-- part1 xs = sum $ map (\x -> if length (nub x) ==length x then 1 else 0) (preprocessing xs)

-- part2 xs = sum $ map (\x -> if length (setdups x) == length x then 1 else 0) (preprocessing xs)

--- GPT refine code

--- list comprehension beautiful
part1 xs = length [x | x <- map (splitOn " ") (splitOn "\n" xs), length (nub x) == length x]

part2 xs = length [x | x <- map (splitOn " ") (splitOn "\n" xs), length (setdups x) == length x]

testGroupPart1 :: TestTree
testGroupPart1 =
  testGroup
    "Part1"
    [ testCase "Test 1" (assertEqual "" 2 (part1 "aa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa"))
    ]

testGroupPart2 :: TestTree
testGroupPart2 =
  testGroup
    "Part2"
    [ testCase "Test 1:" (assertEqual "" 1 (part2 "abcde fghij")),
      testCase "Test 2:" (assertEqual "" 0 (part2 "abcde xyz ecdab")),
      testCase "Test 3:" (assertEqual "" 1 (part2 "a ab abc abd abf abj")),
      testCase "Test 4:" (assertEqual "" 1 (part2 "iiii oiii ooii oooi oooo")),
      testCase "Test 5:" (assertEqual "" 0 (part2 "oiii ioii iioi iiio"))
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
        defaultMain $
          testGroup
            "AdventCodeTest"
            [testGroupPart]
    else do
      input <- getInput 2017 4
      let result1 = case input of
            Right val -> part1 val
            Left err -> error err
      let result2 = case input of
            Right val -> part2 val
            Left err -> error err
      putStrLn $ "The result of part1 is: " ++ show result1
      putStrLn $ "The result of part2 is: " ++ show result2
