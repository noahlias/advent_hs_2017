-- NOTE:https://adventofcode.com/2017/day/1
-- Using QuickCheck
import AdventOfCodeAPI
import Data.ByteString.Lazy qualified as LBS
import Data.Char (digitToInt)
import Data.Either (fromLeft, fromRight)
import System.Environment (getArgs, withArgs)
import Test.Tasty
import Test.Tasty.HUnit

part1 :: [Char] -> Int
part2 :: [Char] -> Int
-- first part

--- cool function
rotate :: Int -> [a] -> [a]
rotate _ [] = []

---- how to understand the rotate
rotate n xs = zipWith const (drop n (cycle xs)) xs


part1 xs = sum $ zipWith (\x y -> if x == y then digitToInt x else 0) xs (rotate 1 xs)

-- My first implement i think it's too ugly
-- part1 xs = sum (map (digitToInt . fst) (filter (uncurry (==)) (zip xs (last xs : init xs))))

--- second part
--- as the
part2 xs = sum $ zipWith (\x y -> if x == y then digitToInt x else 0) xs (rotate (div (length xs) 2) xs)

testGroupPart1 :: TestTree
testGroupPart1 =
  testGroup
    "Part1"
    [ testCase "Test 1" (assertEqual "" 3 (part1 "1122")),
      testCase "Test 2" (assertEqual "" 4 (part1 "1111")),
      testCase "Test 3" (assertEqual "" 0 (part1 "1234")),
      testCase "Test 4" (assertEqual "" 9 (part1 "91212129"))
    ]

testGroupPart2 :: TestTree
testGroupPart2 =
  testGroup
    "Part2"
    [ testCase "Test :" (assertEqual "" 6 (part2 "1212")),
      testCase "Test 2" (assertEqual "" 0 (part2 "1221")),
      testCase "Test 3" (assertEqual "" 4 (part2 "123425")),
      testCase "Test 4" (assertEqual "" 12 (part2 "123123")),
      testCase "Test 5" (assertEqual "" 4 (part2 "12131415"))
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
      input <- getInput 2017 1
      let result1 = case input of
            Right val -> part1 val
            Left err -> error err
      let result2 = case input of
            Right val -> part2 val
            Left err -> error err
      putStrLn $ "The result of part1 is: " ++ show result1
      putStrLn $ "The result of part2 is: " ++ show result2
