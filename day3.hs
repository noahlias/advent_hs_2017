-- NOTE:https://adventofcode.com/2017/day/3
-- Using QuickCheck
import AdventOfCodeAPI
import Data.ByteString.Lazy qualified as LBS
import Data.Char (digitToInt)
import Data.Either (fromLeft, fromRight)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import System.Environment (getArgs, withArgs)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Read (readMaybe)

part1 :: [Char] -> Int
part2 :: [Char] -> Int

-- first part

-- abstract the process to polars
-- the original is (0,0) while other is coordinate
-- so the Manhattan Distance  is easy just coordinate x + coordinate y

data Dir = R | D | L | U

spiralSeq :: Int -> [Dir]
-- https://stackoverflow.com/questions/57569623/coordinates-for-clockwise-outwards-spiral
--- original
--   6 - 7 - 8 - 9
--    |           |
--    5   0 - 1   10
--    |       |   |
--    4 - 3 - 2   11
--                |
--  ..15- 14- 13- 12
-- rn R ++ rn D  ++ rn1 L ++ rn1 U

---anticlockwise
-- 17  16  15  14  13
-- 18   5   4   3  12
-- 19   6   1 -  2  11
-- 20   7   8   9  10
-- 21  22  23---> ...

spiralSeq n = rn R ++ rn U ++ rn1 L ++ rn1 D
  where
    rn = replicate n
    rn1 = replicate (n + 1)

spiral :: [Dir]
spiral = concatMap spiralSeq [1, 3 ..]

move :: (Int, Int) -> Dir -> (Int, Int)
move (x, y) = go
  where
    go R = (x + 1, y)
    go D = (x, y - 1)
    go L = (x - 1, y)
    go U = (x, y + 1)

spiralPos :: [(Int, Int)]
spiralPos = scanl move (0, 0) spiral

manhattanDistance :: Num a => (a, a) -> a
manhattanDistance x = abs (fst x) + abs (snd x)

part1 xs = manhattanDistance (last (take (read xs :: Int) spiralPos))

type Coordinate = (Int, Int)

type Spiral = Map.Map Coordinate Int

-- 创建一个用于获取坐标的所有邻居的函数
neighbors :: Coordinate -> [Coordinate]
neighbors (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]

-- 创建一个用于从螺旋中获取特定坐标的值的函数
getValue :: Spiral -> Coordinate -> Int
getValue spiral coord
  | coord == (0, 0) = 1
  | otherwise = sum $ map (\c -> Map.findWithDefault 0 c spiral) $ neighbors coord

-- 递归生成螺旋值序列
spiralVals :: [Coordinate] -> Spiral -> [(Coordinate, Int)]
spiralVals (pos : positions) spiral = (pos, value) : spiralVals positions updatedSpiral
  where
    value = getValue spiral pos
    updatedSpiral = Map.insert pos value spiral

-- 修改part2以在新的Spiral结构中存储值
part2 xs = solve (read xs :: Int) spirals
  where
    spirals = spiralVals spiralPos (Map.singleton (0, 0) 1)
    solve target spirals = snd . head $ dropWhile ((<= target) . snd) spirals

testGroupPart1 :: TestTree
testGroupPart1 =
  testGroup
    "Part1"
    [ testCase "Test 1" (assertEqual "" 0 (part1 "1")),
      testCase "Test 2" (assertEqual "" 3 (part1 "12")),
      testCase "Test 3" (assertEqual "" 2 (part1 "23")),
      testCase "Test 4" (assertEqual "" 31 (part1 "1024"))
    ]

testGroupPart2 :: TestTree
testGroupPart2 =
  testGroup
    "Part2"
    [ testCase "Test 1" (assertEqual "" 54 (part2 "26")),
      testCase "Test 2" (assertEqual "" 142 (part2 "133"))
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
      input <- getInput 2017 3
      let result1 = case input of
            Right val -> part1 val
            Left err -> error err
      let result2 = case input of
            Right val -> part2 val
            Left err -> error err
      putStrLn $ "The result of part1 is: " ++ show result1
      putStrLn $ "The result of part2 is: " ++ show result2
