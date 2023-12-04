module Main where

import Control.Exception (assert)
import Control.Monad (void)
import Data.Char (ord)
import Data.List (find)
import Data.FixedList (Cons ((:.)), FixedList3, Nil (..))
import Data.Matrix qualified as M
import Text.Megaparsec
import Text.Megaparsec.Byte (digitChar, newline)
import Data.Vector (toList)
import Util

data Symbol = GearSymbol | Other deriving (Show, Eq)

data Cell = EmptyCell | SymbolCell Symbol | DigitCell Int | SymbolAdjacentDigitCell Int deriving (Show, Eq)

type Input = M.Matrix Cell

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: Parser Input
parser = do
  lists <- some parseLine <* eof
  return $ M.fromLists lists

parseLine :: Parser [Cell]
parseLine = some parseChar <* (void newline <|> lookAhead eof)

parseChar :: Parser Cell
parseChar = parseEmptyCell <|> parseDigitCell <|> parseSymbolCell

parseEmptyCell :: Parser Cell
parseEmptyCell = char '.' >> return EmptyCell

parseDigit :: Parser Int
parseDigit = do
  byteDigit <- digitChar
  return $ fromIntegral (byteDigit - fromIntegral (ord '0'))

parseDigitCell :: Parser Cell
parseDigitCell = DigitCell <$> parseDigit

parseSymbolCell :: Parser Cell
parseSymbolCell = do
  b <- anySingleBut $ toWord '\n'
  return $ SymbolCell $ charToSymbol $ toChar b

charToSymbol :: Char -> Symbol
charToSymbol '*' = GearSymbol
charToSymbol _ = Other

neighborsSubmatrix :: M.Matrix Cell -> Int -> Int -> M.Matrix Cell
neighborsSubmatrix matrix row col = M.submatrix (max (row - 1) 1) (min (row + 1) rows) (max (col - 1) 1) (min (col + 1) cols) matrix
  where
    rows = M.nrows matrix
    cols = M.ncols matrix

isDigitCell :: Cell -> Bool
isDigitCell (DigitCell _) = True
isDigitCell (SymbolAdjacentDigitCell _) = True
isDigitCell _ = False

isSymbolCell :: Cell -> Bool
isSymbolCell (SymbolCell _) = True
isSymbolCell _ = False

hasNeighboringSymbol :: M.Matrix Cell -> Int -> Int -> Bool
hasNeighboringSymbol matrix row col = any isSymbolCell $ neighborsSubmatrix matrix row col

findSymbolAdjacentDigits :: M.Matrix Cell -> [Cell]
findSymbolAdjacentDigits matrix = do
  row <- [1 .. M.nrows matrix]
  col <- [1 .. M.ncols matrix]
  return $ case M.getElem row col matrix of
    DigitCell digit -> case hasNeighboringSymbol matrix row col of
      True -> SymbolAdjacentDigitCell digit
      False -> DigitCell digit
    cell -> cell

-- Converts [A"1", D"2", A"3", _, D"4", D"5"] into [A"123", _, D"45"]
collapseDigitsIntoNumbers :: [Cell] -> [Cell]
collapseDigitsIntoNumbers (SymbolAdjacentDigitCell a : SymbolAdjacentDigitCell b : rest) = collapseDigitsIntoNumbers (SymbolAdjacentDigitCell (a * 10 + b) : rest)
collapseDigitsIntoNumbers (SymbolAdjacentDigitCell a : DigitCell b : rest) = collapseDigitsIntoNumbers (SymbolAdjacentDigitCell (a * 10 + b) : rest)
collapseDigitsIntoNumbers (DigitCell a : SymbolAdjacentDigitCell b : rest) = collapseDigitsIntoNumbers (SymbolAdjacentDigitCell (a * 10 + b) : rest)
collapseDigitsIntoNumbers (DigitCell a : DigitCell b : rest) = collapseDigitsIntoNumbers (DigitCell (a * 10 + b) : rest)
collapseDigitsIntoNumbers (a : rest) = a : collapseDigitsIntoNumbers rest
collapseDigitsIntoNumbers [] = []

extractSymbolAdjacentDigits :: [Cell] -> [Int]
extractSymbolAdjacentDigits l = [i | SymbolAdjacentDigitCell i <- l]

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 input = do
  let cellList = collapseDigitsIntoNumbers $ findSymbolAdjacentDigits input
  let adjacentCellsList = extractSymbolAdjacentDigits cellList
  sum adjacentCellsList

type CellRow = FixedList3 Cell

type CellNeighbors = FixedList3 CellRow

-- Returns a number and the length of the number in digits
readNumber :: [Cell] -> (Int, Int)
readNumber (DigitCell a : rest) = do
  let (b, len) = readNumber rest
  (a * 10 ^ len + b, len + 1)
readNumber _ = (0, 0)

spreadNumber :: [Cell] -> [Cell]
spreadNumber [] = []
spreadNumber c =
  let (n, len) = readNumber c
   in if len == 0
        then (head c) : (spreadNumber $ tail c)
        else ((DigitCell n) <$ [1 .. len]) ++ (spreadNumber (drop len c))

-- Converts ["1", "2", "3", _, "4", "5"] into ["123", "123", "123", _, "45", "45"]
spreadNumbers :: M.Matrix Cell -> M.Matrix Cell
spreadNumbers matrix = M.fromLists $ do
  row <- [1 .. M.nrows matrix]
  return $ spreadNumber $ toList $ M.getRow row matrix

asNeighbors :: M.Matrix Cell -> CellNeighbors
asNeighbors matrix = do
  let _ = assert $ M.nrows matrix == 3
  let _ = assert $ M.ncols matrix == 3
  ((M.getElem 1 1 matrix) :. (M.getElem 1 2 matrix) :. (M.getElem 1 3 matrix) :. Nil)
    :. ((M.getElem 2 1 matrix) :. (M.getElem 2 2 matrix) :. (M.getElem 2 3 matrix) :. Nil)
    :. ((M.getElem 3 1 matrix) :. (M.getElem 3 2 matrix) :. (M.getElem 3 3 matrix) :. Nil)
    :. Nil

getAdjacentNumbers :: M.Matrix Cell -> [Int]
getAdjacentNumbers matrix = do
  let neighbors = asNeighbors matrix
  concat (fmap getNumbersInRow neighbors)

getNumbersInRow :: CellRow -> [Int]
getNumbersInRow (DigitCell a :. DigitCell b :. DigitCell c :. Nil) | a == b && b == c = [a]
getNumbersInRow (DigitCell a :. _ :. DigitCell b :. Nil) = [a, b]
getNumbersInRow row = case find isDigitCell row of
  Just (DigitCell a) -> [a]
  _ -> []

getPowerOfGear :: M.Matrix Cell -> Int -> Int -> Int
getPowerOfGear matrix row col = do
  let _ = assert $ M.getElem row col matrix == SymbolCell GearSymbol
  let neighbors = neighborsSubmatrix matrix row col
  case getAdjacentNumbers neighbors of
    [a, b] -> a * b
    _ -> 0

gearPowers :: M.Matrix Cell -> [Int]
gearPowers matrix = do
  row <- [1 .. M.nrows matrix]
  col <- [1 .. M.ncols matrix]
  return $ if M.getElem row col matrix == SymbolCell GearSymbol
    then getPowerOfGear matrix row col
    else 0

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 input = sum $ gearPowers $ spreadNumbers input

main :: IO ()
main = runAoc parser solve1 solve2
