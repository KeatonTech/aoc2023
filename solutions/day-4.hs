module Main where

import Control.Monad (void)
import Data.Bits (Bits (shiftL))
import Data.Set (Set, fromList, intersection, size)
import GHC.Integer (shiftLInteger)
import Text.Megaparsec
import Text.Megaparsec.Byte (newline, space, spaceChar, string)
import Text.Megaparsec.Byte.Lexer (decimal)
import Util

data Card = Card {number :: Int, instances :: Int, winners :: Set Int, selected :: Set Int} deriving (Show, Eq)

type Input = [Card]

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
parser :: Parser Input
parser = some parseCard <* eof

parseCard :: Parser Card
parseCard = do
  void $ string "Card"
  void $ skipSome spaceChar
  number <- decimal
  void $ string ": "
  winners <- parseIntSet
  Card number 1 winners <$> parseIntSet

parseIntSet :: Parser (Set Int)
parseIntSet = do
  numbers <- someTill (space >> decimal) (string " | " <|> string "\n")
  return $ fromList numbers

-- Returns the score for a card given the number of correct picks
calculateScore :: Int -> Int
calculateScore 0 = 0
calculateScore n = 1 `shiftL` (n - 1)

correctlySelectedNumbers :: Card -> Int
correctlySelectedNumbers card = size $ intersection (winners card) (selected card)

cardWinnings :: Card -> Int
cardWinnings card = calculateScore $ correctlySelectedNumbers card

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 input = sum [cardWinnings card | card <- input]

incrementInstances :: Int -> Card -> Card
incrementInstances amount card = card {instances = instances card + amount}

addCardsForWinnings :: [Card] -> [Card]
addCardsForWinnings [] = []
addCardsForWinnings cards =
  let firstCard = head cards
      remainingCards = tail cards
      correctlySelected = correctlySelectedNumbers firstCard
   in [incrementInstances (instances firstCard) c | c <- take correctlySelected remainingCards]
        ++ drop correctlySelected remainingCards

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 [] = 0
solve2 input = instances (head input) + solve2 (addCardsForWinnings input)

main :: IO ()
main = runAoc parser solve1 solve2
