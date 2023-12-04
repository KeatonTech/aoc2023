module Util (Parser, toChar, toWord, char, parseOrPrintError, runAoc) where

import Data.ByteString qualified as B
import System.Environment (getArgs)
import Data.Char (ord)
import Data.Void
import Data.Word (Word8)
import Text.Megaparsec
import Data.ByteString (ByteString)
import System.TimeIt (timeItNamed)

type Parser = Parsec Void ByteString

toChar :: Word8 -> Char
toChar = toEnum . fromIntegral

toWord :: Char -> Word8
toWord = fromIntegral . fromEnum

char :: Char -> Parser Word8
char c = single $ fromIntegral (ord c)

parseOrPrintError :: (Show i) => Parser i -> ByteString -> IO (Maybe i)
parseOrPrintError inputParser content =
  case parse inputParser "input" content of
    Left bundle -> do
      putStr $ errorBundlePretty bundle
      return Nothing
    Right parsed -> do
      return $ Just parsed

evaluatePart :: (Show i, Show o) => [Char] -> (i -> o) -> i -> IO ()
evaluatePart name calc input = do
  putStrLn $ "Evaluating " ++ name ++ "\n"
  value <- timeItNamed "Timing" $ return $ calc input
  putStrLn $ "Result:   " ++ show value

runAoc :: (Show i, Show o) => Parser i -> (i -> o) -> (i -> o) -> IO ()
runAoc parser solve1 solve2 = do
  [part, filepath] <- getArgs
  byteData <- B.readFile filepath
  maybeInput <- parseOrPrintError parser byteData
  case maybeInput of
    Nothing -> return ()
    Just input ->
      if read @Int part == 1
        then evaluatePart "Part 1" solve1 input
        else evaluatePart "Part 2" solve2 input