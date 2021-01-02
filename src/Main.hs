module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

main :: IO ()
main = do
  putStrLn "hello world"

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 4

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength
            && l
              < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, wordCount)
  return $ wl !! randomIndex
  where
    wordCount = (length wl) - 1

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle
  = Puzzle String [Maybe Char] [Char]

renderPuzzleChar = '*'

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed