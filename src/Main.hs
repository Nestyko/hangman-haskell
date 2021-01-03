module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, isJust)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

type WordList = [String]

data Difficulty
  = Easy
  | Medium
  | Hard
  deriving (Ord, Show, Eq)

difficulties :: Int -> Difficulty
difficulties l
  | l < 4 = Easy
  | l < 6 = Medium
  | l > 6 = Hard
  | otherwise = Medium

getDifficulty :: Puzzle -> Difficulty
getDifficulty (Puzzle word _ _) = difficulties (length word)

attempts :: Difficulty -> Int
attempts d = case d of
  Easy -> 8
  Medium -> 12
  Hard -> 15

getMaxAttempts :: Puzzle -> Int
getMaxAttempts = attempts . getDifficulty

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

remainingAttempts :: Puzzle -> Int
remainingAttempts p@(Puzzle _ _ guessed) = getMaxAttempts p - length guessed

instance Show Puzzle where
  show p@(Puzzle _ discovered guessed) =
    "\n" ++ intersperse ' ' (fmap renderPuzzleChar discovered) ++ "\nGuessed so far: " ++ guessed ++ "\nRemaining Attempts: " ++ show (remainingAttempts p) ++ "\nDifficulty: " ++ show (getDifficulty p)

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s pzList []
  where
    pzList = replicate (length s) Nothing

charInWord :: Puzzle -> Char -> Bool
charInWord
  (Puzzle w _ _)
  c = c `elem` w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed
  (Puzzle _ _ guessed)
  c = c `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter
  (Puzzle word filledInSoFar s)
  c =
    Puzzle word newFilledInSoFar (c : s)
    where
      zipper guessed wordChar guessChar =
        if wordChar == guessed
          then Just wordChar
          else guessChar
      newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn
        "You already guessed that\
        \ character, pick \
        \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn
        "This character was in the\
        \ word, filing in the word\
        \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn
        "This character wasn't in\
        \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle wordToGuess filledInSoFar guessed) =
  if (length guessed) > getMaxAttempts p
    then do
      putStrLn "You lose!"
      putStrLn $
        "The word was: " ++ wordToGuess
      exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _) =
  if all isJust filledInSoFar
    then do
      putStrLn "You win!" ++ "\n The word was: " ++ word
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ ->
      putStrLn
        "Your guess must\
        \ be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle