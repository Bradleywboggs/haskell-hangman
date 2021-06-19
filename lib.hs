module Lib where

import Data.Char  (toLower)
import Data.List (elemIndices, foldl')
import Data.Foldable 
import Data.Sequence as S

data Input = GuessInput | WordInput 

newtype HangmanWord =  
      HangmanWord {word :: String} deriving (Eq, Show)

toHangmanWord :: String -> HangmanWord
toHangmanWord word = HangmanWord {word = toLower <$> word}

type Blanks = String

data Guess =  
      WordGuess {w :: [Char]} 
      | LetterGuess {l :: Char} 
      | EmptyGuess deriving (Show, Eq)

inputToGuess :: String -> Guess
inputToGuess ""     = EmptyGuess
inputToGuess [l]    = LetterGuess $ toLower l
inputToGuess (l:ls) = WordGuess $ toLower <$> (l:ls)


data GameState = GameState
      { remainingGuesses :: Integer
      , isWinner :: Bool
      , wrongLetters :: [Char]
      , guessedLetters :: [Char]
      , message :: String
      , hangmanword :: HangmanWord 
      , blanks :: [Char]
      } deriving (Show, Eq)


initGameState :: HangmanWord -> GameState 
initGameState (HangmanWord word) = GameState 
      {remainingGuesses = 9
      , isWinner        = False 
      , wrongLetters    = []
      , guessedLetters  = []
      , message         = ""
      , hangmanword     = HangmanWord word
      , blanks          = '_' <$ word
      }

updateGameState :: Guess -> GameState -> GameState
updateGameState EmptyGuess gamestate = updateMessage "Please enter an actual guess dummy" gamestate

updateGameState (LetterGuess l) (GameState remainingGuesses isWinner wrongLetters guessedLetters msg (HangmanWord wrd) blanks)
      | l `elem` guessedLetters = updateMessage "guessing the same thing won't help you win. guess another letter!" (GameState remainingGuesses isWinner wrongLetters guessedLetters msg (HangmanWord wrd) blanks)
      | l `notElem` wrd         = GameState (remainingGuesses - 1) isWinner (l:wrongLetters) (l:guessedLetters) "wrong wrong mr. tong" (HangmanWord wrd) blanks
      | otherwise               = updateIsWinner $ GameState remainingGuesses isWinner wrongLetters (l:guessedLetters) "you are right" (HangmanWord wrd) (updateBlanks l wrd blanks)

updateGameState (WordGuess w) (GameState remainingGuesses isWinner wrongLetters guessedLetters msg (HangmanWord wrd) blanks) 
      |  w == wrd  = GameState remainingGuesses True wrongLetters guessedLetters msg (HangmanWord wrd) blanks 
      |  otherwise =  GameState (remainingGuesses - 1) isWinner wrongLetters guessedLetters "most wrong person ever" (HangmanWord wrd) blanks 

updateMessage :: String -> GameState -> GameState
updateMessage msg (GameState guesses isWinner wrong guessed message word blanks) = GameState guesses isWinner wrong guessed msg word blanks 

updateIsWinner :: GameState -> GameState
updateIsWinner (GameState guesses isWinner wrong guessed message (HangmanWord word) blanks) 
    | word == blanks = GameState guesses True wrong guessed message (HangmanWord word) blanks
    | otherwise      = GameState guesses isWinner wrong guessed message (HangmanWord word) blanks

updateBlanks :: Char -> String -> String -> String
updateBlanks l wrd blanks  =  toList $ S.mapWithIndex (
      \idx x -> 
            if idx `elem` elemIndices l wrd
                  then l
                  else x
                  ) (S.fromList blanks)





    

