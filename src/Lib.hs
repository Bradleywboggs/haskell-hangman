module Lib where

--TODO: Only expose smart constructors for HangmanWord and inputToGuess not their Data constructors
--TODO: add strictness to datatypes where useful

import Data.Char  (toLower, isLetter)
import Data.List (elemIndices, foldl')
import Data.Foldable ( Foldable(toList) ) 

import Data.Sequence as S ( fromList, mapWithIndex )


data Input = GuessInput | WordInput 
newtype HangmanWord =  HangmanWord {unword :: String} deriving (Eq, Show)


inputToHangmanWord :: String -> Either String HangmanWord
inputToHangmanWord word 
   | word == []                      = Left "You didn't enter a word. Please try again."
   | not $ all isLetter word         = Left "Only letters are valid entries. Please try again."
   | otherwise                       = Right $ HangmanWord $ toLower <$> word
   

type Blanks = String

data Guess =  
      WordGuess     {w :: [Char]} 
      | LetterGuess {l :: Char} 
      | EmptyGuess 
      | InvalidGuess deriving (Show, Eq)

inputToGuess :: String -> Guess
inputToGuess ""             =  EmptyGuess
inputToGuess [l]    
      | isLetter l          = LetterGuess $ toLower l 
      | otherwise           = InvalidGuess
inputToGuess (l:ls) 
      | all isLetter (l:ls) = WordGuess $ toLower <$> (l:ls) 
      | otherwise           = InvalidGuess

data GameStatus = Won | Lost | Pending

data GameState = GameState
      { remainingGuesses :: Integer
      , isWinner :: Bool
      , wrongLetters :: [Char]
      , guessedLetters :: [Char]
      , message :: String
      , hangmanword :: HangmanWord 
      , blanks :: [Char]
      } deriving (Show, Eq)

getGameStatus :: GameState -> GameStatus
getGameStatus gs | isWinner gs == True      = Won
getGameStatus gs | remainingGuesses gs == 0 = Lost
getGameStatus gs                            = Pending

-- TODO: Refactor to us lenses to replace the verbose handling of GameState updates

initGameState :: HangmanWord -> GameState 
initGameState (HangmanWord word) = GameState 
      { remainingGuesses = 9
      , isWinner        = False 
      , wrongLetters    = []
      , guessedLetters  = []
      , message         = ""
      , hangmanword     = HangmanWord word
      , blanks          = '_' <$ word
      }

updateGameState :: Guess -> GameState -> GameState
updateGameState EmptyGuess gamestate = updateMessage "Please enter an actual guess." gamestate
updateGameState InvalidGuess gamestate = updateMessage "Please only enter valid ascii letters" gamestate

updateGameState (LetterGuess l) (GameState remainingGuesses isWinner wrongLetters guessedLetters msg (HangmanWord wrd) blanks)
      | l `elem` guessedLetters = updateMessage "guessing the same thing won't help you win. guess another letter!" (GameState remainingGuesses isWinner wrongLetters guessedLetters msg (HangmanWord wrd) blanks)
      | l `notElem` wrd         = GameState (remainingGuesses - 1) isWinner (l:wrongLetters) (l:guessedLetters) "Incorrect." (HangmanWord wrd) blanks
      | otherwise               = updateIsWinner $ GameState remainingGuesses isWinner wrongLetters (l:guessedLetters) "Correct!" (HangmanWord wrd) (updateBlanks l wrd blanks)

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
updateBlanks ltr wrd blnks  =  toList $ S.mapWithIndex (
      \idx x -> 
            if idx `elem` elemIndices ltr wrd
                  then ltr
                  else x
                  ) (S.fromList blnks)





    

