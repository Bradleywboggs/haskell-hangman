module Lib where
{-# LANGUAGE Datakinds#-}
import Data.Char  (toLower)

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
inputToGuess [l]    = LetterGuess l
inputToGuess (l:ls) = WordGuess (l:ls)


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
updateGameState (LetterGuess l) gamestate = undefined 
updateGameState (WordGuess w) (GameState remainingGuesses isWinner wrongLetters guessedLetters msg (HangmanWord wrd) blanks) = 
      if  w == wrd
            then GameState remainingGuesses True wrongLetters guessedLetters msg (HangmanWord wrd) blanks 
            else GameState (remainingGuesses - 1) isWinner wrongLetters guessedLetters "Incorrect" (HangmanWord wrd) blanks 


updateIsWinner :: GameState -> GameState
updateIsWinner (GameState guesses isWinner wrong guessed message word blanks) = (GameState guesses (not isWinner) wrong guessed message word blanks)
updateMessage :: String -> GameState -> GameState
updateMessage msg (GameState guesses isWinner wrong guessed message word blanks) = GameState guesses isWinner wrong guessed msg word blanks 

updateBlanks :: Guess -> HangmanWord -> Blanks -> Blanks
updateBlanks guess (HangmanWord word) blanks  = undefined




    

