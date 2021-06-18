module Lib where

data Input = GuessInput | WordInput 

newtype HangmanWord =  
      HangmanWord {word :: String} deriving (Eq, Show)

toHangmanWord :: String -> HangmanWord
toHangmanWord word = HangmanWord {word = word}

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
updateGameState guess (GameState remainingGuesses isWinner wrongLetters guessedLetters message hangmanword blanks ) =
      case guess of
            EmptyGuess    -> undefined
            LetterGuess l -> undefined 
            WordGuess   w -> undefined


updateBlanks :: Guess -> HangmanWord -> Blanks -> Blanks
updateBlanks guess (HangmanWord word) blanks  = undefined




    

