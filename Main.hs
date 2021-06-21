{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Data.Functor
import           Lib
import           System.Environment
import qualified System.Process     as SP

clearScreen :: IO ()
clearScreen = do
  _ <- SP.system "reset"
  return ()

playAgainPrompt :: IO ()
playAgainPrompt = do
    answer <- putStrLn "Would you like to play again? (Y/n)" >> getLine
    case answer of
        []  -> main
        "y" -> main
        "Y" -> main
        _   -> putStrLn "Goodbye."

gameView :: GameState -> IO ()
gameView (GameState remainingGuesses isWinner wrongLetters guessedLetters msg (HangmanWord wrd) blanks) = do
    putStrLn msg
    putStrLn ""
    putStrLn $ "You have " ++ show remainingGuesses ++ " guesses remaining."
    putStrLn ""
    putStrLn $ "Wrong Guesses: " ++ wrongLetters
    putStrLn ""
    putStrLn $ foldl (\accum x -> accum ++ "    " ++ x) blanks []
    putStrLn ""


winnerView :: GameState -> IO ()
winnerView (GameState remainingGuesses isWinner wrongLetters guessedLetters msg (HangmanWord wrd) blanks) = do
    putStrLn "Winner!"
    putStrLn ""
    putStrLn $ "The word was " ++ wrd ++ "."
    playAgainPrompt


loserView :: GameState -> IO ()
loserView (GameState remainingGuesses isWinner wrongLetters guessedLetters msg (HangmanWord wrd) blanks)  = do
    putStrLn $ "The word was " ++ wrd ++ "."
    putStrLn "You Lost. someone call the wambulance."
    playAgainPrompt


welcomeView :: IO ()
welcomeView = do
    clearScreen
    putStrLn "Welcome to Hangman!"
    putStrLn ""
    putStrLn ""


getGameInput :: Input -> IO String
getGameInput inputType =
    case inputType of
        WordInput  -> putStrLn "enter a word:" >> getLine
        GuessInput -> putStrLn "enter a guess:" >> getLine

start :: IO ()
start = do
   word <- getGameInput WordInput <&> toHangmanWord
   case word of
       Right (HangmanWord word) -> runGame $ initGameState (HangmanWord word)
       Left error               -> putStrLn error >> start



runGame :: GameState -> IO ()
runGame gamestate = do
    clearScreen
    case gamestate of
        -- Base case 1: Winner (isWinner == True)
        GameState _ True _ _ _ _ _ ->  winnerView gamestate
        -- Base case 2: Loser (guesses == 0)
        GameState 0 _ _ _ _ _ _    ->  loserView  gamestate
        -- Otherwise, display the current gamestate,
        -- get a guess,
        -- update state from the guess,
        -- and then recurse till we hit a base case
        _                          ->  do
            guess <- gameView gamestate >> getGameInput GuessInput <&> inputToGuess
            runGame $ updateGameState guess gamestate


main :: IO ()
main = welcomeView >> start
