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
gameView gamestate = do -- (GameState remainingGuesses isWinner wrongLetters guessedLetters msg (HangmanWord wrd) blanks) = do
    putStrLn $ message gamestate
    putStrLn ""
    putStrLn $ "You have " ++ show (remainingGuesses gamestate) ++ " guesses remaining."
    putStrLn ""
    putStrLn $ "Wrong Guesses: " ++ (wrongLetters gamestate)
    putStrLn ""
    putStrLn $ foldl (\accum x -> accum ++ "    " ++ x) (blanks gamestate) []
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
   word <- getGameInput WordInput <&> inputToHangmanWord
   case word of
       Right (HangmanWord word) -> runGame $ initGameState (HangmanWord word)
       Left error               -> putStrLn error >> start



runGame :: GameState -> IO ()
runGame gamestate = do
    clearScreen
    case getGameStatus gamestate of
        Won     ->  winnerView gamestate    
        Lost    ->  loserView  gamestate
        Pending ->  do
            guess <- gameView gamestate >> getGameInput GuessInput <&> inputToGuess
            runGame $ updateGameState guess gamestate


main :: IO ()
main = welcomeView >> start
