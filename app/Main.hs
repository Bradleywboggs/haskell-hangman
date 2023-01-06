{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Data.Functor
import           Lib
import Data.List (intersperse)
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
gameView gs = mapM_ putStrLn $ 
    intersperse "" [ message gs
                   , "You have " ++ show (remainingGuesses gs) ++ " guesses remaining."
                   , "Wrong Guesses: " ++ (wrongLetters gs)
                   , foldl (\accum x -> accum ++ "    " ++ x) (blanks gs) []
                   ]

winnerView :: GameState -> IO ()
winnerView gs = do
    mapM_ putStrLn $ intersperse "" ["Winner!", "The word was " ++ (unword $ hangmanword gs) ++ "."]
    playAgainPrompt


loserView :: GameState -> IO ()
loserView gs = do
    mapM_ putStrLn [ "The word was " ++ (unword $ hangmanword gs) ++ "."
                    , "You Lost"
                    ]
    playAgainPrompt                


welcomeView :: IO ()
welcomeView = clearScreen >> putStrLn "Welcome to Hangman!\n\n"


getGameInput :: Input -> IO String
getGameInput inputType =
    case inputType of
        WordInput  -> putStrLn "enter a word:" >> getLine
        GuessInput -> putStrLn "enter a guess:" >> getLine

start :: IO ()
start = do
   wrd <- getGameInput WordInput <&> inputToHangmanWord
   case wrd of
       Right (HangmanWord word) -> runGame $ initGameState (HangmanWord word)
       Left err                 -> putStrLn err >> start



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
