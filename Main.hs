{-# LANGUAGE OverloadedStrings#-}

module Main where
import System.Environment
import Lib
import qualified System.Process as SP
import Data.Functor

clearScreen :: IO ()
clearScreen = do
  _ <- SP.system "reset"
  return ()

display :: GameState -> IO ()
display gamestate = print gamestate

winnerView :: GameState -> IO ()
winnerView gamestate  = undefined

loserView :: GameState -> IO ()
loserView gamestate  = undefined

printWelcome :: IO ()
printWelcome = putStrLn "Welcome to Hangman!"

requestWordInput :: Input -> IO String
requestWordInput inputType = 
    case inputType of
        WordInput  -> putStrLn "enter a word" >> getLine
        GuessInput -> putStrLn "enter a guess" >> getLine
        


runGame :: GameState -> IO ()
runGame gamestate = do
    clearScreen 
    case gamestate of
        -- Base case 1: Winner (isWinner == True)
        GameState _ True _ _ _ _ _ ->  winnerView gamestate 
        -- Base case 2: Loser (guesses == 0)
        GameState 0 _ _ _ _ _ _    ->  loserView  gamestate 
        _                          ->  do 
            guess <- display gamestate >> requestWordInput GuessInput <&> inputToGuess
            runGame $ updateGameState guess gamestate -- continue the game and update state from the guess,
                                                      -- then recurse till we hit a base case


main :: IO ()
main = do
   printWelcome
   word <- requestWordInput WordInput <&> toHangmanWord
   runGame $ initGameState word
