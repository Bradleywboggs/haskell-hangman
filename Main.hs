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

gameView :: GameState -> IO ()
gameView gamestate = print gamestate

winnerView :: GameState -> IO ()
winnerView gamestate  = undefined

loserView :: GameState -> IO ()
loserView gamestate  = undefined

welcomeView :: IO ()
welcomeView = putStrLn "Welcome to Hangman!"

getGameInput :: Input -> IO String
getGameInput inputType = 
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
        -- Otherwise, display the current gamestate,
        -- get a guess, 
        -- update state from the guess,
        -- and then recurse till we hit a base case
        _                          ->  do 
            guess <- gameView gamestate >> getGameInput GuessInput <&> inputToGuess
            runGame $ updateGameState guess gamestate 


main :: IO ()
main = do
   welcomeView
   word <- getGameInput WordInput <&> toHangmanWord
   runGame $ initGameState word
