module Main where

import       Risk

import       Control.Monad.Random

import       Data.Bool

-- main function 
main :: IO ()
main =
  do
    putStrLn "Hello! Time to run some risk simulations!\n"
    runSimulations
    userLoop
    putStrLn "\nAlles ist wohl, dass wohl endet" 
    
-- function to create indefinite loop; is there a better way to implement this? 
userLoop :: IO () 
userLoop =
  do
    putStrLn "\nIf you would like to exit, enter 'e'"
    putStrLn "Otherwise, press any key"
    input <- getLine

    bool (runSimulations >> userLoop) (return ()) (input == "e")

-- function to run the simulations and print the results
runSimulations :: IO ()
runSimulations = 
  do
    putStrLn "\nEnter number of attackers: "
    a <- getLine
    putStrLn "Enter number of defenders: "
    d <- getLine
    putStrLn "The probability of attacker success is: "  
    ((>>= putStrLn . show) . evalRandIO . successProb) $
      Battlefield {attackers = read a , defenders = read d}
