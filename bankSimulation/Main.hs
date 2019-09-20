module Main where

import Control.Monad.Random

import Stats

import Models
  
main :: IO ()
main = do
  putStrLn "SIMULATIONS FOR THREE KINDS OF CUSTOMERS"
  putStrLn "Pease enter the number of seconds to run each simulation for: "
  num <- getLine
  fetchAndDisplayStats "YELLOW PEOPLE" yDist (read num)
  putStrLn ""
  fetchAndDisplayStats "RED PEOPLE" rDist (read num)
  putStrLn ""
  fetchAndDisplayStats "BLUE PEOPLE" bDist (read num)
  putStrLn ""
    
  
fetchAndDisplayStats :: String -> DistParams -> Double -> IO ()
fetchAndDisplayStats col d = 
  (>>= go) . evalRandIO . (getAllStats d)
  where
    go (Stats first second third fourth) = 
      do
        putStrLn col 
        putStrLn "\n The average customer waiting time is: "
        (putStr . show . ( / 60.0)) first
        putStrLn " Minutes" 
        putStrLn " The maximum customer waiting time is: "
        (putStr . show . (/ 60.0)) second
        putStrLn " minutes"
        putStrLn " The average queue lenght is: "
        (putStr . show) third
        putStrLn " people"
        putStrLn " The maximum queue lenght is: "
        (putStr . show) fourth
        putStrLn " people" 


  

-- prompt1 :: IO ()
-- prompt1 =
--   do
--   putStrLn "We will now run simulations to check the average and maximum YELLOW customer waiting times"
--   putStrLn "Please enter number of seconds to run the simulation for: "
--   num <- getLine
--   ((>>= go) . evalRandIO . getYellowStats) $ read num
--     where
      

-- prompt2 :: IO ()
-- prompt2 =
--   do
--   putStrLn "We will now run simulations to check the average and maximum RED customer queue lengths"
--   putStrLn "Please enter number of seconds to run the simulation for: "
--   num <- getLine
--   ((>>= go) . evalRandIO . getRedStats) $ read num
--     where
--       go p =
--         do
          


-- -- simulateBank :: IO ()
-- --
          -- simulateBank =        -- 
--   do
    





