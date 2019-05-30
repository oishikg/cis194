module Stats where

import Control.Monad.Random

import Data.Functor.Identity

import Control.Monad.Primitive

import Data.Bifunctor

import Data.Bool

import Data.List

import Bank 

import Models

data Stats = Stats Double Double Double Double
-- avg waiting time, max waiting time, avg queue lenght, max queue length 

-- function to get all stats 
getAllStats :: DistParams -> Double -> Rand StdGen Stats 
getAllStats d t = 
  foldr (\_ bm -> simulateOneSecond bm d) (return newBank) [1..t]
  >>= \b ->
  let waitingTimes = go1 b
  in let queueLengths = go2 b
  in
    return $
    Stats (fst waitingTimes) (snd waitingTimes) (fst queueLengths) (snd queueLengths)
  where
    go1 b =
      let waitingTimes =  timeWaited <$> queue b
      in (sum waitingTimes / fromIntegral (length $ queue b) , maximum waitingTimes)
    go2 b =
      let queueLengths = recordQueueLength b
      in (fromIntegral (sum queueLengths) / fromIntegral (length queueLengths) ,
          fromIntegral (maximum queueLengths))
         -- converting to double is safe since all integers have double representation 

-- test :: Integer -> DistParams -> IO ()
-- test t d =
--   evalRandIO (foldr (\_ bm -> simulateOneSecond bm d) (return newBank) [1..t])
--   >>= \b ->
--   (putStr . show) (filter (\c -> (inQueue c) == False) $ queue b)

      




