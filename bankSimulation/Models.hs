module Models where


import Control.Monad.Random

import Data.Functor.Identity

import Control.Monad.Primitive

import Data.Bifunctor

import Data.Bool

import Data.List

-- to simulate a random value between 0 and 1
roll :: Rand StdGen Double
roll = getRandom
-- to view the double value in stdout, use evalRandIO (roll); the result is an
-- IO double 


-- the alpha distribution to model people arriving 
alpha :: Double
alpha = 100

f :: Double -> Double
f t = 1.0 - exp (-t / alpha)
-- Double is an instance of the fractional class, for which real division (/) and
-- exponentiation are defined

-- the beta distribution to model customer waiting times
rho :: Double
rho = 200

type DistParams = (Int , Int)

yDist, rDist, bDist  :: DistParams
yDist = (2 , 5)
rDist = (2 , 2)
bDist = (5 , 1)

g :: Double -> DistParams -> Double
g x dist =
  let (alpha , beta) = dist
  in rho * (x ^^ (alpha - 1)) * ((1 - x) ^^ (beta - 1)) 














