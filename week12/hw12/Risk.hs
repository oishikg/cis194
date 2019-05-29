{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import       Control.Monad.Random

import       Data.Functor.Identity

import       Control.Monad.Primitive

import       Data.Bifunctor

import       Data.Bool

import       Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

-- first :: (a -> b) -> (a, c) -> (b, c)
-- first f (a, c) = (f a, c)
-- use the implementation in bifunctor module

-- define Random instance of DieValue so that it can be used as an instance of
-- MonadRandom 
instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom


------------------------------------------------------------
-- Risk

------------------------- The rules of risk -------------------------

-- The rules of attacking in Risk are as follows.

-- • There is an attacking army (containing some number of units) and
-- a defending army (containing some number of units).

-- • The attacking player may attack with up to three units at a time.
-- However, they must always leave at least one unit behind. That
-- is, if they only have three total units in their army they may only
-- attack with two, and so on.

-- • The defending player may defend with up to two units (or only
-- one if that is all they have).

-- • To determine the outcome of a single battle, the attacking and
-- defending players each roll one six-sided die for every unit they
-- have attacking or defending. So the attacking player rolls one, two,
-- or three dice, and the defending player rolls one or two dice.

-- • The attacking player sorts their dice rolls in descending order. The
-- defending player does the same.

-- • The dice are then matched up in pairs, starting with the highest
-- roll of each player, then the second-highest.

-- • For each pair, if the attacking player’s roll is higher, then one of
-- the defending player’s units die. If there is a tie, or the defending
-- player’s roll is higher, then one of the attacking player’s units die.

-- For example, suppose player A has 3 units and player B has 5. A
-- can attack with only 2 units, and B can defend with 2 units. So A
-- rolls 2 dice, and B does the same. Suppose A rolls a 3 and a 5, and B
-- rolls a 4 and a 3. After sorting and pairing up the rolls, we have

-- A B
-- 5 4
-- 3 3

-- A wins the first matchup (5 vs. 4), so one of B’s units dies. The second
-- matchup is won by B, however (since B wins ties), so one of A’s
-- units dies. The end result is that now A has 2 units and B has 4. If
-- A wanted to attack again they would only be able to attack with 1
-- unit (whereas B would still get to defend with 2—clearly this would
-- give B an advantage because the higher of B’s two dice rolls will get
-- matched with A’s single roll.)

------------------------- QN 1 -------------------------
-- Import the MonadRandom package

------------------------- QN 2 -------------------------

type Army = Integer

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- write a function with the type

-- battle :: Battlefield -> Rand StdGen Battlefield

-- which simulates a single battle (as explained above) between two
-- opposing armies. That is, it should simulate randomly rolling the
-- appropriate number of dice, interpreting the results, and updating
-- the two armies to reflect casualties. You may assume that each player
-- will attack or defend with the maximum number of units they are
-- allowed.



-- from the lecture notes, to get a monad of a list of
makeMonadOfSequence :: Monad m => [m a] -> m [a]
makeMonadOfSequence [] = return []
makeMonadOfSequence (ma : mas) =
  ma >>= \a ->
  makeMonadOfSequence mas >>= \as ->
  return (a : as)


-- given a function to construct a monad, and an integer n, construct a list of
-- n monads
makeSequenceOfMonads :: Monad m => (Int -> m a) -> Int -> [m a]
makeSequenceOfMonads f n = f <$> [1..n]


-- simulate n die rolls by making a sequence of n die roll monads (Rand)
-- and then applying makeSequenceOfMonads
nDieRolls :: Int -> Rand StdGen [DieValue] 
nDieRolls n =
  makeMonadOfSequence $ makeSequenceOfMonads (\_ -> die >>= return) n


-- take the die rolls and compute the number of soldiers lost by the respective
-- sides as a pair; do so by zipping the lists and comparing each element pairwise 
soldiersLost ::  [DieValue] -> [DieValue] -> (Army , Army) 
soldiersLost as ds = go (reverse $ sort as) (reverse $ sort ds)
  where
    go sortedAs sortedDs = 
      (foldr
        (\(a , d) (ar , dr) ->
           bool (ar + 1 , dr) (ar , dr + 1) (a > d)) --DieValue inherits Ord
        (0 , 0)) (zip sortedAs sortedDs)


-- take the battlefield and the number of die rolls for the attackers and the
-- defenders, simulate the die rolls, calculate the soldiers lost, and update
updateBattleField :: Battlefield -> Int -> Int -> Rand StdGen Battlefield
updateBattleField b a d = 
  nDieRolls a >>= \attackerRolls ->
  nDieRolls d >>= \defenderRolls ->
  let lostSoliders = soldiersLost attackerRolls defenderRolls
  in return $  b {attackers = attackers b - fst lostSoliders ,
                   defenders = defenders b - snd lostSoliders}
-- could also use the do construct to make this simpler; using the >>= for practice 


-- battle function to update battle field, relies on helper functions defined above
battle :: Battlefield -> Rand StdGen Battlefield
battle b
  -- 3 rolls for the attacker, 2 for the defender
  | attackers b > 3 && defenders b > 1 = updateBattleField b 3 2
  -- 3 rolls for the attacker, 1 for the defender   
  | attackers b > 3 && defenders b == 1 = updateBattleField b 3 1
  -- 2 rolls for the attacker, 2 for the defender
  | attackers b == 3 && defenders b > 1 = updateBattleField b 2 2
  -- 2 rolls for the attacker, 1 for the defender
  | attackers b == 3 && defenders b == 1 = updateBattleField b 2 1
  -- 1 roll for the attacker, 2 for the defender
  | attackers b == 2 && defenders b > 1 = updateBattleField b 1 2
  -- 1 roll for the attacker, 1 for the defender
  | attackers b == 2 && defenders b == 1 = updateBattleField b 1 1
  
--  no attack possible if attacker b = 1

------------------------- QN 3 -------------------------


-- Of course, usually an attacker does not stop after just a single
-- battle, but attacks repeatedly in an attempt to destroy the entire defending
-- army (and thus take over its territory).
-- Now implement a function

-- invade :: Battlefield -> Rand StdGen Battlefield

-- which simulates an entire invasion attempt, that is, repeated calls
-- to battle until there are no defenders remaining, or fewer than two
-- attackers.

invade :: Battlefield -> Rand StdGen Battlefield
invade b =
  battle b >>= \b' ->
  bool (invade b') (return b') (attackers b' < 2 || defenders b' == 0)


------------------------- QN 4 -------------------------

-- Finally, implement a function

-- successProb :: Battlefield -> Rand StdGen Double

-- which runs invade 1000 times, and uses the results to compute a
-- Double between 0 and 1 representing the estimated probability that
-- the attacking army will completely destroy the defending army.
-- For example, if the defending army is destroyed in 300 of the 1000
-- simulations (but the attacking army is reduced to 1 unit in the other
-- 700), successProb should return 0.3.

successProb :: Battlefield -> Rand StdGen Double
successProb b =
  makeMonadOfSequence ((const $ invade b) <$> [1..1000]) >>=
  return . (/ 1000) . sum . (fmap (\b -> bool 0 1 (defenders b == 0)))
  

------------------------- QN 5 -------------------------

-- Write a function

-- exactSuccessProb :: Battlefield -> Double

-- which computes the exact probability of success based on principles
-- of probability, without running any simulations. (This won’t give you
-- any particular practice with Haskell; it’s just a potentially interesting
-- challenge in probability theory.)

-- Think about this in my free time
  












