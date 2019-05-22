{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where
{------------------------- QN 3 -------------------------}

--  Mr. Dickens s publishing company has changed their
-- minds. Instead of paying him by the word, they have decided to pay
-- him according to the scoring metric used by the immensely popular
-- game of ScrabbleTM. You must therefore update your editor implementation to count
-- Scrabble scores rather than counting words

-- Hence, the second annotation you decide to implement is one
-- to cache the ScrabbleTM score for every line in a buffer. Create a
-- Scrabble module that defines a Score type, a Monoid instance for
-- Score:

import           Sized
-- We implement the Score type as an instance of the Sized type class

-- score type
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

-- function to unwrap a score value
getScore :: Score -> Int
getScore (Score i) = i

-- Score as an instance of the Sized class
instance Sized Score where
  size (Score n) = Size n

-- (+) as defining a monoid and a semi-group on Score
instance Semigroup Score where
 (<>) = (+)

instance Monoid Score where
  mempty  = Score 0
--  mappend = (+)
    




-- Next, define the following functions:



-- score :: Char -> Score
-- scoreString :: String -> Score

-- The score function should implement the tile scoring values as
-- shown at http://www.thepixiepit.co.uk/scrabble/rules.html; any
-- characters not mentioned (punctuation, spaces, etc.) should be given
-- zero points.
scorePair :: [(Char, Int)]
scorePair =
  [ ('E', 1) , ('e', 1) , ('A', 1) , ('a', 1) , ('I', 1) , ('i', 1) , ('O', 1) ,
    ('o', 1) , ('N', 1) , ('n', 1) , ('R', 1) , ('r', 1) , ('T', 1) , ('t', 1) ,
    ('L', 1) , ('l', 1) , ('S', 1) , ('s', 1) , ('U', 1) , ('u', 1) , ('D', 2) ,
    ('d', 2) , ('G', 2) , ('g', 2) , ('B', 3) , ('b', 3) , ('C', 3) , ('c', 3) ,
    ('M', 3) , ('m', 3) , ('P', 3) , ('p', 3) , ('F', 4) , ('f', 4) , ('H', 4) ,
    ('h', 4) , ('V', 4) , ('v', 4) , ('W', 4) , ('w', 4) , ('Y', 4) , ('y', 4) ,
    ('K', 5) , ('k', 5) , ('J', 8) , ('j', 8) , ('X', 8) , ('x', 8) , ('Q', 10) ,
    ('q', 10) , ('Z', 10) , ('z', 10) ]

--  much better ways to implement this, e.g. using a balanced binary tree
score :: Char -> Score
score c =
  case (filter (\(c', s) -> c' == c) scorePair) of
    [(c, s)] -> Score s
    _        -> Score 0

scoreString :: String -> Score
scoreString = foldl (\acc c -> acc + score c) 0


-- To test that you have everything working, add the line import Scrabble
-- to the import section of your JoinList module, and write the following
-- function to test out JoinLists annotated with scores:

-- scoreLine :: String -> JoinList Score String

-- Example:
-- JoinList> scoreLine "yay " +++ scoreLine "haskell!"
-- Append (Score 23)
-- (Single (Score 9) "yay ")
-- (Single (Score 14) "haskell!")
