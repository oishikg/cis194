module Fibonacci where


{- Fibonacci numbers:

The Fibonacci numbers Fn are defined as the sequence of integers,
beginning with 0 and 1, where every integer in the sequence is the
sum of the previous two. That is,

F0 = 0
F1 = 1
Fn = Fn 1 + Fn 2 (n   2)

For example, the first fifteen Fibonacci numbers are

0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, . . .

It s quite likely that you ve heard of the Fibonacci numbers before.
The reason they re so famous probably has something to do with the
simplicity of their definition combined with the astounding variety of
ways that they show up in various areas of mathematics as well as art
and nature. -}


{- Exercise 1

Translate the above definition of Fibonacci numbers directly into a
recursive function definition of type

fib :: Integer -> Integer

so that fib n computes the nth Fibonacci number Fn.
Now use fib to define the infinite list of all Fibonacci numbers,
fibs1 :: [Integer] -}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Is there a simpler way to this, using [0..]?
fibs1 :: [Integer]
fibs1 = makeFibList [0..]
  where
    makeFibList (x : xs) = (fib x) : (makeFibList xs)


{- Exercise 2

When I said  we  in the previous sentence I actually meant  you .
Your task for this exercise is to come up with more efficient implementation.
Specifically, define the infinite list

fibs2 :: [Integer]

so that it has the same elements as fibs1, but computing the first n
elements of fibs2 requires only O(n) addition operations. Be sure to
use standard recursion pattern(s) from the Prelude as appropriate. -}

-- Sanity check: Implement a tail-recursive fibEff function
fibEff 0 = 0
fibEff 1 = 1
fibEff n =
  fibEffHelper 0 1 n where
  fibEffHelper n2 n1 c
    | c == 1 = n1
    | otherwise =  fibEffHelper n1 (n1 + n2) (c - 1)

-- my implementation of fibs2
fibs2 :: [Integer]
fibs2 = makeFibList [0..] 0 1
  where
    makeFibList (x : xs) fPrev2 fPrev1
      | x == 0 = 0 : (makeFibList xs 0 1)
      | x == 1 = 1 : (makeFibList xs 0 1)
      | otherwise =
          let fx = fPrev2 + fPrev1
          in fx : (makeFibList xs (fPrev1) (fx))

-- Can this be written more elegantly using a higher order prelude function?

{- Streams

We can be more explicit about infinite lists by defining a type Stream
representing lists that must be infinite. (The usual list type represents
lists that may be infinite but may also have some finite length.)

In particular, streams are like lists but with only a  cons  constructor
whereas the list type has two constructors, [] (the empty list) and
(:) (cons), there is no such thing as an empty stream. So a stream is
simply defined as an element followed by a stream. -}


{-
Define a data type of polymorphic streams, Stream.

Write a function to convert a Stream to an infinite list,

streamToList :: Stream a -> [a]

To test your Stream functions in the succeeding exercises, it will be
useful to have an instance of Show for Streams. However, if you put
deriving Show after your definition of Stream, as one usually does,
the resulting instance will try to print an entire Stream which,
of course, will never finish. Instead, you should make your own
instance of Show for Stream,

instance Show a => Show (Stream a) where
show ...

which works by showing only some prefix of a stream (say, the
first 20 elements).  -}

-- define the data type for the stream
data Stream t = Cons t (Stream t)

-- function to convert Stream to an infinite list
streamToList :: Stream a -> [a]
streamToList (Cons v s) =
  v : (streamToList s)

-- inverse listToStream function to convert infinite list into a Stream
listToStream :: [a] -> Stream a
listToStream (x : xs) =
  Cons x (listToStream xs)

-- defining instance of show for stream
instance Show a => Show (Stream a) where
-- showList is a predefined operation in Show which returns a value of type
-- ShowS. Values of this type are functions String -> String, thus allowing the
-- the resultant string to be prepended to a given string
  show s = ((showList . (take 20) . streamToList) s) ""

{- Exercise 4 :

Let s create some simple tools for working with Streams. -}


{- Write a function

streamRepeat :: a -> Stream a

which generates a stream containing infinitely many copies of the
given element.
-}

streamRepeat :: a -> Stream a
streamRepeat v = Cons v (streamRepeat v)

{-  Write a function

streamMap :: (a -> b) -> Stream a -> Stream b

which applies a function to every element of a Stream. -}

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons v s') = Cons (f v) (streamMap f s')


{-  Write a function

streamFromSeed :: (a -> a) -> a -> Stream a

which generates a Stream from a  seed  of type a, which is the
first element of the stream, and an  unfolding rule  of type a -> a
which specifies how to transform the seed into a new seed, to be
used for generating the rest of the stream. -}

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed unfold seed =
  Cons seed (streamFromSeed unfold (unfold seed))









