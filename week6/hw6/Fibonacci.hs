{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
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
fibs2 = makeFibList 0 1
  where
    makeFibList a b = a : makeFibList (b) (a + b)

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
  show  = show . (take 20) . streamToList 

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


{- Exercise 5

Now that we have some tools for working with streams, let s create
a few: -}

{- Define the stream

nats :: Stream Integer

which contains the infinite list of natural numbers 0, 1, 2, . . . -}

nats :: Stream Integer
nats = streamFromSeed (\n -> n + 1) 0

{- Define the stream

ruler :: Stream Integer

which corresponds to the ruler function
0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
where the nth element in the stream (assuming the first element
corresponds to n = 1) is the largest power of 2 which evenly
divides n.

Hint: define a function
interleaveStreams which alternates
the elements from two streams. Can
you use this function to implement
ruler in a clever way that does not have
to do any divisibility testing?
-}

-- interleaveStreams (Cons v s) (Cons v' s') =
--   Cons v (Cons v' (interleaveStreams s s'))

{- we discard the above definition of interleaveStreams because it eagerly unfolds
the second stream; this is not required; instead, only unfold the first stream, at
it to the interleaving stream with the Cons constructor, and then construct the rest
of the stream by invoing interleaveStreams on the second stream, and the remaining
part of the first stream -}

interleaveStreams (Cons v s) s' =
  Cons v (interleaveStreams s' s)

testStream = interleaveStreams
             (streamFromSeed (\n -> n) 0)
             (streamFromSeed (\n -> n) 1)

{- we can use interleaveStreams to construct ruler owing to the following interesting
observation.

The stream appears as follows:
0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3 ...

Clearly, a stream of 0s is interleaved with the stream:
1, 2, 1, 3, 1, 2, 1, 4, 1, 2, 1, 3 ...

But this steam is simply a stream of 1s interleaved with the stream:
2, 3, 2, 4, 2, 3 ...

Which is again just a stream of 2s interleaved with the stream:
3, 4, 3 ...

The following definition of ruler captures this interleaving: -}

ruler :: Stream Integer
ruler =
  constructRuler 0 where
  constructRuler n =
    interleaveStreams
    (streamFromSeed (\n -> n) n)
    (constructRuler (n + 1))

-- (think about the math behind this)


{- Fibonacci numbers via generating functions 

The essential idea is to work with generating functions of the form

a0 + a_1x + a_2x^2 + · · · + a_nx^n + . . .

where x is just a “formal parameter” (that is, we will never actually
substitute any values for x; we just use it as a placeholder) and all the
coefficients a_i are integers.

We will store the coefficients a_0, a_1, a_2, . . .
in a Stream Integer. -}

{- First, define

x :: Stream Integer

by noting that

x = 0 + 1x + 0x^2 + 0x^3 + . . . . -} 

x :: Stream Integer
x = Cons 0 (Cons 1 (streamFromSeed (\n -> n) 0))

{- 
Define an instance of the Num type class for Stream Integer. Note that you will
have to add {-# LANGUAGE FlexibleInstances #-} to the top of your .hs file in order
for this instance to be allowed.

Here’s what should go in your Num instance:

– You should implement the fromInteger function. Note that
n = n + 0x + 0x^2 + 0x^3 + . . . 

– You should implement negate: to negate a generating function,
negate all its coefficients.

– You should implement (+), which works like you would expect:
(a_0 + a_1x + a_2x^2 + . . .) + (b_0 + b_1^x + b_2x^2 + . . .) =
(a_0 + b_0) + (a_1 + b_1)x + (a_2 + b_2)x^2 + . . .

– Multiplication is a bit trickier. Suppose A = a_0 + xA' and
B = b_0 + xB' are two generating functions we wish to multiply.

We reason as follows:
AB = (a_0 + xA')B
= a_0B + xA'B
= a_0(b_0 + xB') + xA'B
= a_0b_0 + x(a_0B'+ A'B)

That is, the first element of the product AB is the product of
the first elements, a_0b_0; the remainder of the coefficient stream
(the part after the x) is formed by multiplying every element in
B' (that is, the tail of B) by a0, and to this adding the result of
multiplying A' (the tail of A) by B -}

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamFromSeed (\n -> n) 0)
  
  negate (Cons a0 s) = Cons (-a0) (negate s)
  
  (+) (Cons a0 s) (Cons b0 s') = Cons (a0 + b0) (s + s')
  
  (*) (Cons a0 restOfA) (sb @ (Cons b0 restOfB))
    = Cons (a0 * b0) ((numTimesStream a0 restOfB) + restOfA * sb)
    where
      numTimesStream n (Cons v s') = Cons (n * v) (numTimesStream n s')
                                     
{- The penultimate step is to implement an instance of the Fractional
class for Stream Integer. Here the important method to define is
division, (/). I won’t bother deriving it (though it isn’t hard), but
it turns out that if A = a_0 + xA' and B = b_0 + xB'
, then A/B = Q,
where Q is defined as

Q = (a_0/b_0) + x((1/b_0)(A'− QB')).

That is, the first element of the result is a0/b0; the remainder is
formed by computing A' − QBand dividing each of its elements
by b0.
Of course, in general, this operation might not result in a stream
of Integers. However, we will only be using this instance in cases
where it does, so just use the div operation where appropriate -}

instance Fractional (Stream Integer) where
  (/) (sa @ (Cons a0 restOfA)) (sb @ (Cons b0 restOfB)) =
    let q = fromInteger (a0 `div` b0) + numDivStream (restOfA - q * restOfB) b0
    in Cons (a0 `div` b0) (numDivStream (restOfA - q * restOfB) b0)
    where
      numDivStream (Cons v s') n = Cons (v `div` n) (numDivStream s' n)

{- Consider representing the Fibonacci numbers using a generating
function,

F(x) = F_0 + F_1x + F_2x^2 + F_3x^3 + . . .

Notice that x + xF(x) + x^2F(x) = F(x):

    x
    F_0x + F_1x^2 + F_2x^3 + F_3x^4 + . . .
           F_0x_2 + F_1x_3 + F_2x_4 + . . .

-------------------------------------------
0  +  x  +  F_2x^2 + F_3x^3 + F_4x^4 + . . .

Thus x = F(x) − xF(x) − x^2F(x), and solving for F(x) we find
that

F(x) = x / (1 − x − x^2).

Translate this into an (amazing, totally sweet) definition

fibs3 :: Stream Integer
-} 

-- How to do this? According to the following web-page :

-- https://austinrochford.com/posts/2013-11-01-generating-functions-and-fibonacci-numbers.html

-- the closed form solution of F(x) must be expressed as a power series, following
-- which we may match the coefficients term by term to get the required F_ n

-- fibs3 :: Stream Integer
-- fibs3 = (x) / () 


{- Exercise 7 (Optional)

• Create a type Matrix which represents 2 × 2 matrices of Integers.

• Make an instance of the Num type class for Matrix. In fact, you only
have to implement the (*) method, since that is the only one we
will use

• We now get fast (logarithmic time) matrix exponentiation for free,
since (^) is implemented using a binary exponentiation algorithm
in terms of (*). Write a function

fib4 :: Integer -> Integer

which computes the nth Fibonacci number by raising F to the nth
power and projecting out Fn (you will also need a special case
for zero). Try computing the one millionth or even ten millionth
Fibonacci number. -}

-- defining Matrix type; the four integer elements are in the order e11 e12 e21 e22
-- w.r.t to the matrix they represent

data Matrix = Matrix Integer Integer Integer Integer
  deriving Show
  

-- instance of Num class for Matrix multiplication

instance Num Matrix where
  (*) (Matrix e11 e12 e21 e22) (Matrix e11' e12' e21' e22') =
    Matrix
    (e11 * e11' + e12 * e21')
    (e11 * e12' + e12 * e22')
    (e21 * e11' + e22 * e21')
    (e21 * e12' + e22 * e22') 

fibMatrix :: Matrix
fibMatrix = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 n
  | n == 0 = 0
  | otherwise = 
    case (fibMatrix ^ n) of
      Matrix e11 e12 e21 e22 -> e12











