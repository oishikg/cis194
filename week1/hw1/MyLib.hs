module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
Exercise 1: We need to first find the digits of a number. Define the functions

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigits should convert positive Integers to a list of digits. (For 0 or
negative inputs, toDigits should return the empty list.)

toDigitsRev should do the same, but with the digits reversed.

Example: toDigits 1234 == [1,2,3,4]
Example: toDigitsRev 1234 == [4,3,2,1]
Example: toDigits 0 == []
Example: toDigits (-17) == []
-}



toDigitsRev :: Integer -> [Integer]
revList :: [Integer] -> [Integer] 
toDigits :: Integer -> [Integer]

{- At each recursive call, take the remainer of the number when divided with 10; this
gives the last digit of that number; append a list with the digit to the digits of the
quotient -} 
toDigits n
  | n <= 0 = []
  | otherwise =  (toDigits (n `div` 10)) ++ [(n `mod` 10)]

{-helper function to reverse lists -}
revList [] = []
revList (x : xs) = (revList xs) ++ [x] -- This operation is O(1)

{- reverse list of digits -} 
toDigitsRev n = revList (toDigits n)


{-
Exercise 2:  Once we have the digits in the proper order, we need to
double every other one. Define a function

doubleEveryOther :: [Integer] -> [Integer]

Remember that doubleEveryOther should double every other number
beginning from the right, that is, the second-to-last, fourth-to-last,
. . . numbers are doubled.

Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
Example: doubleEveryOther [1,2,3] == [1,4,3]
-}

{- Reverse the given list; this way, we start at the last digit; then pattern match
and at each recursive call, double the second digit; if there is no second digit
then return the list as it is -} 
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOtherHelper :: [Integer] -> [Integer]

doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x : []) = (x : []) -- is there any 'as' construct? 
doubleEveryOtherHelper (x : x' : xs') =  x : (2 * x') : (doubleEveryOtherHelper xs')

doubleEveryOther xs = revList (doubleEveryOtherHelper (revList xs))


{- Exercise 3 The output of doubleEveryOther has a mix of one-digit
and two-digit numbers. Define the function

sumDigits :: [Integer] -> Integer

to calculate the sum of all digits.

Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22 -}

{- Traverse the list, and for each integer in the list, obtain the corresponding list
of digits and sum them up -}

sumDigits :: [Integer] -> Integer
sumDigitsHelper :: [Integer] -> Integer

sumDigitsHelper [] = 0
sumDigitsHelper (x : xs) = x + sumDigitsHelper xs 

sumDigits [] = 0
sumDigits (x : xs) = sumDigitsHelper (toDigits x) + sumDigits xs


{- Exercise 4: Define the function

validate :: Integer -> Bool

that indicates whether an Integer could be a valid credit card number.
This will use all functions defined in the previous exercises.
Example: validate 4012888888881881 = True
Example: validate 4012888888881882 = False -}

validate :: Integer -> Bool

validate n = sumDigits (doubleEveryOther(toDigits n)) `mod` 10 == 0 

{-Exercise 5: The Towers of Hanoi is a classic puzzle with a solution
that can be described recursively. Disks of different sizes are stacked
on three pegs; the goal is to get from a starting configuration with
all disks stacked on the first peg to an ending configuration with all
disks stacked on the last peg, as shown in Figure 1.

The only rules are
• you may only move one disk at a time, and
• a larger disk may never be stacked on top of a smaller one.

For example, as the first move all you can do is move the topmost,
smallest disk onto a different peg, since only one disk may be moved
at a time.

To move n discs (stacked in increasing size) from peg a to peg b
using peg c as temporary storage,

1. move n − 1 discs from a to c using b as temporary storage
2. move the top disc from a to b
3. move n − 1 discs from c to b using a as temporary storage.

For this exercise, define a function hanoi with the following type:
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

Given the number of discs and names for the three pegs, hanoi
should return a list of moves to be performed to move the stack of
discs from the first peg to the second.

Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")] -}

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

{- the 2nd parameter is the source peg, the third parmeter is the target peg, and the
4th parameter is the temporary peg -} 
hanoi 0 a b c = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)


{-6: Will think about in my free time -} 
  









