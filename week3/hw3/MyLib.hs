module MyLib where



{- Exercise 1

Your first task is to write a function

skips :: [a] -> [[a]]

The output of skips is a list of lists. The first list in the output should
be the same as the input list. The second list in the output should
contain every second element from the input list. . . and the nth list in
the output should contain every nth element from the input list.

For example:
skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1] == [[1]]
skips [True,False] == [[True,False], [False]]
skips [] == []
Note that the output should be the same length as the input. -}

listToPairListHelper :: [a] -> Int -> [(a, Int)]
listToPairListHelper [] _ = []
listToPairListHelper (x : xs) n = (x, n) : (listToPairListHelper xs (n + 1))

{- given a list, convert to a list of pairs such that the nth element x is represented
in this list of pairs as (n,x) -}
listToPairList :: [a]  -> [(a, Int)]
listToPairList xs = listToPairListHelper xs 1

{- covert a list of pairs back to the original list -}
pairListToList :: [(a, Int)] -> [a]
pairListToList xs = map (\(n, _) -> n) xs

{- given a list, obtain the required output for skips by :

 (a) converting the list into a list of pairs
 (b) filtering the list based on n
 (c) converting the pair list back to the list

the functions for (a), (b), (c) are composed to give the resultant skippedList, which
is then appended to the accumulator acc-}

skipsHelper :: [a] -> [[a]] -> Int -> [[a]]
skipsHelper xs acc n
  | n == 0 = acc
  | otherwise =
      let skippedList = 
            ((pairListToList . (filter (\(_, x) -> x `mod` n == 0 ))
               . listToPairList) xs) 
      in skipsHelper xs (skippedList : acc) (n - 1) 
  
{- Non-tail-recursive version
skipsHelper :: [a] -> Int -> [[a]]
skipsHelper xs n
  | n == (length xs + 1) = []
  | otherwise =
    ((pairListToList . (filter (\(_, x) -> x `mod` n == 0 )) . listToPairList) xs)
      : (skipsHelper xs (n + 1)) -}

{- see comment for skipsHelper-}
skips :: [a] -> [[a]]
skips xs = skipsHelper xs [] (length xs) 



{- Exercise 2 : Local maxima

A local maximum of a list is an element of the list which is strictly
greater than both the elements immediately before and after it. For
example, in the list [2,3,4,1,5], the only local maximum is 4, since
it is greater than the elements immediately before and after it (3 and
1). 5 is not a local maximum since there is no element that comes
after it.

Write a function

localMaxima :: [Integer] -> [Integer]

which finds all the local maxima in the input list and returns them in
order. For example:
localMaxima [2,9,5,6,1] == [9,6]
localMaxima [2,3,4,1,5] == [4]
localMaxima [1,2,3,4,5] == [] -} 

{- algebraic data-type to capture the notion of the neighbouring values of a given
element in the list; the neighbourhood of some value v is the value
Neighbourhood prev v next, where prev is the value before v and next is the value
after v -} 
data Neighbourhood = Neighbourhood Integer Integer Integer
                   | MissingNeighbour Integer
  deriving (Show, Eq)

{- function to take a list, and convert each element into its correponsing
neighbourhood -}
intListToNeighbourhoodListHelper :: (Maybe Integer) -> [Integer] -> [Neighbourhood]

intListToNeighbourhoodListHelper prev [] = []
intListToNeighbourhoodListHelper prev (x : []) = [MissingNeighbour x]
intListToNeighbourhoodListHelper Nothing (x : xs) =
  (MissingNeighbour x) : (intListToNeighbourhoodListHelper (Just x) xs) 
intListToNeighbourhoodListHelper (Just prev) (x : x' : xs') =
  (Neighbourhood prev x  x') :
  (intListToNeighbourhoodListHelper (Just x) (x' : xs'))


intListToNeighbourhoodList :: [Integer] -> [Neighbourhood]
intListToNeighbourhoodList = intListToNeighbourhoodListHelper Nothing 

{-function to take a neighbourhood list and convert it back to the integer list -} 
neighbourhoodListToIntList :: [Neighbourhood] -> [Integer]
neighbourhoodListToIntList =
  map (\x ->
         case x of
           Neighbourhood _ v _ -> v
           MissingNeighbour v -> v)


{- Function to find local maxima -} 
localMaxima :: [Integer] -> [Integer] 

localMaxima  = neighbourhoodListToIntList .
               (filter (\x ->
                          case x of
                            MissingNeighbour _ -> False
                            Neighbourhood l v r -> (v > l) && (v > r)))
                              . intListToNeighbourhoodList 

{-Exercise 3 : Histogram

For this task, write a function

histogram :: [Integer] -> String

which takes as input a list of Integers between 0 and 9 (inclusive),
and outputs a vertical histogram showing how many of each number
were in the input list. You may assume that the input list does not
contain any numbers less than zero or greater than 9 (that is, it does
not matter what your function does if the input does contain such
numbers). Your output must exactly match the output shown in the
examples below.

histogram [1,1,1,5] ==

*
*
* *
==========
0123456789

histogram [1,4,5,4,6,6,3,4,2,4,9] ==
*
*
* *
****** *
==========
0123456789

Important note: If you type something like histogram [3,5] at
the ghci prompt, you should see something like this:

" * * \n==========\n0123456789\n"

This is a textual representation of the String output, including \n
escape sequences to indicate newline characters. To actually visualize
the histogram as in the examples above, use putStr, for example,
putStr (histogram [3,5]). -}

{- type to capture frequency of digits -}
data Frequency = Frequency Int
  deriving (Show, Eq)

{- Note that a frequency list is  a list of pairs such that
a pair (p, Frequency f) implies that the digit p occurs f times in the input list 
-} 

constructFrequencyListHelper :: [Int] -> Int -> [Frequency]
constructFrequencyListHelper xs n 
  | n == 10 = []
  | otherwise =
    let freqN = (length . (filter (\x -> x == n))) xs
    in (Frequency freqN) : (constructFrequencyListHelper xs (n + 1))
{-- Note: this function could be implemented much more efficiently if we first
sorted xs, and then dropped freqN elements at each recursive call -}

{- function to take a list of digits and construct a frequency list -}
{-constructFrequencyList :: [Int] -> [FrequencyList] -}
constructFrequencyList :: [Int] -> [Frequency]
constructFrequencyList xs =
  constructFrequencyListHelper xs 0


{- function to generate a row in the output string -}
constructRow :: [Frequency] -> String
constructRow fs =
  foldl (\str (Frequency f) ->
            case (compare f 0) of
              EQ -> ' ' : str
              _ -> '*' : str) "" fs

{- function which, given an input frequency list, reduces only positive frequency
values by 1 -}
reduceFrequencyByOne :: [Frequency] -> [Frequency]
reduceFrequencyByOne =
  map (\ (p @ (Frequency f)) ->
         case (compare f 0) of
           EQ -> p
           _ -> (Frequency (f-1)))

{- function to check if a given frequency list has any positive frequency values left
-}
frequencyListNotEmpty :: [Frequency] -> Bool
frequencyListNotEmpty =
  any (\(Frequency f) -> f > 0) 

{- function to help construct the entire histogram -}
constructStrHelper :: [Frequency] -> String -> String
constructStrHelper fs str
  | frequencyListNotEmpty fs =
    let newStr = ((reverse . ('\n':) . constructRow) fs) ++ str
    in constructStrHelper (reduceFrequencyByOne fs) newStr
  | otherwise =
    str 

{- function to construct the histogram -}
constructString :: [Frequency] -> String
constructString fs =
  let baseOfStr = "         \n=========\n         \n0123456789"
  in constructStrHelper fs baseOfStr


{- function to construct histogram from a list of digits; first construct a frequency
list (see type definition above), and then recursively construct the rows, using
the frequencies as decreasing arguments -}
histogram :: [Int] -> String
histogram =  constructString . constructFrequencyList








