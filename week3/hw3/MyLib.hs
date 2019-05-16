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






