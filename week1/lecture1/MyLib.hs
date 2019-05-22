module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- primitive types -}

{- declaring integer variables -}
x, y, z :: Int
x = 5
y = 4
{- integer expressions -}
z = x + y


{- declaring double-precision fp -}
d1, d2, d3 :: Double
d1 = 4.5
d2 = 8.888
{- double expression -}
d3 = d1 + d2 - d1


{- declaring booleans -}
b1, b2 :: Bool
b1 = True
b2 = False


{- Unicode characters -}
c1, c2 :: Char
c1 = 'x'
c2 = 'y'

{- Strings -}
s1, s2 :: String
s1 = "Hello Haskell"
s2 = "Hello OCaml"


{- Expressions -}

{- Integer expressions -}
intExp1, intExp2, intExp3 :: Int

intExp1 = 3 + 2 * 9
intExp2 = 9 `mod` 6 `div` 3
intExp3 = div 9 3 -- the backticks enable functions to be used in infix

{- Double expressions -}
doubleExp1, doubleExp2 :: Double

doubleExp1 = d1 / d2 --the '/' sign only holds for fp values
doubleExp2 = d1 * d2 + 9.8


{- Boolean expressions -}
boolExp1, boolExp2 :: Bool

boolExp1 = (3 == 4) && (4 + 5 == 9)
boolExp2 = not (boolExp1 || False) 


{- Basic function definitions -}
fact :: Integer -> Integer

fact 0 = 1 -- base clause
fact n = n * fact (n-1) -- inductive clause

{- guards -}

{- factorial -}
factWithGuards :: Integer -> Integer

factWithGuards n
  | n == 0 = 1
  | otherwise =  n * factWithGuards (n-1)
  

{- pairs -}

pair1, pair2 :: (Int, Int)

pair1 = (3, 4)
pair2 = (4, 5)



{- Functions with multiple arguments -}

addThree :: Int -> Int -> Int

addThree x y = x + y


{- Lists -}
xs :: [Integer]

xs = [1, 2, 3, 4]

{- Standard list functions and operations:
- ++ (concat)
- : (append)
- length <list>
- head <list>
- tail <list>
etc. -}

{- As in OCaml, a list [a, b, c, d] is really
(a : (b : (c : (d : [])))); think of it as a singly linked list -}

{- The String type is an alias for [Char], so all functions and operations on
list apply to strings -} 
stringAsList :: [Char]

stringAsList = ['h', 'a', 's', 'k', 'e', 'l', 'l']


{- Functions on lists using pattern matching -}

intListLength :: [Integer] -> Integer


intListLength [] = 0
intListLength (x : xs) = 1 + intListLength xs 

{-
Bad code! Guards evaluate boolean expressions, do not use for pattern matching:
intListLength  xs
| [] = 0
| x' : xs' = 1 + intListLength xs'
-} 

{- length implemented tail recursively? -}

lengthTR :: [Integer] -> Integer
lengthTRHelper :: [Integer] -> Integer -> Integer

lengthTRHelper [] acc = acc
lengthTRHelper (x : xs) acc = lengthTRHelper xs (acc + 1)

lengthTR xs = lengthTRHelper xs 0 












