module MyLib where

{- enumeration type using different constructors -} 
data Thing = Shoe  -- the keyword data is similar to the keyword type
           | Ship 
           | SealingWax 
           | Cabbage 
           | King
  deriving Show

shoe :: Thing
shoe = Shoe 

things :: [Thing]
things = [Shoe, Ship, SealingWax] 


{- Haskell has general algebraic data-types -}

{- implementation of an integer option data type -}
data IntOption = None
               |Some Integer
  deriving Show

optVal1 :: IntOption
optVal1 = None
optVal2 :: IntOption
optVal2 = Some 3

foo :: IntOption -> Bool
foo None = False
foo (Some x) = True

safeDivision :: Integer -> Integer -> IntOption
safeDivision x y
  | y == 0 = None
  | otherwise =  Some (x `div` y)
  
{- Data constructors with multiple arguments -}
data ThreeDCoords = ThreeDCoords Double Double Double
-- note that the constructor and type name are idomatically the same
point1, point2 :: ThreeDCoords
point1 = ThreeDCoords 4.5 5.3 7.8
point2 = ThreeDCoords 2.7 7.1 9.4

getXCoord, getYCoord :: ThreeDCoords -> Double
getXCoord (ThreeDCoords x _ _) = x
getYCoord (ThreeDCoords _ y _) = y

{- In general, an algebraic data type has one or more data constructors, and
each data constructor can have zero or more arguments:

data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4
-}


{- pattern matching on algebraic data types is similar to that in OCaml (or at least,
so it seems at this rudimentary level. In general, the grammar of patterns is as
follows:

pat ::= _
     |  var
     |  var @ ( pat ) -- the @ corresponds to OCaml's 'at' construct
     |  ( Constructor pat1 pat2 ... patn )
-} 

{- case expressions for pattern matching -}
sumList :: [Integer] -> Integer
sumList xs =
  case xs of
    [] -> 0
    x : xs' -> x + (sumList xs')

{- recursive data types -}    
data IntList = Nil
             | Cons Integer IntList
  deriving Show

intList1, intList2 :: IntList
intList1 = Nil
intList2 = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))

userListToLibraryList :: IntList -> [Integer]
userListToLibraryList intList =
  case intList of
    Nil -> []
    Cons n intList' -> n : (userListToLibraryList intList')

























