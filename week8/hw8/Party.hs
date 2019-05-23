{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Bool
import Data.Tree
{------------------------- Context -------------------------}

-- Planning the office party
-- As the most junior employee at Calculators R Us, Inc., you are tasked
-- with organizing the office Spring Break party. As with all party organizers,
-- your goal is, of course, to maximize the amount of fun
--  (as measured, of course, in Standard Transnational Fun Units, or STFUs) had at
-- the party. Since some people enjoy parties more than others,
-- you have estimated the amount of fun which will be had by each employee.
-- So simply summing together all these values should indicate
-- the amount of fun which will be had at the party in total, right?

-- . . . well, there�fs one small problem. It is a well-known fact that
-- anyone whose immediate boss is also at the party will not have any
-- fun at all. So if all the company employees are at the party, only the
-- CEO will have fun, and everyone else will stand around laughing
-- nervously and trying to look natural while looking for their boss out
-- of the corner of their eyes.

-- Your job, then, is to figure out who to invite to the party in order to
-- maximize the total amount of fun

{------------------------- Preliminaries -------------------------}

-- We have provided you with the file Employee.hs, which contains the
-- following definitions:

-- Employee names are represented by Strings.

-- type Name = String

-- The amount of fun an employee would have at the party,
-- represented by an Integer number of STFUs

-- type Fun = Integer

-- An Employee consists of a name and a fun score.

-- data Employee = Emp { empName :: Name, empFun :: Fun }
-- deriving (Show, Read, Eq)

-- It also defines testCompany :: Tree Employee, a small company
-- hierarchy which you can use for testing your code (although your
-- actual company hierarchy is much larger).

-- Finally, Employee.hs defines a type to represent guest lists. The
-- obvious possibility to represent a guest list would be [Employee].
-- However, we will frequently want to know the total amount of fun
-- had by a particular guest list, and it would be inefficient to recompute
-- it every time by adding up the fun scores for all the employees
-- in the list. Instead, a GuestList contains both a list of Employees
-- and a Fun score. Values of type GuestList should always satisfy the
-- invariant that the sum of all the Fun scores in the list of Employees
-- should be equal to the one, �gcached�h Fun score.


{------------------------- QN 1 -------------------------}

-- Now define the following tools for working with GuestLists:

{---------- 1.1 ----------}

-- A function:

-- glCons :: Employee -> GuestList -> GuestList

-- which adds an Employee to the GuestList (updating the cached
-- Fun score appropriately). Of course, in general this is impossible:
-- the updated fun score should depend on whether the Employee
-- being added is already in the list, or if any of their direct subordinates
-- are in the list, and so on. For our purposes, though, you
-- may assume that none of these special cases will hold: that is,
-- glCons should simply add the new Employee and add their fun
-- score without doing any kind of checks.

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL gs f) = GL (emp : gs) (f + empFun emp) 


{---------- 1.2 ----------}

--  A Monoid instance for GuestList.

-- Note that this requires creating an
-- �gorphan instance�h (a type class instance
-- instance C T which is defined in a
-- module which is distinct from both the
-- modules where C and T are defined),
-- which GHC will warn you about.
-- You can ignore the warning, or add
-- {-# OPTIONS_GHC -fno-warn-orphans #-}
-- to the top of your file.

instance Semigroup GuestList where
  (<>) (GL gs f) (GL gs' f') = GL (gs <> gs') (f + f')
-- Although the type Fun is an alias for Integer, Fun is not an instance of
-- semigroup; thus we must explicitly prove the (+) opertaion instead of (<>), which
--  we could use for gs and gs' since they are lists 

instance Monoid GuestList where
  mempty = GL mempty 0

{---------- 1.3 ----------}
--  A function

-- moreFun :: GuestList -> GuestList -> GuestList

-- which takes two GuestLists and returns whichever one of them
-- is more fun, i.e. has the higher fun score. (If the scores are equal it
-- does not matter which is returned.)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = bool gl1 gl2 (gl1 < gl2)
-- see GuestList instance of Ord in Employee

-- test moreFun
gl1 = GL [Emp {empName = "Bob", empFun = 2}] 2
gl2 = GL [Emp {empName = "Bob", empFun = 2},Emp {empName = "Joe", empFun = 5}] 7
gl3 = GL [Emp {empName = "Stan", empFun = 9},Emp {empName = "Bob", empFun = 2},
          Emp {empName = "Joe", empFun = 5}] 16


{------------------------- QN 2 -------------------------}

-- The Data.Tree module from the standard Haskell libraries defines
-- the type of �grose trees�h, where each node stores a data element and
-- has any number of children (i.e. a list of subtrees):

-- data Tree a = Node {
-- rootLabel :: a, -- label value
-- subForest :: [Tree a] -- zero or more child trees
-- }

-- Strangely, Data.Tree does not define a fold for this type! Rectify the
-- situation by implementing

-- treeFold :: ... -> Tree a -> b
-- (See if you can figure out what type(s) should replace the dots in
-- the type of treeFold. If you are stuck, look back at the lecture notes
-- from Week 7, or infer the proper type(s) from the remainder of this
-- assignment.)

-- Sanity check before implementing the fold: Function to calculate # of nodes
numNodes :: Tree a -> Integer
numNodes (Node {subForest = []}) = 1
numNodes (Node {rootLabel = r , subForest = ts}) =
  1 + foldr (\t a -> numNodes t + a) 0 ts 
  
-- Yet some more sanity check: Function to calculate height of the rose tree
heightRoseTree :: Tree a -> Integer
heightRoseTree (Node {subForest = []}) = 1
heightRoseTree (Node {rootLabel = r , subForest = ts}) =
  1 + foldr (\t a ->
               let h = heightRoseTree t
               in bool h a (h < a)) 0 ts
  
-- implementation of rose tree fold 
treeFold :: (a -> [r] -> r) -> Tree a -> r
treeFold node (Node {rootLabel = r , subForest = ts}) =
  node r (map (treeFold node) ts)

numNodesFold =
  treeFold (\_ rs ->
              case rs of
                [] -> 1
                _ -> (foldr (+) 0 rs) + 1)

heightFold =
  treeFold (\_ rs ->
              case rs of
                [] -> 1
                _ -> maximum rs + 1)
  
sumFunFold =
  treeFold (\e rs -> empFun e + (foldr (+) 0 rs))
              
  
-- An alternative implementation that incorporates foldr in treeFold. Which is the
-- better implementation? 

treeFoldAlternative :: b -> (r -> b -> b) -> (a -> b -> r) -> Tree a -> r
treeFoldAlternative nil cons node (Node {rootLabel = r , subForest = ts}) =
    node r (foldr cons nil (map (treeFoldAlternative nil cons node) ts))

-- sanity checks 
numNodesFoldAlternative =
  treeFoldAlternative 0 (\h a -> bool h a (h < a)) (\_ n -> n + 1)
heightFoldAlternative =
  treeFoldAlternative 0 (\h a -> bool h a (h < a)) (\_ n -> n + 1) 
sumFunFoldAlternative =
  treeFoldAlternative 0 (+) (\(Emp _ f) s -> f + s)

{------------------------- some more context ... -------------------------}
-- Now let�fs actually derive an algorithm to solve this problem. Clearly
-- there must be some sort of recursion involved�\in fact, it seems that
-- we should be able to do it with a fold.

-- starting from the bottom of the tree and working our way up, we
-- compute the best guest list for each subtree and somehow combine
-- these to decide on the guest list for the next level up, and so on. So
-- we need to write a combining function

-- combineGLs :: Employee -> [GuestList] -> GuestList

-- which takes an employee (the boss of some division) and the optimal
-- guest list for each subdivision under him, and somehow combines
-- this information to compute the best guest list for the entire division.
-- However, this obvious first attempt fails! The problem is that we
-- don�ft get enough information from the recursive calls. If the best
-- guest list for some subtree involves inviting that subtree�fs boss, then
-- we are stuck, since we might want to consider inviting the boss of the
-- entire tree�\in which case we don�ft want to invite any of the subtree
-- bosses (since they wouldn�ft have any fun anyway). But we might be
-- able to do better than just taking the best possible guest list for each
-- subtree and then excluding their bosses.
-- The solution is to generalize the recursion to compute more information,
-- in such a way that we can actually make the recursive step.
-- In particular, instead of just computing the best guest list for a given
-- tree, we will compute two guest lists:

-- 1. the best possible guest list we can create if we invite the boss (that
-- is, the Employee at the root of the tree); and

-- 2. the best possible guest list we can create if we don�ft invite the boss.
-- It turns out that this gives us enough information at each step to

-- compute the optimal two guest lists for the next level up.

{------------------------- QN 3 -------------------------}
-- Write a function

-- nextLevel :: Employee -> [(GuestList, GuestList)]  -> (GuestList, GuestList)

-- which takes two arguments. The first is the �gboss�h of the current subtree
-- (let�fs call him Bob). The second argument is a list of the results
-- for each subtree under Bob. Each result is a pair of GuestLists: the
-- first GuestList in the pair is the best possible guest list with the boss
-- of that subtree; the second is the best possible guest list without the
-- boss of that subtree. nextLevel should then compute the overall best
-- guest list that includes Bob, and the overall best guest list that doesn�ft
-- include Bob.


nextLevel :: Employee -> [(GuestList, GuestList)]  -> (GuestList, GuestList)
nextLevel b [] = ((glCons b mempty) , mempty)
nextLevel b glPairs =
  ((bestGuestListWithBoss b glPairs) , (bestGuestListWithoutBoss glPairs))
  where
    -- map across the list by checking for each pair which gl has more fun,
    -- and then add the boss to that; finally pick out the maximum gl
    bestGuestListWithBoss b =
     maximum . map (\glPair ->
                      moreFun (glCons b $ fst glPair) (glCons b $ snd glPair)) 
    -- same as for bestGuestListWithBoss, but don't add the boss 
    bestGuestListWithoutBoss =
      maximum . map (\glPair -> moreFun (fst glPair) (snd glPair))

{------------------------- QN 4 -------------------------}
-- Finally, put all of this together to define

-- maxFun :: Tree Employee -> GuestList

-- which takes a company hierarchy as input and outputs a fun-maximizing
-- guest list. You can test your function on testCompany, provided in
-- Employee.hs.

maxFun :: Tree Employee -> GuestList
maxFun ts =
  let optPair = getOptimalPair ts
  in moreFun (fst optPair) (snd optPair)
  where
    getOptimalPair =
      treeFold 
      (\e rs ->
         case rs of
           [] -> nextLevel e []
           _ -> nextLevel e rs)

  
-- implementing maxFunAlternative which uses treeFoldAlternative
maxFunAlternative :: Tree Employee -> GuestList
maxFunAlternative ts =
  let optimalPair = treeFoldAlternative [] (:) (nextLevel) ts
  in moreFun (fst optimalPair) (snd optimalPair) 
             

{------------------------- QN 5 -------------------------}
-- Implement main :: IO () so that it reads your company�fs hierarchy
-- from the file company.txt, and then prints out a formatted guest
-- list, sorted by first name, which looks like








