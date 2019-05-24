{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Note: The prompt for this assignment is quite long and involved. See the pdf
-- file that has been committed if the commented version of the prompt is not
-- clear.

{------------------------- Editors and Buffers -------------------------}

-- You have a working user interface for the word processor implemented
-- in the file Editor.hs. The Editor module defines functionality
-- for working with documents implementing the Buffer type
-- class found in Buffer.hs. Take a look at Buffer.hs to see the operations
-- that a document representation must support to work with
-- the Editor module. The intention of this design is to separate the
-- front-end interface from the back-end representation, with the type
-- class intermediating the two. This allows for the easy swapping of
-- different document representation types without having to change
-- the Editor module.

-- The editor interface is as follows:
-- • v — view the current location in the document
-- • n — move to the next line
-- • p — move to the previous line
-- • l — load a file into the editor
-- • e — edit the current line
-- • q — quit
-- • ? — show this list of commands

-- To move to a specific line, enter the line number you wish to navigate
-- to at the prompt. The display shows you up to two preceding
-- and two following lines in the document surrounding the current
-- line, which is indicated by an asterisk. The prompt itself indicates the
-- current value of the entire document.

-- To run the text-buffer, use `cabal new-build` to build the project, and then
-- execute the binary with `cabal new-exec testEditor`. Here is an example of the run:

-- *115> l carol.txt
-- 235599> ?
-- v --- view the current location in the document
-- n --- move to the next line
-- p --- move to the previous line
-- l --- load a file into the editor
-- e --- edit the current line
-- q --- quit
-- ? --- show this list of commands
-- 235599> v
-- *0: The Project Gutenberg EBook of A Christmas Carol, by Charles Dickens
--  1: 
--  2: This eBook is for the use of anyone anywhere at no cost and with
-- 235599> n
--  0: The Project Gutenberg EBook of A Christmas Carol, by Charles Dickens
-- *1: 
--  2: This eBook is for the use of anyone anywhere at no cost and with
--  3: almost no restrictions whatsoever.  You may copy it, give it away or
-- 235599> ?
-- v --- view the current location in the document
-- n --- move to the next line
-- p --- move to the previous line
-- l --- load a file into the editor
-- e --- edit the current line
-- q --- quit
-- ? --- show this list of commands
-- 235599> 3640
--  3638: 
--  3639: "An intelligent boy!" said Scrooge. "A remarkable boy!
-- *3640: Do you know whether they've sold the prize Turkey that
--  3641: was hanging up there?--Not the little prize Turkey: the
--  3642: big one?"
-- 235599> e
-- Replace line 3640: "Hello Haskell"
-- 235523> 3640
--  3638: 
--  3639: "An intelligent boy!" said Scrooge. "A remarkable boy!
-- *3640: "Hello Haskell"
--  3641: was hanging up there?--Not the little prize Turkey: the
--  3642: big one?"
-- 235523> ?
-- v --- view the current location in the document
-- n --- move to the next line
-- p --- move to the previous line
-- l --- load a file into the editor
-- e --- edit the current line
-- q --- quit
-- ? --- show this list of commands
-- 235523> p
--  3637: "I should hope I did," replied the lad.
--  3638: 
-- *3639: "An intelligent boy!" said Scrooge. "A remarkable boy!
--  3640: "Hello Haskell"
--  3641: was hanging up there?--Not the little prize Turkey: the
-- 235523> p
--  3636: 
--  3637: "I should hope I did," replied the lad.
-- *3638: 
--  3639: "An intelligent boy!" said Scrooge. "A remarkable boy!
--  3640: "Hello Haskell"

-- You will be implementing a lightweight,
-- tree-like structure, both for holding the data and caching the
-- metadata. This data structure is referred to as a join-list. A data type
-- definition for such a data structure might look like this:

-- data JoinListBasic a = Empty
-- | Single a
-- | Append (JoinListBasic a) (JoinListBasic a)

-- The intent of this data structure is to directly represent append
-- operations as data constructors. This has the advantage of making
-- append an O(1) operation: sticking two JoinLists together simply
-- involves applying the Append data constructor. To make this more
-- explicit, consider the function

-- jlbToList :: JoinListBasic a -> [a]
-- jlbToList Empty = []
-- jlbToList (Single a) = [a]
-- jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

-- If jl is a JoinList, we can think of it as a representation of the list
-- jlbToList jl where some append operations have been “deferred”.

-- Such a structure makes sense for text editing applications as it
-- provides a way of breaking the document data into pieces that can
-- be processed individually, rather than having to always traverse the
-- entire document. This structure is also what you will be annotating
-- with the metadata you want to track.


-- The JoinList definition to use for this assignment is

-- data JoinList m a = Empty
-- | Single m a
-- | Append m (JoinList m a) (JoinList m a)
-- deriving (Eq, Show)

-- You should copy this definition into a Haskell module named JoinList.hs.
-- The m parameter will be used to track monoidal annotations to the
-- structure. The idea is that the annotation at the root of a JoinList
-- will always be equal to the combination of all the annotations on
-- the Single nodes (according to whatever notion of “combining” is
-- defined for the monoid in question). Empty nodes do not explicitly
-- store an annotation, but we consider them to have an annotation of
-- mempty (that is, the identity element for the given monoid).

-- For example,

-- Append (Product 210)
-- (Append (Product 30)
-- (Single (Product 5) ’y’)
-- (Append (Product 6)
-- (Single (Product 2) ’e’)
-- (Single (Product 3) ’a’)))
-- (Single (Product 7) ’h’)

-- is a join-list storing four values: the character ’y’ with annotation
-- 5, the character ’e’ with annotation 2, ’a’ with annotation 3, and
-- ’h’ with annotation 7. (See Figure 1 for a graphical representation of
-- the same structure.) Since the multiplicative monoid is being used,
-- each Append node stores the product of all the annotations below it.
-- The point of doing this is that all the subcomputations needed to
-- compute the product of all the annotations in the join-list are cached.
-- If we now change one of the annotations, say, the annotation on ’y’,
-- we need only recompute the annotations on nodes above it in the
-- tree. In particular, in this example we don’t need to descend into the
-- subtree containing ’e’ and ’a’, since we have cached the fact that
-- their product is 6. This means that for balanced join-lists, it takes
-- only O(log n) time to rebuild the annotations after making an edit.

{------------------------- END OF CONTEXT -------------------------}


module JoinList where

import           Buffer
import           Scrabble
import           Sized

-- data type for buffer
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


-- test joint list expressions
jl1 = Empty
jl2 = Single (Size 1) 'a'
jl3 = Single (Size 1) 'b'
jl4 = Append (Size 2) (Single (Size 1) 's') (Single (Size 1) 't')
jl5 = Append (Size 5)
      (Append (Size 3)
       (Append (Size 2)
        (Single (Size 1) 'H')
        (Single (Size 1) 'E'))
       (Single (Size 1) 'L'))
      (Append (Size 2)
       (Single (Size 1) 'L')
       (Single (Size 1) 'O'))

-- fold function for joinList
joinListFold ::
  (Sized b, Monoid b)
  => r
  -> (b -> a -> r)
  -> (b -> r -> r -> r)
  -> JoinList b a
  -> r
joinListFold empty single append = go
  where
    go Empty = empty
    go (Single m a) = single m a
    go (Append m l1 l2) =
        append m
        (joinListFold empty single append l1)
        (joinListFold empty single append l2)



{------------------------- QN 1 -------------------------}

-- We first consider how to write some simple operations
-- on these JoinLists. Perhaps the most important operation we will
-- consider is how to append two JoinLists. Previously, we said that
-- the point of JoinLists is to represent append operations as data, but
-- what about the annotations? Write an append function for JoinLists
-- that yields a new JoinList whose monoidal annotation is derived
-- from those of the two arguments.

-- (+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a

-- You may find it helpful to implement a helper function
-- tag :: Monoid m => JoinList m a -> m
-- which gets the annotation at the root of a JoinList.
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- Formally, Monoid constraint is too restrictive in this function. It's enough
-- to have only Semigroup m here
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) j1 j2 = Append (tag j1 <> tag j2) j1 j2


{------------------------- QN 2 -------------------------}

-- The first annotation to try out is one for fast indexing
-- into a JoinList. The idea is to cache the size (number of data elements)
-- of each subtree. This can then be used at each step to determine
-- if the desired index is in the left or the right branch.

-- We have provided the Sized module that defines the Size type,
-- which is simply a newtype wrapper around an Int. In order to make
-- Sizes more accessible, we have also defined the Sized type class
-- which provides a method for obtaining a Size from a value.
-- Use the Sized type class to write the following functions.

{------------------------- QN 2.1 -------------------------}

-- Implement the function

-- indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a

-- indexJ finds the JoinList element at the specified index. If the
-- index is out of bounds, the function returns Nothing. By an index
-- in a JoinList we mean the index in the list that it represents. That
-- is, consider a safe list indexing function

-- (!!?) :: [a] -> Int -> Maybe a
-- [] !!? _ = Nothing
-- _ !!? i | i < 0 = Nothing
-- (x:xs) !!? 0 = Just x
-- (x:xs) !!? i = xs !!? (i-1)

-- which returns Just the ith element in a list (starting at zero) if
-- such an element exists, or Nothing otherwise.

-- We also consider
-- an updated function for converting join-lists into lists, just like
-- jlbToList but ignoring the monoidal annotations:

-- jlToList :: JoinList m a -> [a]
-- jlToList Empty = []
-- jlToList (Single _ a) = [a]
-- jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- We can now specify the desired behavior of indexJ. For any index
-- i and join-list jl, it should be the case that

-- (indexJ i jl) == (jlToList jl !!? i)

-- That is, calling indexJ on a join-list is the same as first converting
-- the join-list to a list and then indexing into the list. The point,
-- of course, is that indexJ can be more efficient (O(log n) versus
-- O(n), assuming a balanced join-list), because it gets to use the size
-- annotations to throw away whole parts of the tree at once, whereas
-- the list indexing operation has to walk over every element.



-- helper function to extract int size from a type of kind Sized; we use the instance
-- size to get the value (Size n), and then apply the getSize function which unwraps
-- the integer value; see Sized module for details
metaDataAsInt :: (Sized b, Monoid b) => b -> Int
metaDataAsInt = getSize . size


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
-- Exceptional cases:
-- negative index
indexJ n _ | n < 0 = Nothing
-- index out of bounds
indexJ n l | n > (metaDataAsInt . tag) l - 1 = Nothing
-- Base and Inductive cases for valid inputs:
-- base case(s)
indexJ n Empty = Nothing
indexJ _ (Single _ a) = Just a
-- inductive case
indexJ n (Append m l1 l2) =
  let m1 = (metaDataAsInt . tag) l1 in
    case compare n m1 of
      LT ->
        indexJ n l1
      _ ->
        indexJ (n - m1) l2

-- test indexJ with jl1, jl2, jl3, jl4,

{------------------------- QN 2.2 -------------------------}


-- Implement the function

-- dropJ :: (Sized b, Monoid b) =>  Int -> JoinList b a -> JoinList b a

-- The dropJ function drops the first n elements from a JoinList.
-- This is analogous to the standard drop function on lists. Formally,
-- dropJ should behave in such a way that
-- jlToList (dropJ n jl) == drop n (jlToList jl).


dropJ :: (Sized b, Monoid b) =>  Int -> JoinList b a -> JoinList b a
-- Exceptional cases:
-- if n <= 0, return the entire list
dropJ n l | n <= 0 = l
-- if n > lenght of list, return the empty list
dropJ n l | (metaDataAsInt . tag) l < n = Empty
-- Base and inductive cases:
-- base case(s)
dropJ _ Empty = Empty
dropJ _ (Single _ a) = Empty
-- inductive case
dropJ n (Append m l1 l2) =
  let m1 = (metaDataAsInt . tag) l1 in
    case compare n m1 of
      GT -> dropJ (n - m1) l2
      EQ -> l2
      LT -> (dropJ n l1) +++ l2

{------------------------- QN 2.3 -------------------------}

-- Finally, implement the function

-- takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a

-- The takeJ function returns the first n elements of a JoinList,
-- dropping all other elements. Again, this function works similarly
-- to the standard library take function; that is, it should be the case
-- that
-- jlToList (takeJ n jl) == take n (jlToList jl).

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
-- Exceptional cases:
-- if n <= 0, return the empty list
takeJ n l | n <= 0 = Empty
-- if n > lenght of list, return the entire list
takeJ n l | (metaDataAsInt . tag) l < n = l
-- Base and inductive cases:
-- base case(s)
takeJ _ Empty = Empty
takeJ _ (l @ (Single _ a)) = l
-- inductive case
takeJ n (Append m l1 l2) =
  let m1 = (metaDataAsInt . tag) l1 in
    case compare n m1 of
      GT -> l1 +++ takeJ (n - m1) l2
      EQ -> l1
      LT -> (takeJ n l1)


{------------------------- For Q3, see module Scrabble -------------------------}



-- To test that you have everything working, add the line import Scrabble
-- to the import section of your JoinList module, and write the following
-- function to test out JoinLists annotated with scores:

-- scoreLine :: String -> JoinList Score String

-- Example:
-- JoinList> scoreLine "yay " +++ scoreLine "haskell!"
-- Append (Score 23)
-- (Single (Score 9) "yay ")
-- (Single (Score 14) "haskell!")

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

foo = (scoreLine "yay ") +++ (scoreLine "haskell!")

{------------------------- QN 4 -------------------------}

--  Finally, combine these two kinds of annotations. A pair
-- of monoids is itself a monoid:

-- instance (Monoid a, Monoid b) => Monoid (a,b) where
-- mempty = (mempty, mempty)
-- mappend (a1,b1) (a2,b2) = (mappend a1 a2, mappend b1 b2)

-- This instance is defined in Data.Monoid. This means that join-lists
-- can track more than one type of annotation at once, in parallel, simply
-- by using a pair type.

-- Since we want to track both the size and score of a buffer, you
-- should provide a Buffer instance for the type

-- JoinList (Score, Size) String

instance Buffer (JoinList (Score, Size) String) where

  -- Convert a buffer to a String; we do so by flattening the JoinList
  -- and then invoking mconcat on it (since lists are monoids)
  -- toString :: b -> String
  toString =
    mconcat . (joinListFold [] (\_ str -> [str ++ "\n"]) (\_ ls rs -> ls <> rs))

  -- Create a buffer from a String; in order to create a balanced tree, we
  -- recursively divide the input list of lines into half until we are left with
  -- a singleton list or an empty list, which correspond respectively to the
  -- Empty and Single constructors
  -- fromString :: String -> b
  fromString = linesToJoinList . lines
    where
      linesToJoinList [] =
        Empty
      linesToJoinList [s] =
        Single (scoreString s, Size 1) s
      linesToJoinList ls =
        let halfLengthStrs = (length ls) `div` 2 in
          let jl1 = linesToJoinList (take halfLengthStrs ls) in
            let jl2 = linesToJoinList (drop halfLengthStrs ls) in
              Append (tag jl1 <> tag jl2) (jl1) (jl2)

  -- Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices; this corresponds directly to our indexJ function
  -- line :: Int -> b -> Maybe String
  line n b = indexJ n b

  --  @replaceLine n ln buf@ returns a modified version of @buf@,
  --  with the @n@th line replaced by @ln@.  If the index is
  --  out-of-bounds, the buffer should be returned unmodified.
  --  replaceLine :: Int -> String -> b -> b
  --  we solve this using guards to factor out exceptional cases, and then making
  --  use of our takeJ and dropJ functions

  replaceLine n str b
    | n < 0 = b
    | let (_, Size s) = (tag b) in
        n > s - 1 = b
    | otherwise =
      (takeJ n b) +++ (Single ((scoreString str), Size 1) str) +++ (dropJ (n + 1) b)


  -- -- | Compute the number of lines in the buffer.
  -- numLines :: b -> Int
  numLines b =
    let (_, Size s) = (tag b)
    in s


  -- -- | Compute the value of the buffer, i.e. the amount someone would
  -- --   be paid for publishing the contents of the buffer.
  -- value :: b -> Int
  value b =
    let (Score s, _) = (tag b)
    in s

-- strings to test the buffer instance
testStr1 = "hello \n i love you \n won't you tell me \n your name \n hello \n i love you \n let me jump in your game"
testStr2 = "mary had \n a little lamb \n a little lamb \n a little lamb \n mary had a \n little lamb \n her fleece was white as snow"
testStr3 = ""
testStr4 = "Haskiller"


stringToBuffer :: String -> JoinList (Score, Size) String
stringToBuffer = fromString

bufferToString :: JoinList (Score, Size) String -> String
bufferToString = toString

getNthLine :: Int -> JoinList (Score, Size) String -> Maybe String
getNthLine = line

replaceNthLine ::
  Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
replaceNthLine = replaceLine

testNumLines :: JoinList (Score, Size) String -> Int
testNumLines = numLines

testValue :: JoinList (Score, Size) String -> Int
testValue = value


-- Finally, make a main function to run the editor interface using
-- your join-list backend in place of the slow String backend (see
-- StringBufEditor.hs for an example of how to do this). You should
-- create an initial buffer of type JoinList (Score, Size) String and
-- pass it as an argument to runEditor editor. Verify that the editor
-- demonstration described in the section  Editors and Buffers  does
-- not exhibit delays when showing the prompt

-- To run the buffer, see instructions at the top of the file












