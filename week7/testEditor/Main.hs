module Main where

import           Editor
import           JoinList
import           Scrabble
import           Sized

defaultStr :: JoinList (Score, Size) String
defaultStr =
  (Single (Score 115, Size 1) "To load a different file, type the character L followed by the name of the file.")
  
main =
  runEditor editor defaultStr
  

  -- unlines
  --        [ "This buffer is for notes you don't want to save, and for"
  --        , "evaluation of steam valve coefficients."
  --        , "To load a different file, type the character L followed"
  --        , "by the name of the file."
  --        ]
