module Main where

import           Editor
import           JoinList

main =
  runEditor editor
  (Single (Score 115) "To load a different file, type the character L followed by the name of the file.")

  -- unlines
  --        [ "This buffer is for notes you don't want to save, and for"
  --        , "evaluation of steam valve coefficients."
  --        , "To load a different file, type the character L followed"
  --        , "by the name of the file."
  --        ]
