module Main where

import Party
import Employee

-- main function is as minimal as possible
main :: IO ()
main = do
  input <- readFile filePath
  putStr . stringToGuestListString $ input
  

filePath :: FilePath
filePath = "./company.txt"

-- readFromCompanyTextFile :: IO String
-- readFromCompanyTextFile = 

stringToGuestListString :: String -> String
stringToGuestListString =
  guestListToString . maxFun . read

guestListToString :: GuestList -> String
guestListToString (GL es f) = 
  foldl
  (\acc e -> acc ++ (empName e ++ "\n"))
  ("Total fun : " ++ (show f) ++ "\n")
  es


