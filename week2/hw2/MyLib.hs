-- CIS 194 Homework 2
{- Introducting the problem : Log file parsing 

We’re really not sure what happened, but we did manage to recover
the log file error.log. It seems to consist of a different log message
on each line. Each line begins with a character indicating the type of
log message it represents:

• ’I’ for informational messages,
• ’W’ for warnings, and
• ’E’ for errors.

The error message lines then have an integer indicating the severity
of the error, with 1 being the sort of error you might get around to
caring about sometime next summer, and 100 being epic, catastrophic
failure. All the types of log messages then have an integer time stamp
followed by textual content that runs to the end of the line. Here is a
snippet of the log file including an informational message followed
by a level 2 error message:

cis 194: homework 2 2

I 147 mice in the air, I’m afraid, but you might catch a bat, and
E 2 148 #56k istereadeat lo d200ff] BOOTMEM

It’s all quite confusing; clearly we need a program to sort through
this mess. We’ve come up with some data types to capture the structure
of this log file format:

data MessageType = Info
| Warning
| Error Int
deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
| Unknown String
deriving (Show, Eq)

Note that LogMessage has two constructors: one to represent normallyformatted
log messages, and one to represent anything else that does
not fit the proper format.
-} 

{-- code by course facilitators --}
module MyLib where

import Control.Applicative
import System.IO 

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file

{- Exercise 1 The first step is figuring out how to parse an individual
message. Define a function

parseMessage :: String -> LogMessage

which parses an individual line from the log file. For example,

parseMessage "E 2 562 help help"
== LogMessage (Error 2) 562 "help help"


parseMessage "I 29 la la la"
== LogMessage Info 29 "la la la"

parseMessage "This is not in the right format"
== Unknown "This is not in the right format" -}
parseMessageHelper :: [String] -> LogMessage

parseMessageHelper ("E" : ws) = 
  LogMessage (Error (read (ws !! 0))) (read (ws !! 1)) (unwords (drop 2 ws))
parseMessageHelper ("W" : ws) =
  LogMessage Warning (read (ws !! 0)) (unwords (drop 1 ws))
parseMessageHelper ("I" : ws) = 
  LogMessage Info (read (ws !! 0)) (unwords (drop 1 ws))
parseMessageHelper ws = Unknown (unwords ws) 
  
{- Read string, use words to get array of words, then match first word to see if we
are dealing with E/W/I or unknown -}
parseMessage :: String -> LogMessage

parseMessage  =
  parseMessageHelper . words 



{- Once we can parse one log message, we can parse a whole log file.
Define a function

parse :: String -> [LogMessage]

which parses an entire log file at once and returns its contents as a
list of LogMessages. To test your function, use the testParse function provided in the
Log module, giving it as arguments your parse function, the number
of lines to parse, and the log file to parse from (which should also be
in the same folder as your assignment). For example, after loading
your assignment into GHCi, type something like this at the prompt:

testParse parse 10 "error.log"

Don’t reinvent the wheel! (That’s so last week.) Use Prelude functions
to make your solution as concise, high-level, and functional as
possible. For example, to convert a String like "562" into an Int, you
can use the read function. Other functions which may (or may not)
be useful to you include lines, words, unwords, take, drop, and (.).
-}

parse :: String -> [LogMessage]

parse e  = map parseMessage (lines e)









