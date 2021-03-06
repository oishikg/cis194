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


parseMessageHelper ("E" : nStr1 : nStr2 : ws) = 
  LogMessage (Error (read nStr1)) (read nStr2) (unwords ws)
parseMessageHelper ("W" : nStr : ws) =
  LogMessage Warning (read nStr) (unwords ws)
parseMessageHelper ("I" : nStr : ws) = 
  LogMessage Info (read nStr) (unwords ws)


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


parse = (map parseMessage) . lines



{- Unfortunately, due to the error messages being generated by multiple
servers in multiple locations around the globe, a lightning storm, a
failed disk, and a bored yet incompetent programmer, the log messages
are horribly out of order. Until we do some organizing, there
will be no way to make sense of what went wrong! We’ve designed a
data structure that should help—a binary search tree of LogMessages:

data MessageTree = Leaf
| Node MessageTree LogMessage MessageTree

Note that MessageTree is a recursive data type: the Node constructor
itself takes two children as arguments, representing the left and
right subtrees, as well as a LogMessage. Here, Leaf represents the
empty tree.

A MessageTree should be sorted by timestamp: that is, the timestamp
of a LogMessage in any Node should be greater than all timestamps
of any LogMessage in the left subtree, and less than all timestamps
of any LogMessage in the right child.

Unknown messages should not be stored in a MessageTree since
they lack a timestamp. -}



{- Exercise 2 Define a function

insert :: LogMessage -> MessageTree -> MessageTree

which inserts a new LogMessage into an existing MessageTree, producing
a new MessageTree. insert may assume that it is given a
sorted MessageTree, and must produce a new sorted MessageTree
containing the new LogMessage in addition to the contents of the
original MessageTree.

However, note that if insert is given a LogMessage which is
Unknown, it should return the MessageTree unchanged.
-} 
testInput :: [LogMessage]
testInput = [LogMessage Info 5053 "pci_id: con ing!",LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled",LogMessage Info 4076 "verse.'",LogMessage Info 4764 "He trusts to you to set them free,",LogMessage Info 858 "your pocket?' he went on, turning to Alice.",LogMessage Info 898 "would be offended again.",LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)",LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And",LogMessage Info 3899 "hastily.",LogMessage Info 2194 "little creature, and held out its arms and legs in all directions, 'just",LogMessage Info 1447 "she was terribly frightened all the time at the thought that it might be",LogMessage Info 1147 "began ordering people about like that!'",LogMessage Info 3466 "pci_hcd beed VRAM=2)",LogMessage Info 3974 "#55500:00000 (nux Us nel chablesen ster C)",LogMessage Info 3724 "Laughing and Grief, they used to say.'",LogMessage Info 1283 "'Now tell me, Pat, what's that in the window?'",LogMessage Info 4469 "'If that's all you know about it, you may stand down,' continued the",LogMessage Info 1641 "'I feared it might injure the brain;",LogMessage Info 1744 "aloud; and in another moment it was out of sight.",LogMessage (Error 47) 1034 "'What a pity it wouldn't stay!' sighed the Lory, as soon as it was quite",LogMessage Info 3284 "uhcd 00:03.3: LNVS) @ 0000000:0174715:00000000000] BOOTMEM",LogMessage Info 4018 "VGA mem Dynabled, nor memodiregis nosaved)",LogMessage Info 3304 "DMA32: 5",LogMessage Info 2920 "all of them bowed low.",LogMessage Warning 1654 "'I kept all my limbs very supple",LogMessage Info 2803 "'They were learning to draw,' the Dormouse went on, yawning and rubbing",LogMessage Info 2788 "from?'",LogMessage Info 2378 "vanishing so suddenly: you make one quite giddy.'",LogMessage Info 2669 "pci_hci 000: 000:1c.1: RCONF(NETDEV_UP): 6232 has numalithis pregista_sone bus 00000-0x000009ff]",LogMessage Info 861 "#66WW-1a.2:e680fed13 00:1a00: adevicesetlindow [0xa0] (idged",LogMessage Info 4751 "pci 0001: [mem 000.58 (to 6-0xf43devialligurcpice wing ling IOM by 32 kipts ask: fa0000:1c.1: T4062",LogMessage Info 4391 "'Give your evidence,' the King repeated angrily, 'or I'll have you",LogMessage Info 1883 "cpi 0xf42 by EHCI ting CRT]",LogMessage Info 4436 "raw0::throl: atardb0fead: brit 10:24: Slock",LogMessage Info 2382 "Seto 00: Regispor suppow) st sp stus cold a00009dc0000003.2:e6:02. TCONF(NETDEV_UP): e3/0xa000000fed",LogMessage Info 2751 "Allencortenablerveres irq 9 pi 0000000000:1d.7: se fory 64",LogMessage Info 2965 "6 fortc009d908:c000000000",LogMessage Info 1108 "had vanished completely.",LogMessage Info 1862 "'Well! WHAT are you?' said the Pigeon. 'I can see you're trying to"]

insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) bt = bt
insert m Leaf = Node Leaf m Leaf
insert (m @ (LogMessage _ t _)) (Node bt1 (mc @ (LogMessage _ tc _)) bt2)
  | t > tc = Node bt1 mc (insert m bt2)
  | otherwise = Node (insert m bt1) mc bt2

{-
Exercise 3 Once we can insert a single LogMessage into a MessageTree,
we can build a complete MessageTree from a list of messages. Specifi-
cally, define a function

build :: [LogMessage] -> MessageTree

which builds up a MessageTree containing the messages in the list,
by successively inserting the messages into a MessageTree (beginning
with a Leaf).
-}

{- Revision: foldr is a polymorphic higher-order function of type
(a -> b -> b) -> b -> [a] -> b -} 

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf 


{- Exercise 4:
Finally, define the function

inOrder :: MessageTree -> [LogMessage]

which takes a sorted MessageTree and produces a list of all the
LogMessages it contains, sorted by timestamp from smallest to biggest.
(This is known as an in-order traversal of the MessageTree.)
With these functions, we can now remove Unknown messages and
sort the well-formed messages using an expression such as:
inOrder (build tree)
[Note: there are much better ways to sort a list; this is just an exercise
to get you working with recursive data structures!] -}

inOrderHelper :: MessageTree -> [LogMessage] -> [LogMessage]

inOrderHelper Leaf acc = acc
inOrderHelper (Node mt1 m mt2) acc =
  inOrderHelper mt1 (m : (inOrderHelper mt2 acc))


-- non-accumulator based implementation 
inOrderHelper' Leaf = []
inOrderHelper' (Node mt1 m mt2) =
  (inOrderHelper' mt1) ++ (m : (inOrderHelper' mt2))



inOrder :: MessageTree -> [LogMessage]

inOrder mt = inOrderHelper mt []


{- Exercise 5:

Now that we can sort the log messages, the only thing
left to do is extract the relevant information. We have decided that
“relevant” means “errors with a severity of at least 50”.
Write a function

whatWentWrong :: [LogMessage] -> [String]

which takes an unsorted list of LogMessages, and returns a list of the
messages corresponding to any errors with a severity of 50 or greater,
sorted by timestamp. (Of course, you can use your functions from the
previous exercises to do the sorting.)

For example, suppose our log file looked like this:

I 6 Completed armadillo processing
I 1 Nothing to report
E 99 10 Flange failed!
I 4 Everything normal
I 11 Initiating self-destruct sequence
E 70 3 Way too many pickles
E 65 8 Bad pickle-flange interaction detected
W 5 Flange is due for a check-up
I 7 Out for lunch, back in two time steps
E 20 2 Too many pickles
I 9 Back from lunch

This file is provided as sample.log. There are four errors, three of
which have a severity of greater than 50. The output of whatWentWrong
on sample.log ought to be

[ "Way too many pickles"
, "Bad pickle-flange interaction detected"
, "Flange failed!"
]

You can test your whatWentWrong function with testWhatWentWrong,
which is also provided by the Log module. You should provide
testWhatWentWrong with your parse function, your whatWentWrong
function, and the name of the log file to parse.
-} 

whatWentWrong :: [LogMessage] -> [String]

whatWentWrong = (map (\(LogMessage _ _ m) -> m)) . 
                (filter (\x ->
                           case x of
                             LogMessage (Error x) _ _ -> x > 50
                             _ -> False
                        )) . inOrder . build










