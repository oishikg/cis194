module SimpleQueue where


class SimpleQueue a where
  newQueue :: [a]
  enqueue ::  a -> [a] -> [a] 
  dequeue ::  [a] -> [a] -- we don't ask for the dequeued element


  





