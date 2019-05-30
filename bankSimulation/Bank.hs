module Bank where

import Control.Monad.Random

import Data.Functor.Identity

import Control.Monad.Primitive

import Data.Bifunctor

import Data.Bool

import Data.List

import Models

import SimpleQueue


-- data type to capture a customer
data Customer = Customer { timeWaited :: Double , inQueue :: Bool }
  deriving (Show) 

newCustomer :: Customer
newCustomer = Customer { timeWaited = 0 , inQueue = True }

-- some simple queue functions for the customer queue
instance SimpleQueue Customer where
  newQueue = []

  enqueue = (:)

  dequeue [] = []
  dequeue (c : []) = [ c {inQueue = False} ]
  dequeue (c : (cs @ (Customer {inQueue = False} : cs'))) =
    c {inQueue = False} : cs
  dequeue (c : cs) = c : (dequeue cs) 
  -- dequeue cs =
  --  let (servedCustomers , customersToBeServed) =
  --        (filter (not . inQueue) cs , filter inQueue cs) 
  --  in (go customersToBeServed) ++ servedCustomers
  --  where
  --    go [] = []
  --    go (lc : []) = [ lc {inQueue = False} ]
  --    go (c : cs) = c : (dequeue cs) 

  


-- data type to capture state of bank
data BankState = BankState { queue :: [Customer] ,
                             recordQueueLength :: [Int] , 
                             queueLength :: Int ,
                             timeSinceLastCustomer :: Double, 
                             timeLeftAtTeller :: Double
                           }

newBank :: BankState
newBank = BankState { queue = newQueue ,
                      recordQueueLength = [] ,
                      queueLength = 0 ,
                      timeSinceLastCustomer = 0 ,
                      timeLeftAtTeller = 0 } 

------------------------- Notes -------------------------

-- a new customer is added to the queue everytime addCustomer is called

-- the last customer is removed from the queue everytime that customer moves to the
-- teller

--  one second is added to the time waited for each customer while the current
-- customer is at the teller

-- if a customer does not come in, the timeSinceLastCustomer is updated by 1.0, but
-- the time waited by each customer is not (because this time is counted in the
-- while the current customer is processed)

-- in effect, we have broken up the one second during which our model would both
-- check for customers and our current customer would be served by the teller 


------------------------- Helper functions -------------------------

-- function to simualate waiting for customer by adding one second
waitForCustomer :: BankState -> Rand StdGen BankState
waitForCustomer b =
  return $
  b { timeSinceLastCustomer = timeSinceLastCustomer b + 1.0 }
      

-- function to add customer by increasing queue lenght and resetting time 
addCustomer :: BankState -> Rand StdGen BankState
addCustomer b = return $ b { queue = enqueue newCustomer (queue b) ,
                             recordQueueLength =
                             (queueLength b + 1) : (recordQueueLength b) ,
                             queueLength = queueLength b + 1 ,
                             timeSinceLastCustomer = 0.0 } 




-- function to wait upon customer at teller
currentCustomerAtTeller :: BankState -> Rand StdGen BankState
currentCustomerAtTeller b =
  return $ b { queue = updateCustomerWaitingTime <$> queue b, 
               timeLeftAtTeller = timeLeftAtTeller b - 1.0 }
  where
    updateCustomerWaitingTime c =
      bool c (c {timeWaited = timeWaited c + 1.0}) (inQueue c)



-- function to simulate the addition of a new customer at a teller
newCustomerAtTeller :: BankState -> DistParams -> Rand StdGen BankState
newCustomerAtTeller b dist =
  bool (return b) go (queueLength b >= 1)
  where
    go = 
      roll >>= \x ->
      return $ b { queue = dequeue (queue b) ,
                   recordQueueLength =
                     (queueLength b - 1) : (recordQueueLength b) ,
                   timeLeftAtTeller =  g x dist , -- see Models for spec of g
                   queueLength = queueLength b - 1 } 


------------------------- Simulation functions -------------------------

-- function to simulate waiting for the customer
simulateCustomer :: BankState ->  Rand StdGen BankState
simulateCustomer b = 
  roll >>= \p ->
  bool (waitForCustomer b) (addCustomer b) (p <= (f $ timeSinceLastCustomer b))
-- see Models for spec of f

-- function to simulate customer being served by the teller
simulateTeller :: BankState -> DistParams -> Rand StdGen BankState
simulateTeller b dist =
  bool
  (currentCustomerAtTeller b)
  (newCustomerAtTeller b dist)
  (timeLeftAtTeller b <= 1.0)

-- function to simulate one second at the bank 
simulateOneSecond :: Rand StdGen BankState -> DistParams -> Rand StdGen BankState
simulateOneSecond bm dist =
  bm >>= \b ->
  -- first check for customers coming in
  simulateCustomer b >>= \b' ->
  -- then check teller status 
  simulateTeller b' dist






