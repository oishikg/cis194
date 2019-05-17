module MyLib where

{- Expressions:

On day one of your new job as a software engineer, you’ve been
asked to program the brains of the company’s new blockbuster product:
a calculator. But this isn’t just any calculator! Extensive focus
group analysis has revealed that what people really want out of their
calculator is something that can add and multiply integers. Anything
more just clutters the interface.

Your boss has already started by modeling the domain with the
following data type of arithmetic expressions:

data ExprT = Lit Integer
| Add ExprT ExprT
| Mul ExprT ExprT
deriving (Show, Eq)

This type is capable of representing expressions involving integer
constants, addition, and multiplication. For example, the expression
(2 + 3) × 4 would be represented by the value

Mul (Add (Lit 2) (Lit 3)) (Lit 4).

-}
import Parser
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

e1, e2, e3, e4, e5 :: ExprT
e1 = Mul (Add (Lit 2) (Lit 3)) (Lit 4) -- (2 + 3) x 4 = 20
e2 = Mul (Mul (Add (Lit 1) (Lit 3)) (Mul (Lit 9) (Lit 8)))
     (Add (Mul (Lit 2) (Lit 4)) (Add (Lit 3) (Lit 4))) -- 4320
e3 = Add (Mul (Lit 3) (Lit 4)) (Mul (Lit 4) (Lit 5)) -- 3 x 4  +  4 x 5 = 32
e4 = Add (Add (Lit 5) (Lit 6)) (Mul (Add (Lit 2) (Lit 3)) (Lit 8) )
-- (5 + 6) + ((2 + 3) x 8) = 51
e5 = Mul (Lit 0) (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) -- 0 

{- Exercise 1:

Write Version 1 of the calculator: an evaluator for ExprT, with the
signature
nn
eval :: ExprT -> Integer

For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20. -}

{- quick and dirty unit tests -}
testEval :: (ExprT -> Integer) -> Bool
testEval f =
  (f e1 == 20)
  &&
  (f e2 == 4320)
  &&
  (f e3 == 32)
  &&
  (f e4 == 51)
  &&
  (f e5 == 0) 
  
{- function to evaluate the expression} -} 
eval :: ExprT -> Integer
eval exp =
  case exp of
    Lit n -> n
    Add e1 e2 -> (eval e1) + (eval e2)
    Mul e1 e2 -> (eval e1) * (eval e2) 

-- testEval eval

{- Exercise 2 :

The UI department has internalized the focus group data and is
ready to synergize with you. They have developed the front-facing
user-interface: a parser that handles the textual representation of the
selected language. They have sent you the module Parser.hs, which
exports parseExp, a parser for arithmetic expressions. If you pass
the constructors of ExprT to it as arguments, it will convert Strings
representing arithmetic expressions into values of type ExprT. For
example:

*Calc> parseExp Lit Add Mul "(2+3)*4"
Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

*Calc> parseExp Lit Add Mul "2+3*4"
Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))

*Calc> parseExp Lit Add Mul "2+3*"
Nothing

Leverage the assets of the UI team to implement the value-added
function
evalStr :: String -> Maybe Integer
which evaluates arithmetic expressions given as a String, producing
Nothing for inputs which are not well-formed expressions, and
Just n for well-formed inputs that evaluate to n. -}

testEvalStr :: (String -> Maybe Integer) -> Bool
testEvalStr f =
  (f "(2+3)*4" == Just 20)
  &&
  (f "(4*5)*6*7" == Just 840)
  &&
  (f "(2+3)*(5+8)" == Just 65)
  &&
  (f "4*((3*2)+(2*2))" == Just 40)
  &&
  (f "3+4**3+2" == Nothing)
  &&
  (f "39+*2+22+**3" == Nothing) 
  
evalStr :: String -> Maybe Integer
evalStr = (\x ->
             case x of
               Nothing -> Nothing
               Just e -> Just (eval e)) . (parseExp Lit Add Mul)
-- testEvalStr evalStr          


{- Exercise 3 :

Good news! Early customer feedback indicates that people really
do love the interface! Unfortunately, there seems to be some disagreement
over exactly how the calculator should go about its calculating
business. The problem the software department (i.e. you) has is that
while ExprT is nice, it is also rather inflexible, which makes catering
to diverse demographics a bit clumsy. You decide to abstract away
the properties of ExprT with a type class.

Create a type class called Expr with three methods called lit, add,
and mul which parallel the constructors of ExprT. Make an instance of
Expr for the ExprT type, in such a way that

mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
== Mul (Add (Lit 2) (Lit 3)) (Lit 4)

Think carefully about what types lit, add, and mul should have. It
may be helpful to consider the types of the ExprT constructors, which
you can find out by typing (for example)

*Calc> :t Lit

at the ghci prompt.

Remark. Take a look at the type of the foregoing example expression:

*Calc> :t mul (add (lit 2) (lit 3)) (lit 4)
Expr a => a

What does this mean? The expression mul (add (lit 2) (lit 3)) (lit 4)
has any type which is an instance of the Expr type class. So writing it
by itself is ambiguous: GHC doesn’t know what concrete type you
want to use, so it doesn’t know which implementations of mul, add,
and lit to pick.



One way to resolve the ambiguity is by giving an explicit type
signature, as in the above example. Another way is by using such an
expression as part of some larger expression so that the context in
which it is used determines the type. For example, we may write a
function reify as follows:

reify :: ExprT -> ExprT
reify = id

To the untrained eye it may look like reify does no actual work!
But its real purpose is to constrain the type of its argument to ExprT.
Now we can write things like
reify $ mul (add (lit 2) (lit 3)) (lit 4)
at the ghci prompt. -}

{- type class Expr to abstract arithmetic expressions -}
class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

{- ExprT as an instance of the type class -} 
instance Expr ExprT where
  lit e = Lit e
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2


{- Exercise 4
The marketing department has gotten wind of just how flexible
the calculator project is and has promised custom calculators to some
big clients. As you noticed after the initial roll-out, everyone loves the
interface, but everyone seems to have their own opinion on what the
semantics should be. Remember when we wrote ExprT and thought
that addition and multiplication of integers was pretty cut and dried?
Well, it turns out that some big clients want customized calculators
with behaviors that they have decided are right for them.
The point of our Expr type class is that we can now write down
arithmetic expressions once and have them interpreted in various
ways just by using them at various types.
cis 194: homework 5 4
Make instances of Expr for each of the following types:
• Integer — works like the original calculator
• Bool — every literal value less than or equal to 0 is interpreted
as False, and all positive Integers
are interpreted as True; “addition” is logical or,
“multiplication” is logical and
• MinMax — “addition” is taken to be the max function, while
“multiplication” is the min function
• Mod7 — all values should be in the ranage 0 . . . 6, and
all arithmetic is done modulo 7; for example,
5 + 3 = 1.
The last two variants work with Integers internally, but in order
to provide different instances, we wrap those Integers in newtype
wrappers. These are used just like the data constructors we’ve seen
before.
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)
Once done, the following code should demonstrate our family of
calculators:
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
Try printing out each of those tests in ghci to see if things are
working. It’s great how easy it is for us to swap in new semantics for
the same syntactic expression! -}












