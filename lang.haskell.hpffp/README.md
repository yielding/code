# HASKELL PROGRAMMING FROM FIRST PRINCIPLES

## CHAPTER1. ALL YOU NEED IS LAMBDA

### 1.1 All You Need is Lambda

#### lambda calculus:

computation model, 1930s, Alonzo Church, formalizing a method, Turing machines

### 1.2 What is functional programming?

#### functional programming:

expression combination, first class function, purity, referential transparency, abstraction and composability

### 1.3 What is a function?

relation -> input & output,

input set = domain, output = codomain, output <- input = range, 

### 1.4 The structure of lambda terms

lambda calculus -> three lambda terms: expressions, variables,  abstractions

expression:  variable name, abstraction, combination of those things. 

abstraction: anonymous function 𝜆𝑥.𝑥 = function = head (𝜆 lambda + parameter name) + body (expression) <- argument (input value) 

#### Alpha equivalence
𝜆𝑥.𝑥 = 𝜆𝑑.𝑑 = 𝜆𝑧.𝑧

### 1.5 Beta reduction
 
(𝜆𝑥.𝑥)(𝜆𝑦.𝑦) => [𝑥 ∶= (𝜆𝑦.𝑦)] => 𝜆𝑦.𝑦


#### Free variables

𝜆𝑥.𝑥𝑦 <= y

### 1.6 Multiple arguments

currying: 1920s Moses Schönfinke -> Haskell Curry 

𝜆𝑥𝑦.𝑥𝑦 -> 𝜆𝑥.(𝜆𝑦.𝑥𝑦)

### 1.7 Evaluation is simplification

beta normal form: cannot reduce further

### 1.8 Combinators

combinator: a lambda term with no free variables:
1. 𝜆𝑥.𝑥 2. 𝜆𝑥𝑦.𝑥 3. 𝜆𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧)

not combinator, free variable: 1. 𝜆𝑦.𝑥 2. 𝜆𝑥.𝑥𝑧 

### 1.9 Divergence 
Divergence: reduction process never terminates:

#### omega:

(𝜆𝑥.𝑥𝑥)(𝜆𝑥.𝑥𝑥) => ([𝑥 ∶= (𝜆𝑥.𝑥𝑥)]𝑥𝑥) => (𝜆𝑥.𝑥𝑥)(𝜆𝑥.𝑥𝑥)

### 1.10 Summary

* FP: expression composition
* function: head, body <= apply, reduce, evaluate
* function declaration: bound variable
* one argument => function => one result
* function: same input => same output

Haskell: typed lambda calculus

### 1.11 Exercises

#### Combinators

1. 𝜆𝑥.𝑥𝑥𝑥
3. 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥) 
4. 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥𝑦)

#### Normal form or diverge

1. 𝜆𝑥.𝑥𝑥𝑥 => normal
2. (𝜆𝑧.𝑧𝑧)(𝜆𝑦.𝑦𝑦) => diverge
3. (𝜆𝑥.𝑥𝑥𝑥)𝑧 => nomal


#### Beta reduce

1. (𝜆𝑎𝑏𝑐.𝑐𝑏𝑎)𝑧𝑧(𝜆𝑤𝑣.𝑤) => z
2. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑎.𝑎)𝑏 => bb
3. (𝜆𝑦.𝑦)(𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞) => qq
4. (𝜆𝑧.𝑧)(𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦) => yy
5. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑦.𝑦)𝑦 => yy
6. (𝜆𝑎.𝑎𝑎)(𝜆𝑏.𝑏𝑎)𝑐 => aac
7. (𝜆𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑧)(𝜆𝑥.𝑎) => 𝜆𝑧'.𝑧𝑎

### 1.13 Definitions

* normal order: left associative

### 1.14 Follow-up resources 

1. Raul Rojas. [A Tutorial Introduction to the Lambda Calculus](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)

2. Henk Barendregt; Erik Barendsen. [Introduction to Lambda Calculus](http://www.cse.chalmers.se/research/group/logic/TypesSS05/Extra/geuvers.pdf)
    
3. Jean-Yves Girard; P. Taylor; Yves Lafon. [Proofs and Types](http://www.paultaylor.eu/stable/prot.pdf)

## CHAPTER2. HELLO,HASKELL!

### 2.1 Hello, Haskell

install tools

### 2.2 Interacting with Haskell code 
#### Using the REPL

command:

* :quit / :q
* :info / : i
* :load / :l
* :reload / :r
* :module / :m  #unload module
* :type /:t
* :browse #browse module

### 2.3 Understanding expressions

redexes: reducible expressions

normalizing = executing = evaluating = reducing

### 2.4 Functions 
#### Defining functions 
triple x = x * 3

#### Evaluating functions

𝑓′ : "eff-prime"

#### Intermission: Exercises

1. half x = x / 2 => add 'let' to run in REPL
2. f r = 3.14 * r * r

### 2.5 Infix operators

default: prefix syntax

#### Associativity and precedence 
```
:info (*) 
> infixl 7 * 
```
infixl: infix operator, left associative
infixr: infix operator, right associative
7: precedence on a scale of 0-9, indicated by higher number

#### Intermission: Exercises

Below are some pairs of functions that are alike except for parenthe- sization. Read them carefully and decide if the parentheses change the results of the function. Check your work in GHCi.
1. a) 8 + 7 * 9 b) (8 + 7) * 9
2. a) perimeter x y = (x * 2) + (y * 2) b) perimeter x y = x * 2 + y * 2
3. a) f x = x / 2 + 9
b) f x = x / (2 + 9)

### 2.6 Declaring values

```haskell
module Learn where
x = 10 * 5 + y 
myResult = x * 5
y = 10
```

#### Troubleshooting

spacing:

```haskell
let 
  x = 3
  y = 4 

-- or
  
let x = 3 
    y = 4
```

```haskell
foo x =
    let y = x * 2
        z = x ^ 2 
    in 2 * y * z
```

#### Intermission: Exercises

```haskell
let area x = 3. 14 * (x * x) -- 3.14
let double x = b * 2 -- b is free
x = 7
 y = 10
f = x + y -- spacing
```

### 2.7 Arithmetic functions in Haskell

operator: + - * / div mod quot rem

#### Using ‘mod‘ 
mod & rem:
 ```haskell
mod (-12) 7 -- 5
rem (-12) 7 -- -2
```

### 2.8 Negative numbers

syntactic sugar: unary - => negate

```haskell
2000 + (-1234)
2000 + (negate 1234)
```

### 2.9 Parenthesizing infix functions 
```haskell
1 + 1
(+) 1 1
(+ 1) 1 -- sectioning
```

### 2.10 Laws for quotients and remainders

(quot x y)*y + (rem x y) == x

(div x y)*y + (mod x y) == x

### 2.11 Evaluation

### 2.12 Let and where

```haskell
-- FunctionWithWhere.hs
module FunctionWithWhere where 
printInc n = print plusTwo
  where plusTwo = n + 2
  
printInc 1 -- 3
```

#### Desugaring let to lambda 
```haskell
printInc2 n = let plusTwo = n + 2 in print plusTwo

-- turns into

printInc2 n = (\plusTwo -> print plusTwo)(n + 2)
```

#### Intermission: Exercises

```haskell
let x = 5 in x -- 5

let x = 5 in x * x -- 25

let x = 5; y = 6 in x * y -- 30

let x = 3; y = 1000 in x + 3 -- 6
```

#### The lambdas beneath let expressions

λx.x => \x -> x

```haskell
let a = b in c
-- equivalent to
(\a -> c) b

let x = 10 in x + 9001
-- equivalent to
(\x -> x + 9001) 10
```

```haskell
c where a = b
-- equivalent to
(\a -> c) b

x + 9001 where x = 10
-- equivalent to
(\x -> x + 9001) 10
```

#### More exercises!

```haskell
let x = 3; y = 1000 in x * 3 + y
v = x * 3 + y where x = 3; y = 100

let y = 10; x = 10 * 5 + y in x * 5
v = x * 5 where y = 10; x = 10 * 5 + y

let x = 7; y = negate x; z = y * 10 in z / x + y
v = z / x + y where x = 7; y = negate x; z = y * 10
```

### 2.13 Chapter Exercises

#### Parenthesization

```shell
:info ($)
> ($) :: (a -> b) -> a -> b
> infixr 0 $
```

```haskell
(2^) $ 2 + 2 -- 16

(2^) (2 + 2) -- 16

(2^) 2 + 2 -- 6

(2^) $ (+2) $ 3 * 2 -- 256
```

#### Equivalent expressions

```haskell
1 + 1 -- 2

10 ^ 2 -- 10 + 9 * 10

400 - 37 -- not (-) 37 400

100 `div` 3 -- 100 / 3

2 * 5 + 18 -- not 2 * (5 + 18)
``` 

#### More fun with functions

### 2.14 Definitions

1. argument & parameter: 

```haskell
f x = x + 2
f 1
```

parameter 𝑥, argument 1

2. expression: combination of constants, variables, and functions.
3. redex: reducible expression.
4. value: expression that cannot be reduced or evaluated.
5. function: a list -> inputs => outputs
6. Infix notation
7. Operators: infix functions
8. Syntactic sugar

### 2.15 Follow-up resources

1. Haskell wiki article on [Let vs. Where](https://wiki.haskell.org/Let_vs._Where)
2. [How to desugar Haskell code](http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code. html); Gabriel Gonzalez

## CHAPTER3. STRINGS

### 3.1 Printing strings

### 3.2 A first look at types

[Char] === String

```haskell
-- :type "Hello"
"Hello" :: [Char]
```
:: type signature

### 3.3 Printing simple strings

```haskell
-- print1.hs
module Print1 where main :: IO ()
main = do
  putStrLn "Count to four for me:"
  putStr   "one, two"
  putStr   ", three, and"
  putStrLn " four!"
main
```

#### String concatenation

++ 

### Global versus local definitions

### Intermission: Exercises

### 3.4 Types of concatenation functions

```haskell
-- :t concat
concat :: Foldable t => t [a] -> [a]
```

#### Intermission: Exercises

### 3.5 Concatenation and scoping

### 3.6 More list functions

```haskell
'c' : "hris"

head "Papuchon"

tail "Papuchon" -- "apuchon"

take 6 "Papuchon" -- "Papuch"

drop 1 "Papuchon" -- "apuchon"

"Papu" ++ "chon" -- "Papuchon"

"Papuchon" !! 0 -- 'p'
```

### 3.7 Chapter Exercises

#### Reading syntax
#### Building functions
### 3.8 Definitions

1. String: [Char]
2. type / datatype: value classification
3. concatenation
4. scope: visibility
5. local binding: let where
6. global: top level bindings
7. data structure

## CHAPTER4. BASICDATATYPES

### 4.1 Basic Datatypes

type: classifying, organizing, delimiting data

datatypes, type constructors, data constructors, type signatures, typeclasses

### 4.2 Anatomy of a data declaration

Data declarations: datatype

```haskell
data Bool = False | True
```

Type constructor: type name, capitalized (Bool)

Data constructor: value (False | True)

#### Intermission: Exercises

### 4.3 Numeric types

#### Integral numbers

1. Int: fixed-precision integer
2. Integer: arbitrarily large numbers

#### Fractional

1. Float: single-precision floating point number.
2. Double: double-precision floating point number.twice bits of Float.
3. Rational: ratio of two Integers
4. Scientific: almost-arbitrary precision scientific number type

typeclass Num: numeric datatype 

Typeclass: reuse functionality across type.

#### Integral numbers

Integral number: Int & Integer: no fraction: 1 2 3

#### Integer

infinite data constructor, how represent?

#### Why do we have Int?

```haskell
import GHC.Int

127 :: Int8
128 :: Int8 -- warning

--typeclass Bounded
minBound :: Int8 -- -128
maxBound :: Int8 -- 128
```

#### Fractional numbers

Float, Double, Rational, Scientific

```haskell
-- :t (/)
(/) :: Fractional a => a -> a -> a
```

type variable a implement Fractional typeclass

Num is superclass of Fractional.
function from Num can be used with Fractional
function from Fractional cannot .. Fractional

### 4.4 Comparing values

```haskell
['a', 'b'] > ['b', 'a'] -- false
[1, 2] > [2, 1] -- false
```

```haskell
data Mood = Blah | Woot deriving Show

[Blah, Woot] > [Woot, Blah] -- error
```

#### Go on and Bool me

Term-level: code <= running

type-level: type variables, type constructors, and typeclasses <= static analysis & verification 

module

#### Intermission: Exercises

#### if-then-else

```haskell
if True then "Truthin" else "Falsin"
```

```haskell
greetIfCool :: String -> IO () 
greetIfCool coolness =
  if cool coolness
    then putStrLn "eyyyyy. What's shakin'?"
  else
    putStrLn "pshhhh."
  where cool v = v == "downright frosty yo"
```

### 4.5 Tuples

multiple values within a single value

tuple's arity: set in type and immutable

pair: two-tuple

triple: three-tuple

```haskell
fst :: (x, xs) -> a
snd :: (x, xs) -> xs
```

different types in tuple:

```haskell
let myTup = (1 :: Integer, "blah")

import Data.Tuple
swap myTup -- ("blah", 1)
```

### 4.6 Lists

```haskell
let awesome = ["Papuchon", "curry", ":)"]
-- :t awesome
awesome :: [[Char]]
```

### 4.7 Chapter Exercises 
```haskell
awesome = ["Papuchon", "curry", ":)"] 
alsoAwesome = ["Quake", "The Simons"] 
allAwesome = [awesome, alsoAwesome]
```

1. type signature of length:

```haskell
-- :t length
length :: Foldable t => t a -> Int
```

2.

```haskell
length allAwesome -- 3
length (concat allAwesome) -- 5
```

3.

```haskell
6 / length [1, 2, 3] -- error

-- this works
6 / (fromIntegral (length [1, 2, 3]))

```

8. palindrome

```haskell
isPalindrome :: Eq a => [a] -> Bool 
isPalindrome x = reverse x == x
```

9.

```haskell
myAbs :: Integer -> Integer
myAbs n = if n < 0 then (-n) else n
```

10.

```haskell
f :: (a, b) -> (c, d) -> ((b, d), (a, c)) 
f (a, b) (c, d) = ((b, d), (a, c)) 
```

#### Reading syntax

#### Match the function names to their types

### 4.8 Definitions 
1. tuple: no singleton tuple, but zero tuple/unit ()
2. typeclass: 
3. data constructor: constant value(nullary) or take arguments like function, Cat Dog

```haskell
type Name = String
data Pet = Cat | Dog Name

-- :t Cat
Cat :: Pet
-- :t Dog
Dog :: Name -> Pet
```

4. type constructor: not value, Pet
5. data declaration: create new type constructor + data constructor
6. type alias: alternate name

```haskell
type Nam
```

7. arity: arguments number
8. polymorphism: parametric or constrained

```haskell
-- parametric
id :: a -> a
id x = x

-- constrained
isEqual :: Eq a => a -> a -> Bool
isEqual x y = x == y
```
### 4.9 Answers

## CHAPTER5. TYPES

### 5.1 Types

type: Bool tuple

type class: Num Eq

### 5.2 What are types?

### 5.3 Querying and Reading Types

```haskell
-- :i (->)
data (->) t1 t2

-- :i (,)
data (,) a b = (,) a b

-- fst is a value of type (a, b) -> a 
fst :: (a, b) -> a

:type length
-- before GHC 7.10 length :: [a] -> Int
Foldable t => t a -> Int
```

### 5.4 Typeclass constrained type variables

typeclass-constrained polymorphic type variable

```haskell
let fifteen = 15

-- :t fifteen
fifteen :: Num a => a
```

fifteen is constrained by typeclass Num, but dont know its concrete type. its type can be a Num instance (Float, Int, Integer...).

```haskell
-- :info Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
  	-- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
```


multiple typeclass constraints:

```haskell
(Num a, Num b) => a -> b -> b -- or
(Ord a, Num a) => a -> Ordering
```

#### Intermission: Exercises 

```haskell
not :: Bool -> Bool
length :: Foldable t => t a -> Int
concat :: Foldable t => t [a] -> [a]
head :: [a] -> a
(<) :: Ord a => a -> a -> Bool
```

### 5.5 Currying

partial application

#### Binding variables to types

#### Manual currying and Uncurry

• Uncurried functions: One function, many arguments
• Curried functions: Many functions, one argument apiece

#### Currying and uncurrying existing functions

```haskell
let curry f a b = f (a, b)

-- :t curry
curry :: ((t1, t2) -> t) -> t1 -> t2 -> t

-- :t fst
fst :: (a, b) -> a

-- :t curry fst
curry fst :: t -> b -> t

fst (1, 2) -- 1
curry fst 1 2 --1

let uncurry f (a, b) = f a b

-- :t uncurry
uncurry :: (t1 -> t2 -> t) -> (t1, t2) -> t

-- :t (+)
(+) :: Num a => a -> a -> a

(+) 1 2 -- 3
uncurry (+) (1, 2) -- 3
```

#### Intermission: Exercises

### 5.6 Polymorphism

type signatures types: concrete, constrained polymorphic, or parametrically polymorphic.

polymorphism: parametric polymorphism, constrained polymorphism.

* constrained, ad-hoc polymorphism(overload): puts typeclass constraints on the variable. capitalized name in type signature.

* Parametric polymorphism: type variables, or parameters. fully polymorphic. unconstrained by typeclass, lowercase name in type signature.

more type flexibility, less method; vice versa.

* type: a set of possible values
* type variable: a set of types, constrained by type class

* polymorphic function: type signature has variables which represent more than one type.

#### Intermission: Exercises 
1. given function type signature: a -> a. make a function other than id. 
 
```haskell
-- impossible
```

2. type signature: a -> a -> a, find two and only two functions

```haskell
f :: a -> a -> a
f a b = a

f :: a -> a -> a
f a b = b
```

3. type signature: a -> b -> b

```haskell
f a b = b
```

#### Polymorphic constants

```haskell
1 + 0.1
-- 1.1

-- :t 1 + 0.1
1 + 0.1 :: Fractional a => a

-- :t 1
1 :: Num t => t 

-- :t 0.1
0.1 :: Fractional t => t
```

type of 0.1 polymorphic

#### Working around constraints

```haskell
6 / (length [1, 2, 3]) -- error

6 / fromIntegral (length [1, 2, 3])
```

### 5.7 Type inference

Damas-Hindley-Milner type system

#### Intermission: Exercises

### 5.8 Asserting types for declarations

```haskell
-- type signature locally, uncommon
triple x = tripleIt x
  where tripleIt :: Integer -> Integer
        tripleIt y = y * 3
```

### 5.9 Chapter Exercises

#### Multiple choice

#### Determine the type

1.

```haskell
(* 9) 6 :: Num a => a

head [(0,"doge"),(1,"kitteh")] :: Num t => (t,[Char])

head [(0 :: Integer ,"doge"),(1,"kitteh")] :: (Integer,[Char])

if False then True else False :: Bool

length [1, 2, 3, 4, 5] :: Int

(length [1, 2, 3, 4]) > (length "TACOCAT") :: Bool
```

2.

```haskell
x = 5 
y = x + 5
w = y * 10 :: Num
```

3.

```haskell
x = 5 
y = x + 5
z y = y * 10 
z :: Num a => a -> a
```
4.

```haskell
x = 5 
y = x + 5 
f = 4 / y 
f :: Fractional a => a
```

5.

```haskell
x = "Julie"
y = " <3 "
z = "Haskell"
f = x ++ y ++ z
f :: [Char]
```

#### Does it compile?

#### Type variable or specific type constructor?

```
f :: Num a => a -> b -> Int -> Int
```
* constrained polymorphic: a
* fully polymorphic: b
* concrete: Int

#### Write a type signature

```haskell
functionH :: [t] -> t
functionH (x : _) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) 
                  then True 
                else False

functionS :: (t, t1) -> t1
functionS (x, y) = y
```

#### Given a type, write the function

```haskell
i :: a -> a
i = id

c :: a -> b -> a 
c a _ = a

r :: [a] -> [a] 
r a = tail a
-- ... others

co :: (b -> c) -> (a -> b) -> (a -> c) 
co = (.) -- function composition

a :: (a -> c) -> a -> a 
a f x = x -- ???

a' :: (a -> b) -> a -> b 
-- a' f x = f x
a' = ($)
```

#### Fix it

```haskell
module Arith3Broken where

main :: IO () 
main = do
  print $ 1 + 2
  putStrLn $ show 10
  print (negate -1) 
  print ((+) 0 blah) 
  where blah = negate 1
```

#### Type-Kwon-Do 
1.

```haskell
f :: Int -> String 
f = undefined

g :: String -> Char 
g = undefined

h :: Int -> Char
h = g . f
```

2.

```haskell
data A 
data B 
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q 
```

3.

```haskell 
data X 
data Y 
data Z

xz :: X -> Z 
xz = undefined

yz :: Y -> Z 
yz = undefined

xform :: (X, Y) -> (Z, Z) 
xform (a, b) = (xz a, xz b)
```

4.

```haskell
munge :: (x -> y) -> (y -> (w, z)) -> x -> w 
munge f1 f2 x = fst $ f2 $ f1 x
munge f1 f2 = fst . f2 . f1
```

### 5.10 Definitions

1. Polymorphism: parametric / ad-hoc
2. principal type: generic type which still typechecks

```haskell
a
Num a => a 
Int
-- The principal type here is the
-- parametrically polymorphic 'a'
```

3. Type inference
4. Type variable: unspecified type or set of types 
5. typeclass
6. Parametricity
7. Ad-hoc polymorphism: constrained polymorphism, apply typeclass constraints

### 5.11 Follow-up resources

1. Luis Damas; Robin Milner. [Principal type-schemes for func- tional programs](http://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf)
2. Christopher Strachey. [Fundamental Concepts in Programming Languages](http://www.cs.cmu.edu/~crary/819-f09/Strachey67.pdf)

Popular origin of the parametric/ad-hoc polymorphism distinction.

## CHAPTER6. TYPECLASSES

### 6.1 Typeclasses

* typeclass Eq, Num, Ord, Enum, Show
* type-defaulting typeclass, typeclass inheritance
* implicit function that create side effect

### 6.2 What are typeclasses?

typeclass and type are opposite

* type declaration: how type is created
* typeclass declaration: how type is consumed

typeclass - interface: ad-hoc polymorphism, generalize over a set of types to define and execute a standard set of features. eg, typeclass Eq -> type data comparation


### 6.3 Back to Bool 

```haskell
-- :info Bool

-- data declaration
data Bool = False | True 

-- typeclass that Bool implements.
instance Bounded Bool -- upper and lower bound
instance Enum Bool -- can be enumerated
instance Eq Bool -- equation
instance Ord Bool -- can be put into a sequential order
instance Read Bool -- parse string into thing. DONT USE.
instance Show Bool -- render thing into string
```

* Ord <- Eq: be compared for equality before ordering. 
* Enum <- Ord: be orderd before put into enumerated list.

### 6.4 Eq

```haskell
:info Eq
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  
-- List of some Eq instances
instance Eq a => Eq [a]
instance Eq Ordering
instance Eq Int
instance Eq Float
instance Eq Double
instance Eq Char
instance Eq Bool
instance (Eq a, Eq b) => Eq (a, b)
instance Eq ()
instance Eq a => Eq (Maybe a)
instance Eq Integer
```

```haskell
data (,) a b = (,) a b
instance (Eq a, Eq b) => Eq (a, b) 
instance (Ord a, Ord b) => Ord (a, b) 
instance (Read a, Read b) => Read (a, b)
instance (Show a, Show b) => Show (a, b)
```

### 6.5 Num

```haskell
class Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a 
  fromInteger :: Integer -> a
instance Num Integer
instance Num Int
instance Num Float
instance Num Double
```

#### Integral

```haskell
class (Real a, Enum a) => Integral a where 
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a) divMod :: a -> a -> (a, a)   
  toInteger :: a -> Integer
```

#### Fractional

```haskell
class (Num a) => Fractional a where 
  (/) :: a -> a -> a
  recip :: a -> a 
  fromRational :: Rational -> a
```

```haskell
divideThenAdd :: Fractional a => a -> a -> a 
divideThenAdd x y = (x / y) + 1

divideThenAdd :: Num a => a -> a -> a  -- error
divideThenAdd x y = (x / y) + 1
```

## 6.6 Type-defaulting typeclasses

Haskell Report:

```haskell
default Num Integer
default Real Integer
default Enum Integer
default Integral Integer
default Fractional Double
default RealFrac Double
default Floating Double
default RealFloat Double
```

### 6.7 Ord

```haskell
-- :info Ord
-- Ord is constrained by Eq
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
instance Ord a => Ord (Maybe a)
instance (Ord a, Ord b) => Ord (Either a b)
instance Ord Integer
instance Ord a => Ord [a]
instance Ord Ordering
instance Ord Int
instance Ord Float
instance Ord Double
instance Ord Char
instance Ord Bool
```

#### Intermission: Exercises

### 6.8 Enum

```haskell
-- :info Enum
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
instance Enum Ordering
instance Enum Integer
instance Enum Int
instance Enum Char
instance Enum Bool
instance Enum ()
instance Enum Float
instance Enum Double
```

```haskell
succ 4 -- 5
pred 'd' -- c
succ 4.5 -- 5.5
enumFormTo 3 8 -- [3, 4, 5, 6, 7, 8]
enumFromTo 'a' 'f' -- "abcdef"
enumFromThenTo 1 10 100 ---[1,10,19,28,37,46,55,64,73,82,91,100]
```

### 6.9 Show

not for serialization, but for human redability.

```haskell

class Show a where
  showsPrec :: Int -> a -> ShowS 
  show :: a -> String
  showList :: [a] -> ShowS
  
instance Show a => Show [a] 
instance Show Ordering
instance Show a => Show (Maybe a) 
instance Show Integer
instance Show Int instance Show Char 
instance Show Bool instance Show () 
instance Show Float instance Show Double
```

#### Printing and side effects

```haskell
-- :t print
print :: Show a => a -> IO ()
```

main function => side effect 

() : empty tuple / unit: represents nothing

expression must have return value. () represents the end of IO action.

IO String:  a means of producing a String, which perform side effects before get the value.

#### Working with Show

minimal implementation: implement show / showPrec

```haskell
data Mood = Blah

instance Show Mood where
  show _ = "blah"

-- Blah => blah
```

```haskell
data Mood = Blah deriving Show
-- Blah -> Blah
```

##### Typeclass deriving

Typeclass instances we can magically derive are Eq, Ord, Enum, Bounded, Read, and Show.

### 6.10 Read

```haskell
-- :t read
read :: Read a => String -> a
```

avoid using it:

```haskell
read "1234567" :: Integer
-- 1234567

read "BLAH" :: Integer
-- *** Exception: Prelude.read: no parse
```

### 6.11 Instances are dispatched by type

* a typeclass defines a set of functions and/or values

* types have instances of that typeclass

* the instances specify the ways that type uses the functions of the typeclass

```haskell
class Numberish a where 
  fromNumber :: Integer -> a 
  toNumber :: a -> Integer
  
-- pretend newtype is data for now
newtype Age =
  Age Integer 
  deriving (Eq, Show)

instance Numberish Age where 
  fromNumber n = Age n 
  toNumber (Age n) = n
  
newtype Year =
  Year Integer 
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n 
  toNumber (Year n) = n
```

usage:

```haskell
sumNumberish :: Numberish a => a -> a -> a 
sumNumberish a a' = fromNumber summed where 
  integerOfA = toNumber a 
  integerOfAPrime = toNumber a'
  summed = integerOfA + integerOfAPrime
  
sumNumberish (Age 10) (Age 10)
-- Age 20
```

```haskell
-- This is even worse than the last one.
-- Don't use typeclasses to define default values.
-- Seriously. Haskell Ninjas will find you
-- and replace your toothpaste with muddy chalk.

class Numberish a where 
  fromNumber :: Integer -> a 
  toNumber :: a -> Integer 
  defaultNumber :: a
  
instance Numberish Age where 
  fromNumber n = Age n 
  toNumber (Age n) = n 
  defaultNumber = Age 65
  
instance Numberish Year where 
  fromNumber n = Year n 
  toNumber (Year n) = n 
  defaultNumber = Year 1988
  
defaultNumber -- error

defaultNumber :: Age -- Age 65

defaultNumber :: Year -- Year 1988
```

#### Why not write a typeclass like this?

see Monoid chapter

### 6.12 Writing typeclass instances

#### Eq instances

Minimal complete definition: either == or /=.

```haskell
data Trivial = Trivial

Trivial == Trivial -- error, no instance


data Trivial = Trivial'

-- instance TYPECLASS TYPE where
instance Eq Trivial where
-- (DATA CONSTRUCTOR == DATA CONSTRUCTOR) = BOOL
  Trivial' == Trivial' = True
-- (==) Trivial' Trivial' = True -- prefix notation

Trivial' == Trivial' -- True
```

another example:

```haskell
data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  
-- day of week and numerical day of month
data Date =
  Date DayOfWeek Int
  
instance Eq DayOfWeek where 
  (==) Mon Mon = True 
  (==) Tue Tue = True 
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') = 
    weekday == weekday' && dayOfMonth == dayOfMonth'
    

Date Thu 10 == Date Thu 10
-- True
Date Thu 10 == Date Thu 11
-- False
Date Thu 10 == Date Weds 10
-- False
```

#### Partial functions — not so strange danger

```haskell
f :: Int -> Bool
f 1 = True

f 1 -- True
f 2 -- error

-- using a unconditional case
f :: Int -> Bool
f 1 = True
f _ = False
```

#### Sometimes we need to ask for more

```haskell
data Identity a = Identity a

-- error 
instance Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- okay, ensure a to have an instance of Eq
instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

```

#### Intermission: Exercises

```haskell
-- 1
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (Tis a) (Tis b) = a == b

-- 2
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = a' == a' && b == b'
  
-- 3
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False
  
-- 4
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b) = a' == a' && b == b'

-- 5
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b) = a' == a' && b == b'
  
-- 6
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq Which a where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _ = False
  
-- 7
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) _ _ = False
```

#### Ord instances

```haskell
data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun 
  deriving (Ord, Show)
  
-- default Eq, left value smaller 
Mon > Tue -- False
Sun > Mon -- True
compare Tue Weds -- LT

-- custom instance
instance Ord DayOfWeek where 
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT 
  compare _ _ = EQ

compare Fri Sat -- GT
compare Sat Mon -- EQ
compare Fri Mon -- GT
compare Sat Fri -- LT
Mon > Fri -- False
Fri > Sat -- True
```

### 6.13 Gimme more operations

```haskell
-- error
add :: a -> a -> a 
add x y = x + y

-- okay
add :: Num a => a -> a -> a 
add x y = x + y

-- error
addWeird :: Num a => a -> a -> a 
addWeird x y =
  if x > 1 
  then x + y 
  else x
  
-- okay
addWeird :: (Num a, Ord a) => a -> a -> a 
addWeird x y =
  if x > 1 
  then x + y 
  else x
```

#### Ord implies Eq

```haskell
-- error
check' :: a -> a -> Bool
check' a a' = a == a'

-- okay
check' :: Ord a => a -> a -> Bool 
check' a a' = a == a'
```

#### Concrete types imply all the typeclasses they provide

```haskell
add :: Int -> Int -> Int
add x y = x + y
addWeird :: Int -> Int -> Int 
addWeird x y =
  if x > 1 
  then x + y 
  else x
  
check' :: Int -> Int -> Bool 
check' a a' = a == a'
```

### 6.14 Chapter Exercises

#### Does it typecheck?

```haskell
-- 1
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2
data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot 
                 then Blah
                 else x
```

#### Given a datatype declaration, what can we do?

```haskell
data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- 1 fixed
phew = Papu (Rocks "chases") (Yeah True)

-- 2
truth = Papu (Rocks "chomskydoz")
             (Yeah True)

-- 3 
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4  error no instance Ord
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

```

#### Match the types

#### Type-Kwon-Do

```haskell
-- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool 
chk f a b = (f a) == b

-- 2 
-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b 
arith f i a = (f a) + (fromInteger i)
```

### 6.15 Chapter Definitions

1. typeclass inheritance: superclass => typeclass

```haskell
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
```

2. Side effects:

3. IO:

4. instance: typeclass => type

5. derived instance: deriving ..

### 6.16 Typeclass inheritance, partial

Eq => Ord => Real
      
Num => Real / Fractional

Real, Enum => Integral

### 6.17 Follow-up resources

1. P. Wadler and S. Blott. [How to make ad-hoc polymorphism less ad hoc.](http://www.cse.iitk.ac.in/users/karkare/courses/2010/ cs653/Papers/ad-hoc-polymorphism.pdf)
2. Cordelia V. Hall, Kevin Hammond, Simon L. Peyton Jones, and Philip L. Wadler. [Typeclasses in Haskell.](http://ropas.snu.ac.kr/lib/dock/HaHaJoWa1996.pdf)

## CHAPTER7. MOREFUNCTIONALPATTERNS

### 7.1 Make it func-y

function:

* as values in expressions, lists, or tuples; 
* as arguments in function;
* as returned result from function; 
* make use of syntactic patterns.

### 7.2 Arguments and parameters

#### Declaring parameters

#### Binding variables to values

```haskell
bindExp :: Integer -> String 
bindExp x = let y = 5 in
              "the integer was: " ++ show x 
              ++ " and y was: " ++ show y
```

shadowing:

```haskell
bindExp :: Integer -> String 
-- x is shadowed
bindExp x = let x = 10; y = 5 in
              "the integer was: " ++ show x 
              ++ " and y was: " ++ show y
```

### 7.3 Anonymous functions

#### Intermission: Exercises

### 7.4 Pattern matching

pattern => value === data constructor

#### Handling all the cases

order matters

#### Pattern matching against data constructors

```haskell
module RegisteredUser where 

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

-- RegisteredUser :: Username -> AccountNumber -> User
-- Username :: String -> Username
-- AccountNumber :: Integer -> AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser" 
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum)) 
          = putStrLn $ name ++ " " ++ show acctNum
          
-- usage:
printUser UnregisteredUser -- "UnregisteredUser"

let myUser = (Username "callen")
let myAcct = (AccountNumber 10456)
printUser $ RegisteredUser myUser myAcct
```

unpack data constructor: 

```haskell
data WherePenguinsLive = Galapagos
                       | Antarctica
                       | Australia
                       | SouthAfrica
                       | SouthAmerica deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool 
galapagosPenguin (Peng Galapagos) = True 
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool 
antarcticPenguin (Peng Antarctica) = True 
antarcticPenguin _ = False

-- in this final function, the || operator
-- is an `or` function, which will return True 
-- if either value is True 
antarcticOrGalapagos :: Penguin -> Bool 
antarcticOrGalapagos p = (galapagosPenguin p) 
                         || (antarcticPenguin p)
```

#### Pattern matching tuples

#### Intermission: Exercises

```haskell
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f)) 
f (a, b, c) (d, e, f) = ((a, d), (c, f))
```

### 7.5 Case expressions

```haskell
f x = if x + 1 == 1 then "AWESOME" else "wut"

-- using case
funcZ x = case x + 1 == 1 of
            True -> "AWESOME" 
            False -> "wut"
            
pal' xs = case y of
            True -> "yes"
            False -> "no"
          where y = xs == reverse xs
```

#### Intermission: Exercises

```haskell
functionC x y = if (x > y) then x else y
-- using case
functionC x y = case x > y of
                  True -> x
                  False -> y

ifEvenAdd2 n = if even n then (n+2) else n
-- using case
ifEvenAdd2 n = case even n of 
                 True -> n + 2 
                 False -> n
                 

nums x = case compare x 0 of
           LT -> -1 
           GT -> 1
           EQ -> 0
```

### 7.6 Higher-order functions

```haskell
-- :t flip
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
```

#### Intermission: Exercises

### 7.7 Guards 
```haskell
myAbs :: Integer -> Integer 
myAbs x
  | x < 0 = (-x) 
  | otherwise = x
```

#### Intermission: Exercises

### 7.8 Function composition

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)
```


```haskell
-- why using $ ?
negate . sum $ [1, 2, 3, 4, 5] -- -15

-- :i (.) 
(.) :: (b -> c) -> (a -> b) -> a -> c
infixr 9 .

-- composition precedence 9, function application 10

-- wrong
negate . sum [1, 2, 3, 4, 5]
negate . 15 -- error
```

```haskell
take 5 . reverse $ [1..10]
-- [10,9,8,7,6]

take 5 . enumFrom $ 3
-- [3,4,5,6,7]

take 5 . filter odd . enumFrom $ 3
-- [3,5,7,9,11]
```

### 7.9 Pointfree style 
point: argument

### 7.10 Demonstrating composition

print: the composition of show and putStrLn

```haskell
pusStr :: String -> IO()
putStrLn :: String -> IO()

show :: Show a => a -> String

print :: Show a => a -> IO ()

print :: Show a => a -> IO ()
print = putStrLn . show
```

### 7.11 Chapter Exercises

#### Let’s write code

```haskell
tensDigit :: Integral a => a -> a 
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10
        
-- using divMod
tensDigit :: Integral a => a -> a 
tensDigit x = d
  where xLast = fst $ divMod x 10
        d = lst $ divMod xLast 10 
```

```haskell
foldBool :: a -> a -> Bool -> a
-- using case
foldBool x y b = case b of
                   True -> x
                   False -> y

-- using guard
foldBool x y b =
               | b = x
               | otherwise = y

-- using pattern matching
foldBool3 :: a -> a -> Bool -> a 
foldBool3 x y True = x 
foldBool3 x y False = y
```

```haskell
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
```

```haskell
read :: Read a => String -> a
show :: Show a => a -> String

roundTrip :: (Show a, Read a) => a -> a 
roundTrip a = read (show a)

main = do
  print (roundTrip 4) 
  print (id 4)


-- using point free
roundTrip :: (Show a, Read a) => a -> a 
roundTrip = read . show


-- force to Int
main = do
  print (roundTrip 4 :: Int) 
  print (id 4)
```

### 7.12 Chapter Definitions

1. Binding or bound 
2. anonymousfunction
3. Currying
4. Pattern matching: Pattern matching is about your data.
5. Bottom: non-value, lazy
6. Higher-order functions
7. Composition
8. Pointfree

### 7.13 Follow-up resources

1. Paul Hudak; John Peterson; Joseph Fasel. [A Gentle Introduction to Haskell](https://www.haskell.org/tutorial/patterns.html), chapter on case expressions and pattern matching.

2. Simon Peyton Jones. [The Implementation of Functional Programming Languages](http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/index.htm), pages 53-103. 

3. Christopher Strachey. [Fundamental Concepts in Programming Languages](http://www.cs.cmu.edu/~crary/819-f09/Strachey67.pdf), page 11 for explanation of currying. 

4. J.N. Oliveira. [An introduction to pointfree programming](http://www.di.uminho.pt/~jno/ps/iscalc_1.ps.gz).
5. Manuel Alcino Pereira da Cunha. [Point-free Program Calculation](http://www4.di.uminho.pt/~mac/Publications/phd.pdf).

## CHAPTER8. RECURSION

### 8.1 Recursion

### 8.2 Factorial

```haskell
fact :: Int -> Int
fact 1 = 1
fact i = i * fact (i-1)
```

#### Another way to look at recursion

Any programming language, such as Haskell, that is built purely on lambda calculus has only one verb: apply a function to an argument.

#### Intermission: Exercise

### 8.3 Bottom

⊥ or bottom: computations that do not successfully result in a value.

```haskell
f :: Bool -> Int
f False = 0
f _ = error $ "*** Exception: "
            ++ "Non-exhaustive"
            ++ "patterns in function f"
```

partial function / total function

```haskell
data Maybe a = Nothing | Just a

f :: Bool -> Maybe Int 
f False = Just 0
f _ = Nothing
```


### 8.4 Fibonacci numbers

1. Consider the types

```haskell
fibonacci :: Integer -> Integer 
-- or
fibonacci :: Integral a => a -> a
```

2. Consider the base case

```haskell
fibonacci :: Integral a => a -> a
fibonacci 0 = 0 fibonacci 1 = 1
```

3. Consider the arguments

```haskell
fibonacci :: Integral a => a -> a 
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = (x - 1) (x - 2)
-- note: this doesn't work quite yet.
```

4. Consider the recursion

```haskell
fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2) 
```

### 8.5 Integral division from scratch

```haskell
dividedBy :: Integer -> Integer -> Integer 
dividedBy = div

-- type synonyms, changes to
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator -> Denominator -> Quotient 
dividedBy = div
```

```haskell
dividedBy :: Integral a => a -> a -> (a, a) 

dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)
```


### 8.6 Chapter Exercises

#### Review of types

```haskell
-- :t
[[True, False], [True, True], [False, True]] -- [[Bool]]
```

#### Reviewing currying

#### Recursion
1.

```haskell
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n-d) d (count+1)
```

2.

```haskell
recSum :: (Eq a, Num a) => a -> a
recSum n = go n 0
  where go n acc
          | n < 0 = acc
          | otherwise = go (n-1) (acc+n)
```

3.

```haskell
recMul :: (Integral a) => a -> a -> a
recMul a b = go a b 0
  where go a b acc
          | a == 0 = acc
          | otherwise = go (a-1) b (acc+b)
```

#### Fixing dividedBy

#### McCarthy 91 function

```haskell
mc91 :: Int -> Int
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11

map mc91 [95..110]
-- [91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]
```

#### Numbers into words

```haskell
module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
                  0 -> "zero"
                  1 -> "one"
                  2 -> "two"
                  3 -> "three"
                  4 -> "four"
                  5 -> "five"
                  6 -> "six"
                  7 -> "seven"
                  8 -> "eight"
                  9 -> "nine"
                  
digits :: Int -> [Int] 
digits n = go n []
  where go n acc
         | n > 9 = go (div n 10) ((mod n 10) : acc)
         | otherwise = n : acc
         
wordNumber :: Int -> String 
wordNumber n = concat . intersperse '-' . map digitToWord . digits 
```

### 8.7 Definitions

1. Recursion

## CHAPTER9. LISTS

### 9.1 Lists

### 9.2 The list datatype

```haskell
-- data TYPE-CONSTRUCTOR ARGS = 
-- DATA CONSTRUCTOR or  
-- DATA CONSTRUCTOR and MORE LIST
data [] a = [] | a : [a]
```

(:) : cons / construct

### 9.3 Pattern matching on lists

```haskell -- fall safe
myHead [] = []
myHead (x : _) = x

-- fall safe
myTail [] = []
myTail (_: xs) = xs
```

#### Using Maybe

```haskell
data Maybe a = Nothing | Just a
```

```haskell
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : []) = Nothing
safeTail (_ : xs) = Just xs
```

### 9.4 List’s syntactic sugar

```haskell
[1, 2, 3] ++ [4]

-- equals to

1 : 2 : 3 : [] ++ 4 : []
```

### 9.5 Using ranges to construct lists

```haskell
[1..10]
-- equals to
enumFromTo 1 10

[1, 2..10]
```

```haskell
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
```

#### Exercise

```haskell
myEnumFromTo :: Enum a => a -> a -> [a]
myEnumFromTo x y
      | xi > yi = []
      | otherwise = x : myEnumFromToi (succ xi) yi
      where xi = fromEnum x
            yi = fromEnum y
            myEnumFromToi a b
                | a > b = []
                | otherwise = (toEnum a) : myEnumFromToi (succ a) b
```

### 9.6 Extracting portions of lists

```haskell
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
splitAt :: Int -> [a] -> ([a], [a])


takeWhile :: (a -> Bool) -> [a] -> [a] 
dropWhile :: (a -> Bool) -> [a] -> [a]
```

#### Intermission: Exercises

1. reverse using ```dropWhile``` and ```takeWhile```

```haskell
myWords :: [Char] -> [[Char]]
myWords [] = []
myWords a@(x:xs)
        | x == ' ' = myWords xs
        | otherwise = takeWhile (/=' ') a : (myWords . dropWhile (/=' ') $ a)
        
-- using case
myWords :: [Char] -> [[Char]]
myWords [] = []
myWords a = 
  case dropWhile (==' ') a of
                           [] -> []
                           xs -> takeWhile (/=' ') xs
                                 : myWords (dropWhile (/=' ') xs)
                         
-- using break
myWords :: [Char] -> [[Char]]
myWords [] = []
myWords a = 
  case dropWhile (==' ') a of
                           [] -> []
                           az -> w : rest
                                 where (w, rest) = break (==' ') az 


myWords :: String -> [String]
myWords [] = []
myWords (' ':xs) = myWords xs
myWords xs =  takeWhile (/= ' ') xs : myWords (dropWhile (/= ' ') xs)
```

2.

```haskell
module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
            ++ thirdSen ++ fourthSen
            
-- putStrLn sentences 
-- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this

myLines :: String -> [String]
myLines [] = []
myLines xs = case dropWhile (== '\n') xs of
                      [] -> []
                      az -> w : myLines rest
                            where (w, rest) = break (== '\n') az

-- What we want 'myLines sentences' to equal 
shouldEqual =
      [ "Tyger Tyger, burning bright"
      , "In the forests of the night"
      , "What immortal hand or eye"
      , "Could frame thy fearful symmetry?"
      ]
-- The main function here is a small test 
-- to ensure you've written your function 
-- correctly.

main :: IO ()
main =
  print $ "Are they equal? "
        ++ show (myLines sentences == shouldEqual)
```

3.

```haskell
separate :: Char -> [Char] -> [[Char]]
separate _ [] = []
separate sep xs = case dropWhile (== sep) xs of
                      [] -> []
                      az -> w : separate sep rest
                            where (w, rest) = break (== sep) az

-- using purely takeWhile and dropWhile
separate' :: Char -> [Char] -> [[Char]]
separate' _ [] = []
separate' sep xs = w : separate' sep ws
  where w = takeWhile (/= sep) xs
        ws = dropWhile (== sep) (drop (length w) xs)

myWords :: [Char] -> [[Char]]
myWords = separate ' '


myLines :: String -> [String]
myLines = separate '\n'
```

### 9.7 List comprehensions

#### Adding predicates

#### Intermission: Exercises

#### List comprehensions with Strings

```haskell
-- :t elem
elem :: Eq a => a -> [a] -> Bool
```

#### Intermission: Exercises

```haskell
[(x, y) | x <- mySqr, y <- myCube]

[(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
```

### 9.8 Spines and non-strict evaluation

spine (:) => shape of tree, It is also possible to evaluate only part of the spine of a list and not the rest of it.

spine-strict: length

spine-lazt: map

non-strictness

#### Using GHCi’s :sprint command

```:spring```

```haskell
l = ['a'..'z'] -- WHNF

-- :sprint
l = _

take 5 l
-- :sprint
l = 'a' : 'b' : 'c' : 'd' : 'e' : _
```

#### Spines are evaluated independently of values

 weak head normal form: evaluated to reach data constructor
 
 normal form: fully evaluated

```haskell
(1, 2) -- WHNF & NF

(1, _ + _)
-- WHNF, but not NF. The (+) and its
-- unknown arguments could be evaluated

(1, 1 + 1)
-- WHNF, but not NF.
-- The 1 + 1 could be evaluated.

\x -> x * 10 -- WHNF & NF

"Papu" ++ "chon" -- Neither WHNF nor NF
```

only evaluating spine, so wont crash

```haskell
x = [1, undefined, 3]
length x -- 3

-- like this
length :: [a] -> Integer 
length [] = 0
length (_:xs) = 1 + length xs
```

#### Intermission: Exercises

#### Intermission: Is it in normal form?

### 9.9 Transforming lists of values

map lazy

```haskell
take 1 . map (+1) $ [1, 2, undefined] -- [2]
```

*lazy in the spine, strict in the leaves*

#### Intermission: Exercises

### 9.10 Filtering lists of values

```haskell
filter :: (a -> Bool) -> [a] -> [a] 
filter _ [] = []
filter pred (x:xs)
  | pred x = x : filter pred xs 
  | otherwise = filter pred xs
```

#### Intermission: Exercises



```haskell
filter (\x -> rem x 3 == 0) [1..30]
```

2.

```haskell
length . filter (\x -> rem x 3 == 0) $ [1..30]
```

3.

```haskell
filter \x -> not $ elem x ["the", "a", "an"] . words $ "the brown dog was a goof"

filter (not . flip elem ["the", "a", "an"]) . words $ "the brown dog was a goof"
```

### 9.11 Zipping lists

```haskell
-- :t zip
zip :: [a] -> [b] -> [(a, b)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
```

#### Zipping exercises

1.

```haskell
zip' :: [a] -> [b] -> [(a, b)]
zip' (a: az) (b: bz) = (a, b) : zip' az bz
zip' _ _ = []
```

2.

```haskell
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:az) (b:bz) = f a b : zipWith' az bz
zipWith' _ _ _ = []
```

### 9.12 Chapter Exercises

#### Data.Char

```haskell
import Data.Char

fUp = filter isUpper

capFisrt :: [Char] -> [Char]
capFisrt (x:xs) = toUpper x : xs
capFisrt _ = ""

capAll :: [Char] -> [Char]
capAll (x:xs) = toUpper x : capAll xs
capAll _ = ""

fstCap :: [Char] -> Char
fstCap xs = toUpper $ head xs
```

#### Ciphers

```haskell
myOr :: [Bool] -> Bool
myOr (x:xs) = if x then True else myOr xs
myOr [] = False

myOr' [] = False
myOr' (x:xs)
    | x = x
    | otherwise = myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny f (x:xs) = if f x then True else myAny f xs
myAny _ _ = False


myElem :: Eq a => a -> [a] -> Bool
myElem a (x:xs) = if a == x then True else myElem a xs
myElem _ _ = False


myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse _ = []

myReverse' a = go a []
  where go (x:xs) acc = go xs (x:acc)
        go _ acc = acc


squish :: [[a]] -> [a]
squish (x:xs) = x ++ squish xs
squish _ = [] 


squishMap :: (a -> [b]) -> [a] -> [b] 
squishMap f (x:xs) = f x ++ squishMap f xs
squishMap _ _ = []


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a 
myMaximumBy f (acc:xs) = go f acc xs
  where go f acc (x:xs)
          | f acc x == GT = go f acc xs
          | otherwise = go f x xs
        go _ acc _ = acc
myMaximumBy _ _ = undefined
```

### 9.13 Definitions

1. Product type: tuples or data constructors with more than one argument. 

2. Sum type: using the pipe, |


3. Cons:

4. Cons cell:

5. spine

### 9.14 Answers

### 9.15 Follow-up resources

1. [Data.List documentation for the base library](http://hackage.haskell.org/package/base/docs/Data-List.html)

2. [Ninety-nine Haskell problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).

## CHAPTER10. FOLDINGLISTS

### 10.1 Folds

Catamorphism: https://en.wikipedia.org/wiki/Catamorphism

### 10.2 Bringing you into the fold

```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

### 10.3 Recursive patterns

### 10.4 Fold right

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f acc xs = case xs of
                   [] -> acc
                   (x:xs) -> f x (foldr f acc xs)
```

#### How foldr evaluates

stage: traversal and folding

used with infinite list

```haskell
foldr (\_ _ -> 9001) 0 [1..5]
-- 9001

foldr (\_ _ -> 9001) 0 [1, 2, 3, undefined]
-- 9001

foldr (\_ _ -> 9001) 0 ([1, 2, 3] ++ undefined)
-- 9001

foldr (\_ _ -> 9001) 0 [1..]
-- 9001
```

```haskell
const x _ = x
foldr const 0 [1..] 
-- 1
```

### 10.5 Fold left

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b 
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
```

```haskell
f = (\x y -> concat ["(",x,"+",y,")"])

foldl f "0" (map show [1..5])
"(((((0+1)+2)+3)+4)+5)"

foldr f "0" (map show [1..5])
"(1+(2+(3+(4+(5+0)))))"

foldr (+) 0 [1..5] -- 15

scanr (+) 0 [1..5] -- [15,14,12,9,5,0]

foldl (+) 0 [1..5] -- 15

scanl (+) 0 [1..5] -- [0,1,3,6,10,15]

last (scanl f z xs) = foldl f z xs
head (scanr f z xs) = foldr f z xs
```

#### Associativity and folding

```haskell
foldr (:) [] [1..3] -- [1,2,3]

foldl (flip (:)) [] [1..3] -- [3,2,1]
```

#### Intermission: Exercises

#### Unconditional spine recursion

foldl: forced spine evaluation => finite list

foldl'

### 10.6 How to write fold functions

#### Intermission: Exercises

### 10.7 Folding and evaluation

```haskell
foldr f acc a = foldl (flip f) acc (reverse a) 
```

foldl only works for finite list

### 10.8 Summary

foldr: 

1. foldr :: ```(a -> b -> b) -> b -> [a] -> b```; b in ```-> b ->``` is the rest of the fold

2. associate to the right

3. infinite list, lazy evaluation

foldl:

1. produce value after reaching the end

2. associate to the right

3. finite list

4. nearly useless; prefer foldl'

### 10.9 Scans

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
scanr :: (a -> b -> b) -> b -> [a] -> [b]
foldl :: (b -> a -> b) -> b -> [a] -> b
scanl :: (b -> a -> b) -> b -> [a] -> [b]
```

```haskell
scanl :: (a -> b -> a) -> a -> [b] -> [a] 
scanl f q ls = 
  q : (case ls of 
         []   -> []
         x:xs -> scanl f (f q x) xs)
```

#### Getting the fibonacci number we want

bang bang operator:

```haskell
(!!) :: [a] -> Int -> a
```

```haskell
fibs = 1 : scanl (+) 1 fibs -- infinite list

fibsN x = fibs !! x
```

#### Scans Exercises

### 10.10 Chapter Exercises

#### Warm-up and review

1.

```haskell
stops = "pbtdkg"
vowels = "aeiou"

svs :: [Char] -> [Char] -> [(Char, Char, Char)]
svs stops vowels = [(s, v, s2) | s <- stops, v <- vowels, s2 <- stops]

svs :: [Char] -> [Char] -> [(Char, Char, Char)]
svs stops vowels = [(s, v, s2) | s <- stops, v <- vowels, s2 <- stops, p == 'p']
```

3.

```haskell
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

-- rewrite
seekritFunc ::  Fractional a => String -> a
seekritFunc x = fromIntegral (sum (map length (words x))) 
                / fromIntegral (length (words x))
```

#### Prime number machine

#### Rewriting functions using folds

```haskell
myOr :: [Bool] -> Bool
myOr = foldr || False


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a acc -> f a || acc) False


myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a acc -> x == a || acc) False


myReverse :: [a] -> [a] 
myReverse = foldl (flip :) []


myMap :: (a -> b) -> [a] -> [b] 
myMap f = foldr (\a acc -> f a : acc) []
myMap' f = foldr ((:) . f) [] -- nicer


myFilter :: (a -> Bool) -> [a] -> [a] 
myFilter f = foldr (\a acc -> if f a then a : acc else acc) []
myFilter' f = foldr g []
                where g a acc
                        | f a = a : acc
                        | otherwise = acc


squish :: [[a]] -> [a]
squish = foldr (++) []


squishMap :: (a -> [b]) -> [a] -> [b] 
squishMap f = foldr (\a acc -> f a ++ acc) []
squishMap' f = foldr ((++) . f) [] -- nicer


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a 
myMaximumBy f (x:xs) = foldr (\a acc -> if f a acc == GT 
                                        then a 
                                        else acc) 
                             x
                             xs
myMaximumBy _ [x] = x
myMaximumBy _ [] = undefined

myMaximumBy' f (x:xs) = foldr g x xs
                          where g a b
                                  | g a b == GT = a
                                  |otherwise b
myMaximumBy' _ [x] = x
myMaximumBy' _ [] = undefined
```

### 10.11 Definitions

1. Fold:

2. Catamorphism: breaking down structure

3. tail call: final result of a function

4. Tail recursion: a function whose tail calls are recursive invo- cations of itself

### 10.12 Answers

### 10.13 Follow-up resources

1. Haskell Wiki. [Fold](https://wiki.haskell.org/Fold).

2. Richard Bird. Sections 4.5 and 4.6 of Introduction to Functional Programming using Haskell (1998).

3. Antoni Diller. [Introduction to Haskell](http://www.cantab.net/users/antoni.diller/haskell/units/unit06.html)

4. Graham Hutton. [A tutorial on the universality and expressive- ness of fold](http://www.cs.nott.ac.uk/~gmh/fold.pdf).

## CHAPTER11. ALGEBRAICDATATYPES

### 11.1 Algebraic datatypes

pattern matching, type checking, and inference

A type can be thought of as an enumeration of constructors that have zero or more arguments.

sum types, product types: record syntax, type aliases (String = [Char]), newtype

This chapter will:

* ```algebra``` in algebraic datatypes
* data constructors
* custom datatypes
* type synonyms / newtype;
* kinds

### 11.2 Data declarations review

```haskell
-- data TYPE-CONSTRUCTOR (ARGS) = DATA-CONSTRUCTOR (ARGS) or ..

-- enumeration of two possible constructors
data Bool = False | True

data [] a = [] | a:[a]
```

### 11.3 Data and type constructors

type constructors: type level, in type signatures and typeclass declarations and instances. Types are static and resolve at compile time. 

data constructors: construct the values at term level, values you can interact with at runtime.

constants: Type and data constructors that take no arguments. They can only store a fixed type and amount of data. eg, Bool - type constant;. It enumerates two values that are also constants, True and False, because they take no arguments. 

```haskell
-- type constants, value constants
data Trivial = Trivial'

-- type constructor, data constructor
data UnaryTypeCon a = UnaryValueCon a
```

#### Type constructors and kinds

kind:

```haskell
-- :k Bool
Bool :: *

-- :k [Int]
[Int] :: *

-- :k []
[] :: * -> *
```

### 11.4 Data constructors and values

data constructors: 

constant values:

```haskell
-- type constant, constant value
data PugType = PugData

-- phantom a,  constant value
data HuskyType a = HuskyData

-- 
data DogueDeBordeaux doge = DogueDeBordeaux doge
```

query ```:t``` of the data, not the type

#### Intermission Exercises

### 11.5 What’s a type and what’s data?

type constructors - compile-time

----> phase separation --->

data constructors - runtime

#### Intermission: Exercises

### 11.6 Data constructor arities

Arity: number of arguments a function or constructor takes.

nullary: takes no arguments

```haskell
-- nullary
data Example0 =
Example0 deriving (Eq, Show)

-- unary
data Example1 =
Example1 Int deriving (Eq, Show)

-- product of Int and String
data Example2 =
Example2 Int String deriving (Eq, Show)
```

### 11.7 What makes these datatypes algebraic?

cardinality

#### Intermission: Exercises

#### Simple datatypes with nullary data constructors

#### Intermission: Exercises

#### Unary constructors

it has cardinality same as the type they contain

#### newtype

no runtime overhead

```haskell
newType Goats = Goats Int deriving (Eq, Show)

newType Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42
```
 
```haskell
class TooMany a where 
  tooMany :: a -> Bool
  
instance TooMany Int where 
  tooMany n = n > 42
  
newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43
  

newtype Goats = Goats Int deriving (Eq, Show)

instance TooMany Goats where 
  tooMany (Goats n) = tooMany n
```

GeneralizedNewtypeDeriving:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
class TooMany a where 
  tooMany :: a -> Bool
  
instance TooMany Int where 
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
-- add pragma, so do not need instance
```

#### Intermission: Exercises

1.

```haskell
{-# FlexibleInstances #-}
class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (n, s) = n > 42
  
-- with newtype
newtype Goats = Goats (Int, String) deriving Show

instance TooMany Goats where
  tooMany (Goats (n, s)) = n > 42
```

2.

```haskell
class TooMany a where
  tooMany :: a -> Bool
  
newtype Goats = Goats (Int, Int) deriving Show
  
instance TooMany Goats where
  tooMany (Goats (a, b)) = (a + b) > 42
```

3. 

```haskell
{-# LANGUAGE FlexibleInstances #-}
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, n') =  tooMany (n + n')
```

### 11.8 Sum types

#### Intermission: Exercises

### 11.9 Product types

#### Record syntax

record: product types with additional syntax to access fields

```haskell
data Person = MkPerson String Int deriving (Eq, Show)

-- these are just sample data
jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s

-- uaing record
data Person = Person { name :: String
                     , age :: Int } 
                     deriving (Eq, Show)


-- :t name
name :: Person -> String
-- :t age
age :: Person -> Int

Person "Papu" 5
-- Person {name = "Papu", age = 5}
papu = Person "Papu" 5
age papu -- 5
name papu -- "Papu"
```

#### Intermission: Jammin Exercises

```haskell
module Jammin where

data Fruit = Peach
           | Plum
           | Apple
           | Blackberry deriving (Eq, Show)
           
data JamJars = Jam Fruit Int deriving (Eq, Show)

-- using record syntax
data JamJars = Jam {fruit :: Fruit 
                   ,jars  :: Int} deriving (Eq, Show, Ord)

rowJars :: [JamJars] -> [Int]
rowJars = map jars

jarsCount :: [JamJars] -> Int
jarsCount = sum . rowJars

mostRow :: [JamJars] -> JamJars
mostRow = maximumBy (\j1 j2 -> compare (jars j1) (jars j2))

compareKind (Jam k _) (Jam k' _) = compare k k'
sortJams :: [JamJars] -> [JamJars]
sortJams = sortBy compareKind

groupJam :: [JamJars] -> [JamJars]
groupJam = groupBy (\j1 j2 -> fruit j1 == fruit j2) . sortJams

```

### 11.10 Normal form

distributive property: ```a * (b + c) -> (a * b) + (a * c)```

Product types distribute over sum types

normal form: sum of products

```haskell
data Expr = Number Int
          | Add Expr Expr
          | Minus Expr
          | Mult Expr Expr
          | Divide Expr Expr
```

```haskell
type Number = Int
type Add = (Expr, Expr) 
type Minus = Expr
type Mult = (Expr, Expr) 
type Divide = (Expr, Expr)

type Expr = Either Number
              (Either Add 
                (Either Minus
                  (Either Mult Divide)))
```

#### Exercises

### 11.11 Constructing and deconstructing values

#### Sum and Product

#### Constructing values

#### Exercise

```haskell
data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill 
                     | Mac
                     | Windows deriving (Eq, Show)

data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript deriving (Eq, Show)
                         
data Programmer = 
  Programmer {os :: OperatingSystem
             ,lang :: ProgrammingLanguage} deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem] 
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDStill
                      , Mac
                      , Windows ]
                      
allLanguages :: [ProgrammingLanguage] 
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer] 
allProgrammers = [ Programmer { os = o
                             , lang = l} 
                   | o <- allOperatingSystems
                   , l <- allLanguages ]
```

#### Accidental bottoms from records

```haskell
-- partially apply record
partialAf = Programmer {os = GnuPlusLinux}

partialAf -- error
```

#### Deconstructing values

```haskell
newtype Name    = Name String deriving Show
newtype Acres   = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show

-- destructing
isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False


data Farmer = Farmer { name       :: Name
                     , acres      :: Acres
                     , farmerType :: FarmerType } deriving Show

-- destruct record
isDairyFarmer :: Farmer -> Bool
isDairyFarmer farmer = farmerType farmer == DairyFarmer
```

#### Dang it, more accidental bottoms from records

### 11.12 Function type is exponential

```a -> b``` : b ^ a

```a -> b -> c``` : (c ^ b) ^ a = c ^ (b * a)

#### Exponentiation in what order?

#### Intermission: Exercises

### 11.13 Higher-kinded datatypes

```haskell
-- Identical to (a, b, c, d)
-- :kind (,,,)
(,,,) :: * -> * -> * -> * -> *

```

### 11.14 Lists are polymorphic

Any operator that starts with a colon (:) must be an infix type or data constructor.

```haskell
data Product a b = a :&: b deriving (Eq, Show)

1 :&: 2 :: (Num a, Num b) => Product a b
```

```haskell
dataList a = Nil | Cons a (List a)

oneItem = (Cons "woohoo!" Nil)
```

### 11.15 Binary Tree

```haskell
data BinaryTree a = 
  Leaf | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Ord, Show)
```

#### Inserting into trees

```haskell
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a 
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right 
  | b > a  = Node left a (insert' b right)
  
-- try it
t1 = insert' 0 Leaf -- Node Leaf 0 Leaf
t2 = insert' 1 t1 -- Node Leaf 0 (Node Leaf 1 Leaf)
t3 = insert' 2 t2 -- Node Leaf 0 (Node Leaf 1 (Node Leaf 3 Leaf))
```

#### Write map for BinaryTree

```haskell
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b 
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree left f) (f a) (mapTree right f)
```

#### Convert binary trees to lists

```haskell
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder = (postorder left) ++ (postorder right) ++ [a]
```

#### Write foldr for BinaryTree

```haskell
-- 3 parameter
foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = f a 
                                       (foldTree f acc left)
                                       (foldTree f acc right)

-- 2 parameter
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc bt = foldr f acc (flattenIn bt [])

flattenIn :: BinaryTree a -> [a] -> [a]
flattenIn Leaf l = l
flattenIn (Node left a right) l = flattenIn left (a : (flattenIn right l))
```

#### Rewrite map for BinaryTree

```haskell
-- 3 parameter, ok
data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = f a (foldTree f acc left) (foldTree f acc right)

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt = foldTree mk Leaf bt
  where mk a l r = Node l (f a) r

-- 2 parameter failed, structure ruined
mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b 
mapTree' f bt = foldTree f Leaf bt
```
  
### 11.16 Chapter Exercises

#### Ciphers

Vigenère cipher

```haskell
import Data.Char

vigenere :: String -> String -> String
vigenere xs ys = vigenere' xs (cycle ys)

vigenere' [] _ = ""
vigenere' (' ':xs) cyp        = ' ' : vigenere' xs cyp
vigenere' (x:xs)   cyp@(y:ys) = docyp x y : vigenere' xs ys
  where base      = ord 'A'
        r         = 26
        dist c    = ord c - base
        docyp x y = chr $ (dist x + dist y) `mod` r + base

main = print $ vigenere "MEET AT DAWN" "ALLY" == "MPPR AE OYWY"
```

#### As-patterns

```haskell
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] ys = True
isSubsequenceOf _ []  = False
isSubsequenceOf sa@(a:az) (b:bz)
  | a == b = isSubsequenceOf az bz 
  | otherwise isSubsequenceOf sa bz
  
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map f . words
  where f wd@(x:xs) = (wd, toUpper x : xs)
```

#### Language exercises

```haskell
import Data.Char (toUpper)
import Data.List (groupBy)
import Data.Function (on)

capitalizeWord :: String -> String
capitalizeWord [] = ""
capitalizeWord (x:xs)
  | x == ' ' = x : capitalizeWord xs
  | otherwise = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph = 
  concatMap capitalizeWord . groupBy ((==) `on` (=='.'))
```

#### Phone exercise

#### Hutton’s Razor

```haskell
data Expr = Lit Integer
          | Add Expr Expr
eval :: Expr -> Integer 
eval (Lit x) = x
eval (Add exp1 exp2) = eval exp1 + eval exp2

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add exp1 exp2) = printExpr exp1 ++ " + " ++ printExp3 exp2
```

### 11.18 Answers

## CHAPTER12. SIGNALINGADVERSITY

### 12.1 Signaling adversity

* Nothing, or Just Maybe
* Either left  or right, but not both
* higher-kindedness
* anamorphisms, but not animorphs.

### 12.2 How I learned to stop worrying and love Nothing

#### Smart constructors for datatypes

```haskell
data Name = String
data Age = Integer
data Person = Person Name Age deriving Show

mkPerson :: String -> Integer -> Maybe Person
mkPerson name age
  | name \= "" && age > 0 = Just $ Person name age
  | otherwise = Nothing
```

### 12.3 Bleating either

```haskell
data Either a b = Left a | Right b

data PersonInvalid = NameEmpty
                   | AgeTooLow deriving (Eq, Show)

-- Compiles fine without Eq
toString :: PersonInvalid -> String 
toString NameEmpty = "NameEmpty" 
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where 
  show = toString

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name \= "" && age > 0 = Right $ Person name age
  | name = "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow

-- can not catch the AgeTooLow fault
mkPerson "" (-1) -- Left NameEmpty
```

```haskell
type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a 

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow deriving (Eq, Show)
                   
ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name 
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name 
          -> ValidatePerson Age 
          -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = 
  Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = 
  Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName 
mkPerson' _ (Left badAge) = Left badAge
```

### 12.4 Kinds, a thousand stars in your types

Kinds are types one level up.

kind: type of type constructor

type: type constructor(higher-kinded type) / type constant

```haskell
-- :kind Int
Int :: *
-- :k Bool
Bool :: *
-- :k Char
Char :: *

data Example a = Blah | RoofGoats | Woot a
-- :k Example
Example :: * -> *

-- :k (,)
(,) :: * -> * -> *

-- :k Maybe
Maybe :: * -> *

-- :k Either
Either :: * -> * -> *
```

####  Lifed and unlifed types

lifted type: (->), bottom, polymorphism

unlifed type: no bottom, native machine types and raw pointers, newtype

### Data constructors are functions

```haskell
data Unary a = Unary a
instance Show a => Show (Unary a)

Unary id -- ??
```

### 12.5 Chapter Exercises

#### String processing

1.

```haskell
import Data.List (intercalate)

notThe :: String -> Maybe String
notThe str
  | str == "the" = Nothing
  | otherwise = Just str

replaceThe :: String -> String
replaceThe = intercalate " " . map athe . fmap notThe . words
  where athe Nothing = "a"
        athe (Just x) = x
```

2.

```haskell
isVowel :: String -> Bool
isVowel (x:xs) = elem x "aeiou"

countTheBeforeVowel :: String -> Integer 
countTheBeforeVowel = f 0 . words

f :: Integer -> [String] -> Integer
f acc (x:y:xs)
  | x == "the" && isVowel y = f (acc+1) xs
  | otherwise = f acc (y:xs)
f acc _ = acc
```

3.

```haskell
isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

countVowels :: String -> Integer
countVowels = f 0

f :: Integer -> String -> Integer
f acc (x:xs)
  | isVowel x = f (acc+1) xs
  | otherwise = f acc xs
f acc _ = acc
```

#### Validate the word

```haskell
newtype Word' =
  Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str
  | count str > 2 * countVowels str = Just (Word' str)
  | otherwise = Nothing
```

#### It’s only Natural

```haskell
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger = f 0
  where f acc Zero = acc
        f acc (Succ Nat) = f (acc+1) Nat

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero)) 
-- 2

integerToNat :: Integer -> Maybe Nat
integerToNat n 
  | n < 0 = Nothing
  | otherwise = Just (f Zero n)
    where f acc n
           | n == 0 = acc
           | otherwise = f (Succ acc) (n-1)
          
-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
```

#### Small library for Maybe

1.

```haskell
-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False
```

2.

```haskell
-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee d _ Nothing = d
```

3.

```haskell
-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1

fromMaybe :: a -> Maybe a -> a
fromMaybe d v = mayybee d id v
```

4.

```haskell
-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe (x:_) = Just x
listToMaybe [] = Nothing

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []
```


5.

```haskell
-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> catMaybes [Nothing, Nothing, Nothing]
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes = map g . filter f
  where g (Just v) = v
        f = (/= Nothing)
        
-- foldr
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' xs = foldr f [] xs
  where f Nothing xs'  = xs'
        f (Just a) xs' = a : xs'
```

6.

```haskell
-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = f []
  where f acc ((Just a):xs) = f (a:acc) xs
        f acc [] = acc
        f _ (Nothing:xs) = Nothing
        
-- using foldr
flipMaybe [] = Just []
flipMaybe xs = foldr f (Just []) xs
  where f _ Nothing = Nothing
        f Nothing _ = Nothing
        f (Just a) (Just b) = Just (a:b)
```

#### Small library for Either

1. 

```haskell
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left a) acc = a : acc
        f _ acc = acc
```

2.

```haskell
rights' :: [Either a b] -> [b]
lefts' = foldr f []
  where f (Right a) acc = a : acc
        f _ acc = acc
```

3.

```haskell
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where f (Left a) (az, bz) = (a:az, bz)
        f (Right b) (az, bz) = (az, b:bz)
```

4.

```haskell
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ (Left a) = Nothing
```

5.

```haskell
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b
```

6.

```haskell
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f =  either' f1 f2
  f1 _ = Nothing
  f2 = Just . f
```

#### Unfolds

anamorphisms - catamorphisms

unfoldr

```haskell
take 10 $ iterate (+1) 0
-- [0,1,2,3,4,5,6,7,8,9]

-- :t unfoldr
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

take 10 $ unfoldr (\b -> Just (b, b+1)) 0
-- [0,1,2,3,4,5,6,7,8,9]
```

#### Why bother?

```haskell
import Data.List

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a 
        go n [] = n
        go n (x:xs) = (go (n+x) xs) 

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a 
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a 
        go n [] = n
        go n (x:xs) = (go (n*x) xs) 
        
niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go :: [a] -> [[a]] -> [a] 
        go xs' [] = xs'
        go xs' (x:xs) = (go (xs' ++ x) xs) 

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []
```

#### Write your own iterate and unfoldr

1.

```haskell
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)
```

2.

```haskell
unfoldr (\b -> Just (b, b+1)) 0

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = g $ f b
  where g (Just (a, b)) = a : myUnfoldr f b
        g _ = []
```

3.

```haskelZ
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr g
  where g b = Just (b, f b)
```

#### Finally something other than a list!

```haskell
data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a) 
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b 
unfold f b = case f b of
                Nothing -> Leaf
                Just (x, y, z) -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = go n Leaf
  where go 0 acc = acc
        go n acc = go (n-1) (Node acc (n-1) acc)

-- using unfold
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f m
          | m == n = Nothing
          | otherwise = Just (m + 1, m, m + 1)

```

### 12.6 Definitions

1. higher kinded type: 

```haskell
Maybe :: * -> *
[] :: * -> *
Either :: * -> * -> *
(->) :: * -> * -> *

-- The following are not:
Int :: *
Char :: *
String :: *
[Char] :: *
```
## CHAPTER15. MONOID,SEMIGROUP

### 15.1 Monoids and semigroups

* Algebras!
* Laws!
* Monoids!
* Semigroups!

### 15.2 What we talk about when we talk about algebras

Algebra: mathematical symbols and the rules, operation <= implemented with typeclass, 

### 15.3 Monoid

monoid: binary associative operation with an identity

monoid: function that takes two arguments and follows two laws: associativity and identity

```haskell
-- [] = mempty, or the identity
-- mappend is the binary operation 
-- to append, or join, two arguments 
mappend [1..5] [] = [1..5]
mappend [] [1..5] = [1..5]
-- or, more generally
mappend x mempty = x 
mappend mempty x = x
```

### 15.4 How Monoid is defined in Haskell

```haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

### 15.5 Examples of using Monoid

#### List

```haskell
instance Monoid [a] where 
  mempty = []
  mappend = (++)

-- usage:
mappend [1, 2, 3] [4, 5, 6]
-- [1,2,3,4,5,6]

mconcat [[1..3], [4..6]]
-- [1,2,3,4,5,6]

mappend "Trout" " goes well with garlic"
-- "Trout goes well with garlic"

(++) [1, 2, 3] [4, 5, 6]
-- [1,2,3,4,5,6]

(++) "Trout" " goes well with garlic"
-- "Trout goes well with garlic"

foldr (++) [] [[1..3], [4..6]]
-- [1,2,3,4,5,6]

foldr mappend mempty [[1..3], [4..6]]
-- [1,2,3,4,5,6]
```

### 15.6 Why Integer doesn’t have a Monoid

Both summation, multiplication are monoidal (binary, associative, having an identity value), but each type should only have one unique instance for a given typeclass, not two (one instance for a sum, one for a product).

solution: wrap the value with Sum, Product newtype

```haskell
mappend (Sum 1) (Sum 5)
-- Sum {getSum = 6}

mappend (Product 5) (Product 5)
-- Product {getProduct = 25}

mappend (Sum 4.5) (Sum 3.4)
-- Sum {getSum = 7.9}
```

#### Why newtype?

```haskell
data Server = Server String
newtype Server' = Server' String
```

newtype: unary data constructor, no additional runtime overhead(identical to what it wraps)

#### In summary, why you might use newtype

#### More on Sum and Product

```haskell
import Data.Monoid
-- :info Sum
newtype Sum a = Sum {getSum :: a}
-- .. other instances
instance Num a => Monoid (Sum a)

-- :info Product
newtype Product a = Product {getProduct :: a}
-- .. other instances
instance Num a => Monoid (Product a)
```

```haskell
-- :t (<>)
(<>) :: Monoid m => m -> m -> m

mappend (Sum 8) (Sum 9)
(Sum 8) <> (Sum 9)
-- Sum {getSum = 17}

mappend mempty Sum 9
mempty <> Sum 9
-- Sum {getSum = 9}

-- error
-- mappend (Sum 1) (Sum 2) (Sum 3)
mappend (Sum 1) (mappend (Sum 2) (Sum 3))
(Sum 1) `mappend` (Sum 2) `mappend` (Sum 3)
(Sum 1) <> (Sum 2) <> (Sum 3)
-- Sum {getSum = 6}

mconcat [(Sum 8), (Sum 9), (Sum 10)]
-- Sum {getSum = 27}
```

### 15.7 Why bother?

A common use of monoids is to structure and describe common modes of processing data. Sometimes this is to describe an API for incrementally processing a large dataset, sometimes to describe guar- antees needed to roll up aggregations (think summation) in a parallel, concurrent, or distributed processing framework.

```haskell
foldr mappend mempty ([2, 4, 6] :: [Product Int])
-- Product {getProduct = 48}

foldr mappend mempty ([2, 4, 6] :: [Sum Int])
-- Sum {getSum = 12}

foldr mappend mempty ["blah", "woot"]
-- "blahwoot"
```

### 15.8 Laws

law - algebra

```haskell
-- left identity
mappend mempty x = x 

-- right identity
mappend x mempty = x 

-- associativity
mappend x (mappend y z) = mappend (mappend x y) z
mconcat = foldr mappend mempty
```

### 15.9 Different typeclass instance, same representation

Bool wrapper:

```haskell
import Data.Monoid
All True <> All True
-- All {getAll = True}

All True <> All False
-- All {getAll = False}

Any True <> Any False
-- Any {getAny = True}

Any False <> Any False
-- Any {getAny = False}
```

Maybe wrapper:

```haskell
First (Just 1) `mappend` First (Just 2)
-- First {getFirst = Just 1}

Last (Just 1) `mappend` Last (Just 2)
-- Last {getLast = Just 2}

Last Nothing `mappend` Last (Just 2)
-- Last {getLast = Just 2}

First Nothing `mappend` First (Just 2)
-- First {getFirst = Just 2}
```

### 15.10 Reusing algebras by asking for algebras

```haskell
instance Monoid b => Monoid (a -> b)
instance (Monoid a, Monoid b) => Monoid (a, b)
instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)
```

#### Exercise

```haskell
data Optional a = Nada | Only a deriving (Eq, Show)
instance Monoid a => Monoid (Optional a) where 
  mempty                    = Nada
  mappend Nada (Only a)     = Only a
  mappend (Only a) Nada     = Only a
  mappend Nada Nada         = Nada
  mappend (Only a) (Only b) = Only (mappend a b)

-- usage:
Only (Sum 1) `mappend` Only (Sum 1)
-- Only (Sum {getSum = 2})

Only (Product 4) `mappend` Only (Product 2)
-- Only (Product {getProduct = 8})

Only (Sum 1) `mappend` Nada
-- Only (Sum {getSum = 1})

Only [1] `mappend` Nada
-- Only [1]

Nada `mappend` Only (Sum 1)
-- Only (Sum {getSum = 1})
```

#### Associativity

monoid is not necessarily commutative

Commutative: reorder the arguments, not just reassociate the parentheses, and still get the same result.

commutative: (+) (*)

not commutative: (-) (++)

#### Identity

identity:

identity value:

#### The problem of orphan instances

problem:

```haskell
module Listy where

newtype Listy a =
  Listy [a] deriving (Eq, Show)
```

```haskell
module ListyInstances where 

import Data.Monoid
import Listy

instance Monoid (Listy a) where
  mempty = Listy []
  mappend (Listy l) (Listy l') = Listy $ mappend l l'
```

solution:

1. You defined the type but not the typeclass? Put the instance in the same module as the type so that the type cannot be imported without its instances.

2. You defined the typeclass but not the type? Put the instance in the same module as the typeclass definition so that the typeclass cannot be imported without its instances.

3. Neither the type nor the typeclass are yours? Define your own
newtype wrapping the original type and now you’ve got a type that “belongs” to you for which you can rightly define typeclass instances.

### 15.11 Madness

```haskell
import Data.Monoid
type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj = mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with this ", adj, " wife."]
```

### 15.12 Better living through QuickCheck

#### Validating associativity with QuickCheck

```haskell
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool 
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
```

#### Quickchecking left and right identity

```haskell
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

quickCheck (monoidLeftIdentity :: String -> Bool)
-- +++ OK, passed 100 tests.

quickCheck (monoidRightIdentity :: String -> Bool)
-- +++ OK, passed 100 tests.
```

#### Testing QuickCheck’s patience

#### Intermission: Exercise

```haskell
newtype First' a =
  First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only x)) _ = First' (Only x)
  mappend (First' Nada) (First' (Only x)) = First' (Only x)
  mappend _ _ = First' Nada
  
firstMappend :: First' a -> First' a -> First' a 
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool
  
main :: IO () main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)

```

### 15.13 Semigroup

```haskell
class Semigroup a where
  (<>) :: a -> a -> a

(a <> b) <> c = a <> (b <> c)
```

#### NonEmpty, a useful datatype

```haskell
-- :| is the data constructor, the product of a and [a]
data NonEmpty a = a :| [a] 
  deriving (Eq, Ord, Show)

-- or like this
newtype NonEmpty a = NonEmpty (a, [a]) 
  deriving (Eq, Ord, Show)
```

ok with binary associative operation, but not with identity.

so, with semigroup:

```haskell
-- you need to have `semigroups` installed
import Data.List.NonEmpty as N
import Data.Semigroup as S

1 :| [2, 3]
-- 1 :| [2,3]

:t 1 :| [2, 3]
-- 1 :| [2, 3] :: Num a => NonEmpty a

:t (<>)
-- (<>) :: Semigroup a => a -> a -> a

xs = 1 :| [2, 3]
ys = 4 :| [5, 6]
xs <> ys
-- 1 :| [2,3,4,5,6]

N.head xs
-- 1
N.length (xs <> ys)
-- 6
```

#### Strength can be weakness

```haskell
class Semigroup a => Monoid a where
  ...
```

the inverse relationship: operations permitted over a type and the number of types that can satisfy.

```haskell
id :: a -> a
```

* Number of types: Infinite

* Number of operations: one

```haskell
inc :: Num a => a -> a
```

* Number of types: anything that implements Num. Zero to many. 
* Number of operations: 7 methods in Num

```haskell
somethingInt :: Int -> Int
```
* Number of types: one — just Int.
* Number of operations: considerably more than 7. In addition to Num, Int has instances of Bounded, Enum, Eq, Integral, Ord, Read, Real, and Show. On top of that, you can write arbitrary functions that pattern match on concrete types and return arbitrary values in that same type as the result. Polymorphism isn’t only useful for reusing code; it’s also useful for expressing intent through parametricity so that people reading the code know what we meant to accomplish.

### 15.14 Chapter exercises

#### Semigroup exercises

#### Monoid exercises

```haskell
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where 
  (<>) = Trivial
  
instance Monoid Trivial where 
  mempty = Trivial
  mappend = (<>)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO () main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
```

### 15.15 Definitions

1. monoid

2. semigroup

3. Law

4. algebra: informal notion of operations over a type and its laws, such as with semigroups, monoids, groups, semirings, and rings.

### 15.16 Follow-up resources

1. [Algebraic structure](https://simple.wikipedia.org/wiki/Algebraic_structure); Simple English Wikipedia;

2. [Algebraic structure](https://en.wikipedia.org/wiki/Algebraic_structure); English Wikipedia

## CHAPTER16. FUNCTOR

### 16.1 Functor

functor: Rudolf Carnap in the 1930s.

* the return of the higher-kinded types;
* fmaps;
* typeclasses and constructor classes; 

### 16.2 What’s a functor?

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

### 16.3 There’s a whole lot of fmap going round

```haskell
-- Functor f =>
fmap :: (a -> b) -> f a -> f b
     :: (a -> b) -> [] a -> [] b
     :: (a -> b) -> Maybe a -> Maybe n
     :: (a -> b) -> Either e a -> Either e b
     :: (a -> b) -> (e,) a -> (e,) b
     :: (a -> b) -> Identity a -> Identity b
     :: (a -> b) -> Constant e a -> Constant e b
```

### 16.4 Let’s talk about 𝑓 , baby

```haskell -- :k (->)
(->) :: * -> * -> *
```

#### Intermission: Exercises

#### A shining star for you to see

#### Functor is function application

```haskell
fmap :: Functorf => (a -> b) -> f a -> f b
(<$>) :: Functorf => (a -> b) -> f a -> f b
($) :: (a->b) -> a -> b
```

#### A shining star for you to see what your 𝑓 can truly be

```haskell
data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)
```

#### Typeclasses and constructor classes

### 16.5 Functor Laws

#### Identity

```haskell
fmap id == id
```

#### Composition

```haskell
fmap (f . g) == fmap f . fmap g
```

#### Structure preservation

### 16.6 The Good, the Bad, and the Ugly

```haskell
data WhoCares a = ItDoesnt
                | Matter a
                | WhatThisIsCalled deriving (Eq, Show)

instance Functor WhoCares where 
  fmap _ ItDoesnt = ItDoesnt 
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)
```

#### Composition should just work

### 16.7 Commonly used functors

#### The functors are stacked and that’s a fact

```haskell
lms = [Just "Ave", Nothing, Just "woohoo"]
replaceWithP = const 'p'

(fmap . fmap) replaceWithP lms
-- [Just 'p', Nothing, Just 'p']

(.) :: (b -> c) -> (a -> b) -> a -> c
fmap :: Functor f => (m -> n) -> f m -> f n 
fmap :: Functor g => (x -> y) -> g x -> g y

fmap . fmap = ((m -> n) -> (f m -> f n))
           -> ((x -> y) -> (g x -> g y))
            = (x -> y) -> (f g x -> f g y)
            = (x -> y) -> f g x -> f g y
```

#### One more round for the P-Funkshun

#### Intermission: Lifting Exercises

1.

```haskell
a = fmap (+1) (read "[1]" :: [Int])
```

2.

```haskell
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
```

3.

```haskell
c = (*2) . (\x -> x -2)
```

4.

```haskell
d = ((return '1' ++) . show) . (\x -> [x, 1..3])
```

### 16.8 Mapping over the structure to transform the unapplied type argument

```haskell
data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

-- error 
instance Functor Two where fmap = undefined
instance Functor Or where fmap = undefined

-- ok
instance Functor (Two a) where 
  fmap f (Two a b) = Two a (f b)
instance Functor (Or a) where 
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)
```

### 16.9 QuickChecking Functor instances

### 16.10 Intermission: Exercises

1.

```haskell
newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
```

2.

```haskell
data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)
```

3.

```haskell
data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
```

4.

```haskell
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
```

5.

```haskell
data Three' a b = Three' a b b 

instance Functor (Three' a) where
  fmap f (Three' a b c) = Two a (f b) (f c)
```

6.

```haskell
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Two a b c (f d)
```

7.

```haskell
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Two a b c (f d)
```

### 16.11 Ignoring possibilities

#### Maybe

#### Short Exercise

```haskell
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where 
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)
```

#### Either

#### Short Exercise

```haskell
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where 
  fmap _ (First a) = First a
  fmap f (Second a) = Second (f a)
```

### 16.12 A somewhat surprising functor

### 16.13 More structure, more functors

```haskell
data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor (Wrap f) where 
  fmap f (Wrap fa) = Wrap (f fa)
instance Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)
instance Functor f => Functor (Wrap f) where 
  fmap f (Wrap fa) = Wrap (fmap f fa)
```

### 16.14 IO Functor

```haskell
getLine :: IO String
read :: Read a => String -> a

getInt :: IO Int
getInt = fmap read getLine

addOne = fmap (+1) getInt

-- or
meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input + 1)
```

### 16.15 What if we want to do something different?

natural transformations: transform the structure, but not the arguments

```haskell
nat :: (f -> g) -> f a -> g a

{-# LANGUAGE RankNTypes #-}
type Nat f g = forall a. f a -> g a


-- This'll work
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- This will not work, not allowed.
degenerateMtl :: Nat Maybe []
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a+1]
```

### 16.16 Functors in Haskell are unique for a given datatype

### 16.17 Chapter exercises

1.

```haskell
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ (Desk a) = Desk a
  fmap _ Finance = Finance
```

2. 

```haskell
data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a
```

3.

```haskell
{-# LANGUAGE FlexibleInstances #-}
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K a b = K a

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f b))
```

4.

```haskell
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)
```

5.

```haskell
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)
```

6.

```haskell
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)
```

7.

```haskell
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)
```

8.

```haskell
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)
```

9.

```haskell
data List a = Nil | Cons a (List a)

instance List where
  fmap f (Cons a (List a)) = Cons (f a) (List (f a))
  fmap _ Nil = Nil
```

10.

```haskell
data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLoard where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f MoreGoats ga1 
                   ga2 
                   ga3 = MoreGoats (fmap f ga1)
                                   (fmap f ga2)
                                   (fmap f ga3)
```

11.

```haskell
data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)
                
instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (fmap f g)
```

### 16.18 Definitions

1. Higher-kinded polymorphism

2. Functor

3. lifting

4. George Clinton

### 16.19 Follow-up resources

1. Haskell Wikibook; [The Functor class](en.wikibooks.org/wiki/Haskell/The_Functor_class).

2. Mark P. Jones; A system of constructor classes: overloading and implicit higher-order polymorphism.

3. Gabriel Gonzalez; [The functor design pattern](http://www.haskellforall.com/2012/09/the-functor-design-pattern.html).
## CHAPTER17. APPLICATIVE 

### 17.1 Applicative

### 17.2 Defining Applicative

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-- Could call <*> tie-fighter or "ap" (short for apply)
```

every type that can have an Applicative instance must also have a
Functor instance.

pure: like ```identity```, embed sth into ```f```

<*>: ap, 

```haskell
(<$>) :: Functor f =>       (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

### 17.3 Functor vs. Applicative

```haskell
fmap :: (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b

fmap f x = pure f <*> x

fmap (+1) [1, 2, 3] -- [2, 3, 4]
pure (+1) <*> [1, 2, 3] -- [2, 3, 4]
```

```pure``` embed the value into the structure

```haskell
pure 1 :: [Int] -- [1]
pure 1 :: Maybe Int -- Just 1
pure 1 :: Either a Int -- Right 1
pure 1 :: ([a], Int) -- ([],1)

fmap (+1) (4, 5)
```

### 17.4 Applicative functors are monoidal functors

```haskell
($) :: (a->b)-> a-> b 
(<$>) :: (a -> b) -> f a -> f b 
(<*>) :: f (a -> b) -> f a -> f b
```

```haskell
mappend :: Monoid a => a -> a -> a

mappend :: f             f      f
$       :: (a -> b)      a      b
(<*>)   :: f (a -> b) -> f a -> f b
```

```haskell
[(*2), (*3)] <*> [4, 5]
-- [8, 10, 12, 15]
```

```haskell
Just (*2) <*> Just 2 = Just 4
Just (*2) <*> Nothing = Nothing
Nothing <*> Just 2 = Nothing
Nothing <*> Nothing = Nothing
```

#### Show me the monoids

```haskell
:info (,)
data (,) a b = (,) a b  -- Defined in ‘GHC.Tuple’
...
instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
...
instance (Monoid a, Monoid b) => Monoid (a, b)

-- functor
fmap (+1) ("blah", 0)
-- ("blah",1)

-- ap
("Woo", (+1)) <*> (" Hoo!", 0)
-- ("Woo Hoo!", 1)

((Sum 2), (+1)) <*> ((Sum 0), 0)
-- (Sum {getSum = 2}, 1)

((Product 3), (+9)) <*> ((Product 2), 8)
-- (Product {getProduct = 6}, 17)

((All True), (+1)) <*> ((All False), 0)
-- (All {getAll = False}, 1)
```

#### Tuple Monoid and Applicative side by side

```haskell
instance (Monoid a, Monoid b) => Monoid (a,b) where 
  mempty = (mempty, mempty)
  (a, b) `mappend` (a', b') = (a `mappend` a', b `mappend` b')

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)

```

#### Maybe Monoid and Applicative

```haskell
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  mappend m Nothing = m
  mappend Nothing m = m
  mappend (Just a) (Just a') = Just (mappend a a') 
  
instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  Just f <*> Just a = Just (f a)
```

### 17.5 Applicative in use

#### List Applicative

```haskell
-- f ~ []
(<*>) :: f (a -> b) -> f a -> f b 
(<*>) :: [] (a -> b) -> [] a -> [] b
-- more syntactically typical
(<*>) :: [(a -> b)] -> [a] -> [b]

pure :: a -> f a 
pure :: a -> [] a
```

#### What’s the List applicative do?

```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b 
-- f ~ []
listApply :: [(a -> b)] -> [a] -> [b]
listFmap :: (a -> b) -> [a] -> [b]
```

functor: a function to a plurality of values:

```haskell
fmap (2^) [1, 2, 3]
-- [2,4,8]
fmap (^2) [1, 2, 3]
-- [1,4,9]
```

ap: a plurality of function to a plurality of values:

```haskell
[(+1), (*2)] <*> [2, 4]
-- [3,5,4,8]
```

```haskell
(,) <$> [1, 2] <*> [3, 4]
-- [(1,3),(1,4),(2,3),(2,4)]
```

#### Short Exercises

1. 

```haskell
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
```

2.

```haskell
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]
tupled :: Maybe (Integer, Integer) 
tupled = (,) <$> y <*> z
```

3.

```haskell
import Data.List (elemIndex) 
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5] 
y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5] 
max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int 
maxed = max' <$> x <*> y
```

#### Identity

#### Specializing the types

#### Exercise

```haskell
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where 
  fmap f (Identity a)= Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity b)= Identity (f b)
```

#### Constant

#### Specializing the types

```haskell
-- f ~ Constant e

(<*>) :: f (a -> b) -> f a -> f b 
(<*>) :: Constant e (a -> b) -> Constant e a -> Constant e b

pure :: a -> f a
pure :: a -> Constant e a
```

#### Exercise

```haskell
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where 
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where 
  pure = Constant
  (<*>) (Constant a) (Constant b) = Constant (a b)
```

#### Maybe Applicative

#### Specializing the types

```haskell
-- f ~ Maybe
(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b

pure :: a -> f a
pure :: a -> Maybe a
```

```haskell
validateLength :: Int -> String -> Maybe String 
validateLength maxLen s = if (length s) > maxLen 
                          then Nothing
                          else Just s
                          
newtype Name = Name String deriving (Eq, Show) 
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person 
mkPerson n a = case mkName n of
                 Nothing -> Nothing 
                 Just n' -> case mkAddress a of 
                              Nothing -> Nothing 
                              Just a' -> Just $ Person n' a'
                              
--usage:
Person <$> (mkName "Babe") <*> (mkAddress 1)
```

#### Maybe Functor and the Name constructor

```haskell
instance Functor Maybe where
  fmap _ Nothing = Nothing 
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where 
  pure = Just
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing 
  Just f <*> Just a = Just (f a)
```

#### Exercise

1.

```haskell
const <$> Just "Hello" <*> pure "World"
```

2. 

```haskell
(,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
```

### 17.6 Applicative laws

#### 1. Identity

```haskell
id v = v
fmap id v = v
pure id <*> v = v
```

#### 2. Composition

```haskell
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```

#### 3. Homomorphism

```haskell
pure f <*> pure x = pure (f x)
```

#### 4. Interchange

```haskell
u <*> pure y = pure ($ y) <*> u 
($ 1) (*1) = 1
```

### 17.7 You knew this was coming

### 17.8 ZipList Monoid

#### Zero vs. Identity

```haskell
Sum 1 `mappend` ??? -> Sum 1

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend
```

#### List Applicative Exercise

```haskell
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where 
  fmap _ Nil = Nil
  fmap f (Cons a az) = Cons (f a) (fmap f az)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f a) bz = fmap f bz <> (a <*> bz)

  
-- usage:
functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)
functions <*> values
-- Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
```

```haskell
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b 
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a 
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b 
flatMap f (a:as) = f a ++ (flatMap f as)
flatMap _ [] = []
```

### Exercise

```haskell
data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a 
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a az) = Cons a (take' (n-1) az)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil a = a
  mappend a Nil = a
  mappend (Cons x xs) ys = Cons x (mappend xs ys)

instance Functor List where 
  fmap _ Nil = Nil
  fmap f (Cons a az) = Cons (f a) (fmap f az)

instance Applicative List where 
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons x xs) ys = fmap x ys <> (xs <*> ys)


newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where 
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs 
                in take' 3000 l
          ys' = let (ZipList' l) = ys 
                in take' 3000 l
                
instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeatList :: a -> (List a)
repeatList x = xs
  where xs = Cons x xs

zipListWith :: (a -> b -> c) -> (List a) -> (List b) -> (List c)
zipListWith _ Nil _ = Nil
zipListWith _ _ Nil = Nil
zipListWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipListWith f as bs)

instance Applicative ZipList' where
  pure = ZipList' . repeatList
  (<*>) (ZipList' fs ) (ZipList' xs) = ZipList' (zipListWith id fs xs)
```

#### Either and Validation Applicative

```haskell
-- f ~ Either e
(<*>) :: f (a -> b) -> f a -> f b 
(<*>) :: Either e (a -> b) -> Either e a -> Either e b

pure :: a -> f a 
pure :: a -> Either e a
```

#### Either versus Validation

```haskell
pure 1 :: Either e Int
-- Right 1
Right (+1) <*> Right 1
-- Right 2
Right (+1) <*> Left ":("
-- Left ":("
```

#### Exercise

```haskell data Sum a b = First a | Second b deriving (Eq, Show)

data Validation e a = Error e
                    | Success a deriving (Eq, Show)

instance Functor (Sum a) where 
  fmap _ (First a) = First a
  fmap f (Second b) = Second b
  
instance Applicative (Sum a) where 
  pure = Second
  (<*>) (First a) _ = First a
  (<*>) _ (First a) = First a
  (<*>) (Second b) (Second c) = Second (b c)
  
instance Functor (Validation e) where
  fmap _ (Error e) = Error e
  fmap f (Success a) = Success a
  
instance Monoid e => Applicative (Validation e) where
  pure = Success 
  (<*>) (Error e) (Error e') = Error (e <> e')
  (<*>) _ (Error e) = Error e
  (<*>) (Error e) _ = Error e
  (<*>) (Success f) (Success b) = Success (f b)
```

### 17.9 Chapter Exercises

#### Combinations

1. 

```haskell
newtype Identity a = Identity a deriving Show

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)
```

2. 

```haskell
data Pair a = Pair a a deriving Show

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')
```

3. 

```haskell
data Two a b = Two a b

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two a f) (Two a' b) = Two (a <> a') (f b)
```

4. 

```haskell
data Three a b c = Three a b c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three a b f) (Three a' b' c) = Three (a <> a') 
                                              (b <> b')
                                              (f c)
```

5. 

```haskell
data Three' a b = Three' a b b

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a f g) (Three' a' b b') = Three' (a <> a') 
                                                 (f b)
                                                 (g b')
```

6. 

```haskell
data Four a b c d = Four a b c d

instance (Monoid a, 
          Monoid b, 
          Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four a b c f) (Four a' b' c' d) = Four (a <> a')
                                                (b <> b')
                                                (c <> c')
                                                (f d)
```

7. 

```haskell
data Four' a b = Four' a a a b

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' a b c f) (Four' a' b' c' d) = Four' (a <> a')
                                                   (b <> b')
                                                   (c <> c')
                                                   (f d)
```

#### Combinations

### 17.10 Definitions

1. Applicative

### 17.11 Follow-up resources

1. Tony Morris; Nick Partridge; [Validation library](http://hackage.haskell.org/package/validation)

2. Conor McBride; Ross Paterson; [Applicative Programming with Effects](http://staff.city.ac.uk/~ross/papers/Applicative.html)

3. Jeremy Gibbons; Bruno C. d. S. Oliveira; Essence of the Iterator Pattern

4. Ross Paterson; [Constructing Applicative Functors](http://staff.city.ac.uk/~ross/papers/Constructors.html)

5. Sam Lindley; Philip Wadler; Jeremy Yallop; Idioms are oblivious, arrows are meticulous, monads are promiscuous.

### 17.12 Answers

## CHAPTER18. MONAD

#### 18.1 Monad

#### 18.2 Sorry — Monad is not a burrito

monad: applicative functor

```haskell
class Applicative m => Monad m where 
  (>>=) :: m a -> (a -> m b) -> m b 
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

#### Applicative m

```haskell
fmap f xs = xs >>= return . f
```

#### Core operations

#### The novel part of Monad

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b 
<*> :: Applicative f => f (a -> b) -> f a -> f b 
>>= :: Monad f => f a ->(a -> f b) -> f b
```

monad is a generalization of ```concat```

```haskell
fmap :: Functor f => (a -> f b) -> f a -> f (f b)

concat :: Foldable t => t [a] -> [a]
```

```haskell
import Control.Monad (join)
join :: Monad m => m (m a) -> m a

bind :: Monad m => (a -> m b) -> m a -> m b 
bind = join . fmap
```

#### What Monad is not

Monad is not:

1. Impure
2. for imperative programming
3. value
4. About strictness

#### Monad also lifts!

### 18.3 Do syntax and monads

```haskell
(*>) :: Applicative f => f a -> f b -> f b
(>>) :: Monad m =>       m a -> m b -> m b
```

```haskell
getLine :: IO String
putStrLn :: String -> IO ()

binding :: IO () 
binding = do
  name <- getLine 
  putStrLn name

binding' :: IO () 
binding' = getLine >>= putStrLn
```

#### When fmap alone isn’t enough

```haskell
getLine <$> putStrLn :: IO (IO ())
join $ getLine <$> putStrLn :: IO ()

```

with and without ```do``` syntax:

```haskell
bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name -> putStrLn ("y helo thar: " ++ name)
```

### 18.4 Examples of Monad use

#### List

##### Specializing the types

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b 
(>>=) :: [a] -> (a -> [b]) -> [b]

return :: Monad m => a -> m a 
return :: a -> [a]
```

#### Example of the List Monad in use

```haskell
twiceWhenEven :: [Integer] -> [Integer] 
twiceWhenEven xs = do
  x <- xs 
  if even x
    then [x * x, x * x] 
    else [x * x]

twiceWhenEven [1..3] -- [1,4,4,9]

twiceWhenEven :: [Integer] -> [Integer] 
twiceWhenEven xs = do
  x <- xs 
  if even x
    then [x*x, x*x] 
    else []
```

#### Maybe

##### Specializing the types

```haskell

-- m ~ Maybe
(>>=) :: Monad m => m a -> (a -> m b) -> m b 
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

-- same as pure
return :: Monad m => a -> m a
return :: a -> Maybe a
```

#### Using the Maybe Monad

#### Exploding a spherical cow

```haskell
instance Monad Maybe where 
  return x = Just x
  (Just x) >>= k = k x 
  Nothing >>= _ = Nothing
```

#### Fail fast, like an overfunded startup

bottom:

```haskell
Nothing >>= undefined
-- Nothing
```

#### Either

##### Specializing the types

```haskell
-- m ~ Either e
(>>=) :: Monad m => m a -> (a -> m b) -> m b 
(>>=) :: Either e a -> (a -> Either e b) -> Either e b

-- same as pure
return :: Monad m => a -> m a 
return :: a -> Either e a

```

#### Using the Either Monad

```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b 
ap    ::       Monad m => m (a -> b) -> m a -> m b

ap :: (Monad m) => m (a -> b) -> m a -> m b 
ap m m' = do
  x <- m
  x' <- m' 
  return (x x')
```

#### Exercise

```haskell
data Sum a b = First a
             | Second b deriving (Eq, Show)

instance Functor (Sum a) where 
  fmap _ (First a)= First a
  fmap f (Second b) = Second (f b)
  
instance Applicative (Sum a) where
  pure = Second
  (<*>) (First a) _ = First a
  (<*>) _ (First a) = First a
  (<*>) (Second f) (Second b) = Second (f b)
    
instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second b) f = f b
```

### 18.5 Monad laws

#### Identity laws

```haskell
-- right identity
m >>= return = m 
-- left identity
return x >>= f = f x
```

#### Associativity 
```haskell
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

#### We’re doing that thing again

#### Bad Monads and their denizens

### 18.6 Application and composition

```haskell
mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join (f <$> (g a))
-- or with monad
mcomp'' f g a = g a >>= f
```

```haskell
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c 
flip (.) :: (a -> b) -> (b -> c) -> a -> c
```

### 18.7 Chapter Exercises

1.

```haskell
data Nope a = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) NopeDotJpg _ = NopeDotJpg
```

2.

```haskell
data PhhhbbtttEither b a = Left a | Right b

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Right b) _ = Right b
  (>>=) (Left a) f = f a
```

3.

```haskell
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a)= Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a
```

4.

```haskell
data List a = Nil | Cons a (List a)

instance Monad List where
  return a = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a l) f = f a <> (l >>= f)
```

1.

```haskell
j :: Monad m => m (m a) -> m a
j = join

j [[1, 2], [], [3]]
-- [1,2,3]
j (Just (Just 1))
-- Just 1
j (Just Nothing)
-- Nothing
j Nothing
-- Nothing
```

2.

```haskell
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM
```

3.

```haskell
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2
```

4.

```haskell
a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap
```

5.

```haskell
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (meh xs f) >>= (\xs -> fmap (:xs) (f x))
-- or
meh (x:xs) f = (fmap (\a -> (a:)) $ f x) <*> (meh xs f)
-- or
meh (x:xs) f = (++) <$> (fmap (\a -> [a]) $ f x) <*> (meh xs f)
```

6.

```haskell
flipType :: (Monad m) => [m a] -> m [a]
flipType [] = return []
flipType (x:xs) = (:) <$> x <*> (flipType xs)
```

### 18.8 Definition

1. monad

2. monadic function: ```a -> m b```

3. bind

```haskell
-- lifting (a -> b) over f in f a
fmap :: (a -> b) -> f a -> f b

-- binding (a -> m b) over m in m a
(>>=) :: m a -> (a -> m b) -> m b
```

### 18.9 Follow-up resources

1. [What a Monad is not](https://wiki.haskell.org/What_a_Monad_is_not)
2. Gabriel Gonzalez; [How to desugar Haskell code](http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html)
3. Stephen Diehl; [What I wish I knew when Learning Haskell](http://dev.stephendiehl.com/hask/#monads)
4. Stephen Diehl; [Monads Made Difficult](http://www.stephendiehl.com/posts/monads.html)
5. Brent Yorgey; [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)

## CHAPTER20. FOLDABLE 

### 20.1 Foldable

* the Foldable class and its core operations; 
* the monoidal nature of folding;
* standard operations derived from folding.

### 20.3 Revenge of the monoids

```haskell
class Foldable (t :: * -> *) where
  fold :: Data.Monoid.Monoid m => t m -> m
  foldMap :: Data.Monoid.Monoid m => (a -> m) -> t a -> m
```

```fold``` does not have a Monoid specified:

```haskell -- error
fold [1, 2, 3, 4, 5]

-- ok
fold [Sum 1, Sum 2, Sum 3, Sum 4, Sum 5]
-- Sum {getSum = 15}

-- or this
fold [1, 2, 3, 4, 5 :: Sum Integer]
-- Sum {getSum = 15}
```

#### And now for something different

```haskell
foldMap (*5) [1, 2, 3 :: Product Integer]
-- Product {getProduct = 750}
-- 5 * 10 * 15 = 750

foldMap (*5) [1, 2, 3 :: Sum Integer]
-- Sum {getSum = 30}
-- 5 + 10 + 15 = 30

foldMap (*5) Nothing :: Sum Integer
-- Sum {getSum = 0}
foldMap (*5) Nothing :: Product Integer
-- Product {getProduct = 1}
```

### 20.4 Demonstrating Foldable instances

#### Identity

```haskell
data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x
```

#### Maybe

```haskell
instance Foldable Optional where 
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z
  foldl _ z Nada = z
  foldl f z (Yep x) = f z x
  foldMap _ Nada = mempty 
  foldMap f (Yep a) = f a
  
-- error
foldMap (+1) Nada
-- ok
foldMap (+1) Nada :: Sum Int
-- Sum {getSum = 0}
```

### 20.5 Some basic derived operations

#### Exercises

```haskell
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum
```

```haskell
product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product
```

```haskell
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem el = getAny . foldMap (Any . (el==))
```

```haskell
newtype Min a = Min {getMin :: Maybe a}

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  m `mappend` Min Nothing = m
  Min Nothing `mappend` n = n
  (Min m@(Just x)) `mappend` (Min n@(Just y))
    | x <= y    = Min m
    | otherwise = Min n

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = getMin . foldMap (\a -> Min {getMin = Just a})
```

```haskell
null :: (Foldable t) => t a -> Bool
null = (==0) . length
-- or
null = foldr (\_ _ -> False) True
```

```haskell
length :: (Foldable t) => t a -> Int
length = foldr (\_ acc -> acc + 1) 0
```

```haskell
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []
```

```haskell
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id
```

```haskell
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (\a acc -> f a <> acc) mempty
```

### 20.6 Chapter Exercises

```haskell
data Constant a b = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty
```

```haskell
data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b
```

```haskell
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c
```

```haskell
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f b <> f c
```

```haskell
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = f b <> f c <> f d
```

```haskell filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF f = foldMap fb
  where fb x
          | f x = pure x
          | otherwise = mempty
```

### 20.7 Answers

### 20.8 Follow-up resources

1. Jakub Arnold, [Foldable and Traversable](http://blog.jakubarnold.cz/2014/07/30/foldable-and-traversable.html)

## CHAPTER21. TRAVERSABLE

### 21.1 Traversable

### 21.2 The Traversable typeclass definition

```haskell
class (Functor t, Foldable t) => Traversable t where
  
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f
  
  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id
```

### 21.3 sequenceA

### 21.4 traverse

```haskell
fmap :: (a -> b) ->fa->fb 
(=<<) :: (a -> m b) -> m a -> m b -- flip bind
traverse :: (a -> f b) -> t a -> f (t b)
```

```haskell
traverse = sequenceA . fmap
```

#### mapM is just traverse

traverse is more generic:

```haskell
mapM :: Monad m => (a -> m b) -> [a] -> m [b] 
-- contrast with
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)


sequence :: Monad m => [m a] -> m [a] 
-- contrast with
sequenceA :: (Applicative f, Traversable t) => t (f a)
-> f (t a)
```

### 21.5 So, what’s traversable for?

### 21.6 Morse code revisited

```haskell
-- we want this
(sequence .) . fmap = \ f xs -> sequence (fmap f xs)
-- not this
sequence . fmap = \ f -> sequence (fmap f)

traverse morseToChar (morse "julie")
-- Just "julie"
```


### 21.7 Axing tedious code

```haskell
data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)] makeIoOnlyObj = undefined

-- before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err 
    (Right res) -> do
      a <- makeIoOnlyObj res 
      return $ Right a

-- after
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)

-- or
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn = (traverse makeIoOnlyObj . mapM decodeFn =<<) . fetchFn

-- or mapM = traverse 
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn = 
(traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn

```

### 21.8 Do all the things

#### Strength for understanding

### 21.9 Traversable instances

#### Either

```haskell
data Either a b = Left a
                | Right b deriving (Eq, Ord, Show)

instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)
  
instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = Left e
  Right f <*> r = fmap f r
  
instance Foldable (Either a) where
  foldMap _ (Left _) = mempty
  foldMap f (Right y) = f y
  foldr _ z (Left _) = z
  foldr f z (Right y) = f y z

instance Traversable (Either a) where
  traverse _ (Left x) = pure (Left x) 
  traverse f (Right y) = fmap Right $ f y

```

#### Tuple

```haskell
instance Functor ((,) a) where
  fmap f (x,y) = (x, f y)

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)
  
instance Foldable ((,) a) where
  foldMap f (_, y) = f y
  foldr f z (_, y) = f y z

instance Traversable ((,) a) where
  traverse f (x, y) = (,) x <$> f y

```

### 21.10 Traversable Laws

1. Naturality

```haskell
t . traverse f = traverse (t . f)

-- ???
traverse :: (a -> f b) -> t a -> f (t b)
t . f = a -> f b
f = a -> g c
t = g c -> f b
traverse (t . f) = t a -> f (t b)
traverse f = t a -> g (t c)
t = g c -> f b = g (t c) -> f (t b) -- ??
t . traverse f = t a -> f (t b)
```

2. Identity

```haskell
traverse Identity = Identity
```

3. Composition

```haskell
traverse (Compose . fmap g . f) =
Compose . fmap (traverse g) . traverse f
```

#### sequenceA


1. Naturality

```haskell
t . sequenceA = sequenceA . fmap t 
```

2. Identity

```haskell
sequenceA . fmap Identity = Identity
```

3. Composition

```haskell
sequenceA . fmap Compose =
Compose . fmap sequenceA . sequenceA
```

### 21.11 Quality Control

### 21.12 Chapter Exercises

#### Traversable instances

#### Identity

```haskell
newtype Identity a = Identity a deriving (Eq, Ord, Show) 

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a
```

#### Constant

```haskell
newtype Constant a b = Constant { getConstant :: a }

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a
```

#### Maybe

```haskell
data Optional a = Nada | Yep a

instance Traversable Optional where
  traverse _ Nada = pure $ Nada
  traverse f (Yep a) = Yep <$> f a
```

#### List

```haskell
data List a = Nil | Cons a (List a)

instance Traversable List where
  traverse _ Nil = pure $ Nil
  traverse f (Cons a l) = Cons <$> f a <*> traverse f l
```

#### Three

```haskell
data Three a b c = Three a b c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c
```

#### Three’

```haskell
data Three' a b = Three' a b b

instance Traversable (Three' a) where
  traverse f (Three' a b c) = Three' a <$> f b <*> f c
```

#### S

```haskell
data S n a = S (n a) a

instance Traversable n => Traversable (S n) where 
  traverse f (S na a) = S <$> traverse f na <*> f a
```

#### Instances for Tree

```haskell
data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t1 a t2) = Node (fmap f t1)
                               (f a)
                               (fmap f t2)
                               
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 a t2) = (foldMap f tl) 
                           <> (f a) 
                           <> (foldMap f tr)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t1 a t2) = Node <$> (traverse f t1) 
                                   <*> f a
                                   <$> (traverse f t2)
```

### 21.13 Follow-up resources

1. Jakub Arnold, [Foldable and Traversable](http://blog.jakubarnold.cz/2014/07/30/foldable-and-traversable.html)

2. The Essence of the Iterator Pattern; Jeremy Gibbons and Bruno Oliveira.

3. Applicative Programming with Effects; Conor McBride and Ross Paterson.

## CHAPTER22. READER 

### 22.1 Reader

### 22.2 A new beginning

```haskell
import Control.Applicative

hurr = (*2)
durr = (+10)

m :: Integer -> Integer
m = hurr . durr


m' :: Integer -> Integer
m' = fmap hurr durr

fmap hurr durr x == (*2) ((+10) x)

-- lift partially-applied function
g = b -> c
f = a -> b
fmap g f = (a ->) (g b) = (a ->) c =
g . f = fmap g f
```


```haskell
m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

m2 :: Integer -> Integer 
m2 = (+) <$> hurr <*> durr
m2 = (+) <$> (*2) <*> (+10)
m2 3 -- 19

--------
<*> = f (a -> b) -> f a -> f b

(+) <$> hurr = (+) . hurr
(+) <$> hurr <*> durr = \x -> (+ hurr x) <*> durr
                      = \x -> durr x + hurr x
```

idea of Reader: stringing functions together, awaiting one input from a shared environment.

#### Short Exercise

```haskell
import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs 

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (,) cap rev

tupled_do :: [Char] -> ([Char], [Char])
tupled_do = do
  a <- rev
  b <- cap
  return (a, b)

tupled_bind :: [Char] -> ([Char], [Char])
tupled_bind = rev >>= \x1 -> cap >>= \x2 -> return (x1, x2)

(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

### 22.3 This is Reader

### 22.4 Breaking down the Functor of functions

```haskell
instance Functor ((->) r) where
  fmap = (.)
```

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
fmap :: Functor f => (a -> b) -> f a -> f b

:: (b -> c) -> (a -> b) -> (a -> c)
:: (a -> b) -> f a -> f b

:: (b -> c) -> (-> a)  b -> (-> a) c
:: (b -> c) -> f b -> f c
```

### 22.5 But uh, Reader?

```haskell
newtype Reader r a =
  Reader { runReader :: r -> a }
  
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b 
  fmap f (Reader ra) = Reader $ f . ra

-- same as (.)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- see it?
\r -> f (ra r) 
\x -> f (g x)
```

#### Exercise

```haskell
ask :: Reader a a 
ask = Reader id
```

### 22.6 Functions have an Applicative too

```haskell
pure :: a -> f a
pure :: a -> (r -> a)

(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
```

#### Demonstrating the function applicative

#### Exercise

```haskell
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c 
myLiftA2 f a b= f <$> a <*> b
```

```haskell
asks :: (r -> a) -> Reader r a
asks f = Reader f
```

```haskell
{-# LANGUAGE InstanceSigs #-}

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a
  
  (<*>) :: Reader r (a -> b) -> 
           Reader r a -> 
           Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)
```

```haskell
getDogR :: Reader Person Dog
getDogR = Reader $ liftA2 Dog dogName address
```

### 22.7 The Monad of functions

#### The Monad instance

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) :: (->) r a -> (a -> (->) r b) -> (->) r b
(>>=) :: (r -> a) -> (a -> r -> b) -> r -> b

return :: Monad m => a -> ma
return :: a -> (->) r a
return :: a -> r -> a
```

#### Example uses of the Reader type

#### Exercise

```haskell
instance Monad (Reader r) where 
  return = pure
  
  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b 
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb $ ra r) $ r
```

### 22.8 Reader Monad by itself is kinda boring

### 22.9 You can change what comes below, but not above

### 22.10 You tend to see ReaderT, not Reader

### 22.11 Chapter Exercises

#### A warm-up stretch

#### Rewriting Shawty

### 22.12 Follow-up resources

1. Reader Monad; [All About Monads](https://wiki.haskell.org/All_About_Monads)

2. Reader Monad; Programming with Monads; Real World Haskell

## CHAPTER23. STATE

### 23.1 State

### 23.2 What is state?

state: originates in the circuit and automata theory.

### 23.3 Random numbers

```haskell
sg = mkStdGen 0
newSg = snd $ next sg
newSg2 = snd $ next newSg2 
...

-- specify the range
-- randomR
randomR (0, 3) newSg
```

### 23.4 The State newtype

```haskell
newtype State s a = State { runState :: s -> (a, s) }

newtype Reader r a = Reader { runReader :: r -> a }
```

isomorphic

```haskell
type Iso a b = (a -> b, b -> a) 

newtype Sum a = Sum { getSum :: a }

sumIsIsomorphicWithItsContents :: Iso a (Sum a) sumIsIsomorphicWithItsContents = (Sum, getSum)
```

```haskell
State :: (s -> (a, s)) -> State s a
runState :: State s a -> s -> (a, s)

randomR :: (...) => (a, a) -> g -> (a, g)
State { runState :: s -> (a, s) }
```

### 23.5 Throw down

```haskell
module RandomExample where 

import System.Random
-- Six-sided die
data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die 
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use this tactic _extremely_ sparingly.
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die) 
rollDieThreeTimes = do
  let s = mkStdGen 0
    (d1, s1) = randomR (1, 6) s 
    (d2, s2) = randomR (1, 6) s1 
    (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)
  

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

state :: Monad m => (s -> (a, s)) -> StateT s m a


rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))
```

#### Keep on rolling

#### Exercises

```haskell
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= n = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen 

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go sum acc@(count, xs) gen
          | sum >= n = acc
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1, (intToDie die):acc)
```

### 23.6 Write State for yourself

```haskell
newtype Moi s a = Moi { runMoi :: s -> (a, s) }
```
#### State Functor

```haskell
instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b 
  fmap f (Moi g) = Moi $ \s -> let (a, b) = g s
                               in (f a, b)
  
runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0
-- (1,0)
```

#### State Applicative

```haskell
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let fab = fst $ f s
                                        (a, b) = g s
                                    in (fab a, b)
```

### State Monad

```haskell
instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b 
  (Moi f) >>= g = Moi $ \s -> let a = fst $ f s
                                  ms = runMoi $ g a
                              in ms s
```

### 23.7 Get a coding job with one weird trick

### 23.8 Chapter exercises

### 23.9 Follow-up resources

1. [State Monad; All About Monads; Haskell Wiki](https://wiki.haskell.org/All_About_Monads)

2. [State Monad; Haskell Wiki](https://wiki.haskell.org/State_Monad)

3. Understanding Monads; Haskell Wikibook

## CHAPTER25. COMPOSINGTYPES

### 25.1 Composing types

### 25.2 Common functions as types

monad transformer: type constructor that takes a monad as an argument.

#### Identity is boring

```haskell
newtype Identity a = Identity { runIdentity :: a }
```

#### Compose

one structure wrapped around another:

```haskell
newtype Compose f g a = 
  Compose { getCompose :: f (g a) } 
  deriving (Eq, Show)
  
Compose [Just (1 :: Int), Nothing]
```

###  25.3 Two little functors sittin’ in a tree, L-I-F-T-I-N-G

lifting both f & g, using ```fmap . fmap```:

```haskell
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga
```

### 25.4 Twinplicative

#### GOTCHA! Exercise time

```haskell
{-# LANGUAGE InstanceSigs #-}

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose $ pure . pure
  
  
  (<*>) :: Compose f g (a -> b) 
        -> Compose f g a 
        -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ ((<*>) <$> f) <*> a
  
-- 
(<*>) <$> f g a <*> f g b
= f ((<*>) g a) <*> f g b
= f (g a <*> g b)
= f g (a b)
```


### 25.5 Twonad?

composing monad impossible:

### 25.6 Intermission: Exercises

#### Compose Foldable

```haskell
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga
```

#### Compose Traversable

```haskell
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga
```

#### And now for something completely different

```haskell
class Bifunctor p where
{-# MINIMAL bimap | first, second #-}

bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
bimap f g = first f . second g

first :: (a -> b) -> p a c -> p b c
first f = bimap f id

second :: (b -> c) -> p a b -> p a c
second = bimap id

-- 
data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)
  first f (Deux a b) = bimap f id (Deux a b) = Deux (f a) b
  second f (Deux a b) = bimap id f (Deux a b) = Deux a (f b)
  
--
data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

--
data Drei a b c = Drei a b c
  
instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

--
data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

--
data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap f g (SemiDrei a) = SemiDrei a
 --
data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

--
data Either a b = Left a | Right b

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)
```

### 25.7 Monad transformers

### 25.8 IdentityT

#### The bind breakdown

```haskell
newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where 
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x) 
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)
  
instance Monad Identity where return = pure
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

-- 
f :: a -> IdentityT m b
runIdentityT :: IdentityT f a -> f a
runIdentityT . f :: a -> f a
```

#### Implementing the bind, step by step

#### The essential extra of Monad transformers

### 25.9 Finding a pattern

### 25.10 Answers

## CHAPTER26. MONADTRANSFORMERS

### 26.1 Monad transformers

### 26.2 MaybeT

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x)) 
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma
  
-- 
(<*>) <$> mMf <*> mMa = m (Mf <*>) <*> mMa
                      = m (Maybe (f a))
                      
instance (Monad m) => Monad (MaybeT m) where
  return = pure
  
  (>>=) :: MaybeT m a
        -> (a -> MaybeT m b)
        -> MaybeT m b
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> returnMaybeT (f y)
```

#### MaybeT Monad instance

### 26.3 EitherT

```haskell
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea
  
instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT $ pure . pure
  (EitherT fab) <*> (EitherT mma) = 
    EitherT $ (<*>) <$> fab <*> mma
  
instance Monad m => Monad (EitherT e m) where
  return = pure
  
  (>>=) :: EitherT e m a
        -> (a -> EitherT e m b)
        -> EitherT e m b
  (EitherT ma) >>= f = 
    EitherT $ do
      v <- ma
      case ma of
        Left e -> return $ Left e
        Right a -> runEitherT $ f a
        

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e swapEitherT (EitherT x) = EitherT $ swapEitherT <$> x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fe _ (Left e) = fe e
either' _ fa (Right a) = fa a

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT fa fb (EitherT x) = x >>= either' fa fb
```

### 26.4 ReaderT

### 26.5 StateT

#### ReaderT, WriterT, StateT

```haskell
newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }
```

#### Correspondence between StateT and Parser

### 26.6 Types you probably don’t want to use

### 26.7 Recovering the ordinary type from the transformer

### 26.9 MonadTrans

### 26.10 MonadIO aka zoom-zoom

### 26.11 Monad transformers in use

### 26.13 Transform if you want to

### 26.14 Chapter Exercises

### 26.15 Answers

### 26.16 Follow-up resources

1. [Parallel and Concurrent Programming in Haskell; Simon Marlow;](http://chimera.labs.oreilly.com/books/1230000000929)

## CHAPTER27. NON-STRICTNESS 

### 27.1 Laziness

### 27.2 Observational Bottom Theory

#### Standards and obligations

### 27.3 Outside in, inside out

### 27.4 What does the other way look like?

#### seq and ye shall find

```haskell
seq :: a -> b -> b

seq bottom b = bottom
seq literallyAnythingNotBottom b = b
```

### 27.5 Call by name, call by need

### 27.6 Non-strict evaluation changes what we can do

### 27.7 Thunk Life