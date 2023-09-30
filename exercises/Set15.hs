module Set15 where

import Control.Applicative
import Data.Char
import Data.Maybe
import Examples.Validation
import Mooc.Todo
import Text.Read (readMaybe)

------------------------------------------------------------------------------
-- Ex 1: Sum two Maybe Int values using Applicative operations (i.e.
-- liftA2 and pure). Don't use pattern matching.
--
-- Examples:
--  sumTwoMaybes (Just 1) (Just 2)  ==> Just 3
--  sumTwoMaybes (Just 1) Nothing   ==> Nothing
--  sumTwoMaybes Nothing Nothing    ==> Nothing

sumTwoMaybes :: Maybe Int -> Maybe Int -> Maybe Int
sumTwoMaybes a b = liftA2 (+) a b

------------------------------------------------------------------------------
-- Ex 2: Given two lists of words, xs and ys, generate all statements
-- of the form "x is [not] y". Use Applicative
-- operations like liftA2!
--
-- The order of the results doesn't matter.
--
-- Examples:
--  statements ["beauty"] ["suffering"]
--    ==> ["beauty is suffering","beauty is not suffering"]
--  statements ["beauty","code"] ["suffering","life"]
--    ==> ["beauty is suffering","beauty is life",
--         "beauty is not suffering","beauty is not life",
--         "code is suffering","code is life",
--         "code is not suffering","code is not life"]

statements :: [String] -> [String] -> [String]
statements xs ys = (liftA2 combineWithIsEquals xs ys) ++ (liftA2 combineWithIsNotEquals xs ys)
  where
    combineWithIsEquals x y = x ++ " is " ++ y
    combineWithIsNotEquals x y = x ++ " is not " ++ y

------------------------------------------------------------------------------
-- Ex 3: A simple calculator with error handling. Given an operation
-- (negate or double) and a number, as strings, compute the result.
-- Return Nothing for an unknown operation or invalid number.
--
-- Use Applicative operations, don't use pattern matching.
--
-- Hint: remember the function readMaybe
--
-- Examples:
--  calculator "negate" "3"   ==> Just (-3)
--  calculator "double" "7"   ==> Just 14
--  calculator "doubl" "7"    ==> Nothing
--  calculator "double" "7x"  ==> Nothing

calculator :: String -> String -> Maybe Int
calculator op number = parseOp op <*> (readMaybe number :: Maybe Int)
  where
    parseOp "negate" = Just negate
    parseOp "double" = Just $ (*) 2
    parseOp _ = Nothing

------------------------------------------------------------------------------
-- Ex 4: Safe division. Implement the function validateDiv that
-- divides two integers, but returns an error ("Division by zero!") if
-- the divisor is zero.
--
-- NB! The constructors of Validation are not exported, so you can't
-- pattern match on Validation, you must use the Applicative methods
-- and the invalid and check functions.
--
-- Examples:
--  validateDiv 6 2 ==> Ok 3
--  validateDiv 6 0 ==> Errors ["Division by zero!"]
--  validateDiv 0 3 ==> Ok 0

validateDiv :: Int -> Int -> Validation Int
validateDiv dividend divisor = check (divisor /= 0) "Division by zero!" (dividend `div` divisor)

------------------------------------------------------------------------------
-- Ex 5: Validating street addresses. A street address consists of a
-- street name, a street number, and a postcode.
--
-- Implement the function validateAddress which constructs an Address
-- value if the input is valid:
--

-- * Street length should be at most 20 characters

--   (if not, error "Invalid street name")

-- * Street number should only contain digits

--   (if not, error "Invalid street number")

-- * Postcode should be exactly five digits long

--   (if not, error "Invalid postcode")
--
-- Examples:
--  validateAddress "Haskell road" "35" "13337"
--    ==> Ok (Address "Haskell road" "35" "13337")
--  validateAddress "Haskell road" "35a" "13337"
--    ==> Errors ["Invalid street number"]
--  validateAddress "Haskell road" "35a" "1333"
--    ==> Errors ["Invalid street number","Invalid postcode"]
--  validateAddress "Haskeller's favourite road" "35a" "1333"
--    ==> Errors ["Invalid street name","Invalid street number","Invalid postcode"]

data Address = Address String String String
  deriving (Show, Eq)

validateAddress :: String -> String -> String -> Validation Address
validateAddress streetName streetNumber postCode =
  let checkedStreetName = check (length streetName <= 20) ("Invalid street name") streetName
      checkedStreetNumber = check (all isDigit streetNumber) ("Invalid street number") streetNumber
      checkedPostCode = check (all isDigit postCode && length postCode == 5) ("Invalid postcode") postCode
   in liftA3 Address checkedStreetName checkedStreetNumber checkedPostCode

------------------------------------------------------------------------------
-- Ex 6: Given the names, ages and employment statuses of two
-- persons, wrapped in Applicatives, return a list of two Person
-- values, wrapped in an applicative.
--
-- Examples:
--  twoPersons (Just "Clarice") (Just 35) (Just True) (Just "Hannibal") (Just 50) (Just False)
--    ==> Just [Person "Clarice" 35 True,Person "Hannibal" 50 False]
--  twoPersons (Just "Clarice") (Just 35) (Just True) (Just "Hannibal") Nothing (Just False)
--    ==> Nothing
--  twoPersons ["Clarice"] [25,35] [True] ["Hannibal"] [50] [False]
--    ==> [[Person "Clarice" 25 True,Person "Hannibal" 50 False],
--         [Person "Clarice" 35 True,Person "Hannibal" 50 False]]

data Person = Person String Int Bool
  deriving (Show, Eq)

twoPersons ::
  Applicative f =>
  f String ->
  f Int ->
  f Bool ->
  f String ->
  f Int ->
  f Bool ->
  f [Person]
twoPersons name1 age1 employed1 name2 age2 employed2 =
  let person1 = mkPerson name1 age1 employed1
      person2 = mkPerson name2 age2 employed2
   in liftA2 (\a b -> [a, b]) person1 person2

mkPerson :: Applicative f => f String -> f Int -> f Bool -> f Person
mkPerson name age employed = Person <$> name <*> age <*> employed

------------------------------------------------------------------------------
-- Ex 7: Validate a String that's either a Bool or an Int. The return
-- type of the function uses Either Bool Int to be able to represent
-- both cases. Use <|> to combine two validators and to produce two
-- errors if the value is not an Int or a Bool.
--
-- Hint: remember readMaybe
--
-- PS. The tests won't test special cases of Int literals like hexadecimal
-- (0x3a) or octal (0o14).
--
-- Examples:
--  boolOrInt "True"    ==> Ok (Left True)
--  boolOrInt "13"      ==> Ok (Right 13)
--  boolOrInt "13.2"    ==> Errors ["Not a Bool","Not an Int"]
--  boolOrInt "Falseb"  ==> Errors ["Not a Bool","Not an Int"]

boolOrInt :: String -> Validation (Either Bool Int)
boolOrInt str = (parseBool str) <|> (parseInt str)
  where
    parseBool str = check (isJust $ readBool str) "Not a Bool" (Left $ fromJust $ readBool str)
    readBool str = readMaybe str :: Maybe Bool
    parseInt str = check (isJust $ readInt str) "Not an Int" (Right $ fromJust $ readInt str)
    readInt str = readMaybe str :: Maybe Int
    isJust (Just _) = True
    isJust Nothing = False

------------------------------------------------------------------------------
-- Ex 8: Improved phone number validation. Implement the function
-- normalizePhone that, given a String:
--

-- * removes all spaces from the string

-- * checks that there are at most 10 remaining characters

-- * checks that all remaining characters are digits, and logs an

--   error for every nonvalid character

-- * returns the string, stripped of whitespace, if no errors

--
-- Examples:
--  normalizePhone "123 456 78" ==> Ok "12345678"
--  normalizePhone "123 4x6 78"
--    ==> Errors ["Invalid character: x"]
--  normalizePhone "123 4x6 7y"
--    ==> Errors ["Invalid character: x","Invalid character: y"]
--  normalizePhone "123 4x6 7y 999"
--    ==> Errors ["Too long","Invalid character: x","Invalid character: y"]
--  normalizePhone "123 456 78 999"
--    ==> Errors ["Too long"]

normalizePhone :: String -> Validation String
normalizePhone str = validateLength str *> traverse validateDigit (removeSpaces str)

validateLength str = check (length (removeSpaces str) <= 10) "Too long" str

validateDigit d = check (isDigit d) ("Invalid character: " ++ [d]) d

removeSpaces str = filter (\c -> c /= ' ') str

------------------------------------------------------------------------------
-- Ex 9: Parsing expressions. The Expression type describes an
-- arithmetic expression that has an operator (+ or -) and two
-- arguments that can be either numbers or single-letter variables.
-- The operator and the arguments are always separated by spaces. Here
-- are some examples of expressions like this: 1 + 2, y + 7, z - w
--
-- Implement the function parseExpression that uses the Validation
-- applicative to convert strings like "y + 7" to Expression values
-- like Plus (Variable 'y') (Number 7).
--
-- The parser should produce the following errors:
--  * For operators other than + or -: "Unknown operator: %"
--  * For variables that aren't single letters: "Invalid variable: xy"
--  * For arguments that aren't numbers: "Invalid number: 1x" --
--  * For expressions that don't consist of three words:
--    "Invalid expression: 1 + 2 +"
--    "Invalid expression: 1 -"
--
-- Hint: The functions `words` and `isAlpha`
--
-- Hint: If you have problems with the ordering of errors, remember
-- that Validation collects errors left-to-right!
--
-- Examples:
--  parseExpression "1 + 2" ==> Ok (Plus (Number 1) (Number 2))
--  parseExpression "z - A" ==> Ok (Minus (Variable 'z') (Variable 'A'))
--  parseExpression "1 * 2" ==> Errors ["Unknown operator: *"]
--  parseExpression "1 + 2x"
--    ==> Errors ["Invalid number: 2x","Invalid variable: 2x"]
--  parseExpression ". % 2x"
--    ==> Errors ["Unknown operator: %",
--                "Invalid number: .","Invalid variable: .",
--                "Invalid number: 2x","Invalid variable: 2x"]

data Arg = Number Int | Variable Char
  deriving (Show, Eq)

data Expression = Plus Arg Arg | Minus Arg Arg
  deriving (Show, Eq)

checkNum num = check (isPositiveInt num) ("Invalid number: " ++ num) num

checkVar var = check (isAlphaChar var) ("Invalid variable: " ++ var) var

checkOp op = check (isKnownOp op) ("Unknown operator: " ++ op) op

checkArg str = (checkNum str) <|> (checkVar str)

isPositiveInt str = all isDigit str

isAlphaChar str = length str == 1 && all isAlpha str

isKnownOp "+" = True
isKnownOp "-" = True
isKnownOp _ = False

parseArg :: String -> Arg
parseArg str =
  if (isPositiveInt str)
    then Number (fromJust (readMaybe str :: Maybe Int))
    else Variable (head str)

parseOp :: String -> Validation String
parseOp str = checkOp str

parseExpression :: String -> Validation Expression
parseExpression str = case words str of
  arg1 : op : arg2 : [] -> mkExpression <$> (checkArg arg1) <*> (checkOp op) <*> (checkArg arg2)
  otherwise -> invalid ("Invalid expression: " ++ str)

mkExpression :: String -> String -> String -> Expression
mkExpression arg1 op arg2
  | op == "+" = Plus (parseArg arg1) (parseArg arg2)
  | otherwise = Minus (parseArg arg1) (parseArg arg2)

------------------------------------------------------------------------------
-- Ex 10: The Priced T type tracks a value of type T, and a price
-- (represented by an Int). Implement the Functor and Applicative
-- instances for Priced. They should work like this:
--

-- * Transforming a Priced value with fmap keeps the price the same

-- * pure should create a value with price 0

-- * liftA2 should sum the prices of the things to be combined

--
-- Examples:
--  fmap reverse (Priced 3 "abc")
--    ==> Priced 3 "cba"
--  liftA2 (*) (pure 2) (pure 3) :: Priced Int
--    ==> Priced 0 6
--  liftA2 (+) (Priced 1 3) (Priced 1 5)
--    ==> Priced 2 8
--  traverse (\x -> Priced (length x) x) ["abc","de","f"]
--    ==> Priced 6 ["abc","de","f"]

data Priced a = Priced Int a
  deriving (Show, Eq)

instance Functor Priced where
  fmap f (Priced price value) = Priced price (f value)

instance Applicative Priced where
  pure value = Priced 0 value
  liftA2 f (Priced price1 val1) (Priced price2 val2) = Priced (price1 + price2) (f val1 val2)

------------------------------------------------------------------------------
-- Ex 11: This and the next exercise will use a copy of the
-- Applicative type class called MyApplicative. MyApplicative lacks
-- the Functor requirement that Applicative has, and also the <*> type
-- class method. You'll get to implement them instead.
--
-- First you'll reimplement <*> using liftA2. In practical terms,
-- implement the operator <#> that works like <*>, using myPure and
-- myLiftA2.
--
-- As long as you get the types right, your implementation is pretty
-- much guaranteed to be correct.
--
-- Examples:
--  Just succ <#> Just 2      ==> Just 3
--  Nothing <#> Just 2        ==> Nothing
--  [(*2),(+1)] <#> [10,100]  ==> [20,200,11,101]

class MyApplicative f where
  myPure :: a -> f a
  myLiftA2 :: (a -> b -> c) -> f a -> f b -> f c

-- Some instances for testing:
instance MyApplicative Maybe where
  myPure = pure
  myLiftA2 = liftA2

instance MyApplicative [] where
  myPure = pure
  myLiftA2 = liftA2

(<#>) :: MyApplicative f => f (a -> b) -> f a -> f b
f <#> fc = myLiftA2 (\a b -> a b) f fc

------------------------------------------------------------------------------
-- Ex 12: Reimplement fmap using liftA2 and pure. In practical terms,
-- implement the function myFmap below using the methods myPure and
-- myLiftA2 from the type class MyApplicative.
--
-- As long as you get the types right, your implementation is pretty
-- much guaranteed to be correct. However, this time there are a
-- couple of different possible implementations!
--
-- Examples:
--  myFmap negate (Just 1) ==> Just (-1)
--  myFmap negate Nothing  ==> Nothing
--  myFmap negate [1,2,3]  ==> [-1,-2,-3]

myFmap :: MyApplicative f => (a -> b) -> f a -> f b
myFmap func applicative = myLiftA2 (\a b -> a b) (myPure func) applicative

------------------------------------------------------------------------------
-- Ex 13: Given a function that returns an Alternative value, and a
-- list, try the function on all the elements in the list and produce
-- any successes.
--
-- Hint: traverse won't help you since it succeeds only if all the
-- calls succeed. You need to use <|>.
--
-- Examples:
--
--   The Maybe Applicative returns the first success:
--     tryAll (\x -> if x>0 then pure x else empty) [0,3,2] :: Maybe Int
--       ==> Just 3
--     tryAll (\x -> if x>0 then pure x else empty) [0,-1,0] :: Maybe Int
--       ==> Nothing
--   The list Applicative returns all successes:
--     tryAll (\x -> if x>0 then pure x else empty) [0,3,2] :: [Int]
--       ==> [3,2]
--   The Validation Applicative returns the first success or all errors:
--     tryAll (\x -> if x>0 then pure x else invalid "zero") [0,3,2]
--       ==> Ok 3
--     tryAll (\x -> if x>0 then pure x else invalid "zero") [0,0,0]
--       ==> Errors ["zero","zero","zero"]

tryAll :: Alternative f => (a -> f b) -> [a] -> f b
tryAll f [] = empty
tryAll f (a : as) = f a <|> (tryAll f as)

------------------------------------------------------------------------------
-- Ex 14: Here's the type `Both` that expresses the composition of
-- functors. Here are some example values and types:
--
--   Both (Just [True])                    :: Both Maybe [] Bool
--   Both [Just True, Nothing, Just False] :: Both [] Maybe Bool
--   Both [[True,False],[]]                :: Both [] [] Bool
--
--   Both (Ok (Just "value"))       :: Both Validation Maybe String
--   Both (Just (Errors ["wrong"])) :: Both Maybe Validation a
--
-- Implement a Functor instance for Both f g, given that f and g are
-- both Functors.
--
-- Examples:
--  fmap not (Both (Just [True]))     ==> Both (Just [False])
--  fmap not (Both [Nothing])         ==> Both [Nothing]
--  fmap (+1) (Both [[1,2,3],[4,5]])  ==> Both [[2,3,4],[5,6]]

newtype Both f g a = Both (f (g a))
  deriving (Show)

instance (Functor f, Functor g) => Functor (Both f g) where
  fmap mapFunc (Both nestedFunctor) = Both (nestedFMap mapFunc nestedFunctor)
    where
      nestedFMap = fmap . fmap

------------------------------------------------------------------------------
-- Ex 15: The composition of two Applicatives is also an Applicative!
-- Implement the instance Applicative (Both f g) (given that f and g
-- are already Applicatives).
--
-- Again, there's only one way to implement this that gets the types
-- right.
--
-- Examples:
--  pure 1 :: Both Maybe [] Int
--    ==> Both (Just [1])
--  liftA2 (+) (Both (Just [10,100])) (Both (Just [1,2]))
--    ==> Both (Just [11,12,101,102])
--  liftA2 (+) (Both (Just [10,100])) (Both Nothing)
--    ==> Both Nothing
--  liftA2 (&&) (Both (Just (invalid "err"))) (Both (Just (pure True)))
--    ==> Both (Just (Errors ["err"]))
--  liftA2 (&&) (Both (Just (invalid "err"))) (Both (Just (invalid "umm")))
--    ==> Both (Just (Errors ["err","umm"]))
--  liftA2 (+) (Both [pure 1, invalid "fail 1"])
--             (Both [pure 10, pure 100, invalid "fail 2"])
--    ==> Both [Ok 11,Ok 101,Errors ["fail 2"],
--              Errors ["fail 1"],Errors ["fail 1"],
--              Errors ["fail 1","fail 2"]]

instance (Applicative f, Applicative g) => Applicative (Both f g) where
  pure value = Both $ pure (pure value)
  liftA2 f (Both x) (Both y) = Both $ (liftA2 . liftA2) f x y
