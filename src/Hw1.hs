{- | CSE 114A: Intro to Haskell Assignment.
     Do not change the skeleton code!

     You may only replace the `error "TBD:..."` parts.

     For this assignment, you may use any library function on integers
     but only the following library functions on lists:

     length
     (++)
     (==)

 -}

module Hw1 where

import Prelude  hiding (replicate, sum, reverse)


-- | Reverse the order of elements in a list; in other words,
--   `listReverse [x1, x2, ..., xn]` returns `[xn, ..., x2, x1]`.
--
-- >>> listReverse [1,2,3,4]
-- [4,3,2,1]
--
-- >>> listReverse ["i", "want", "to", "ride", "my", "bicycle"]
-- ["bicycle", "my", "ride", "to", "want", "i"]
--
-- >>> listReverse []
-- []

listReverse :: [a] -> [a]
listReverse [] = []
listReverse (x:xs) = listReverse xs ++ [x]


-- | Determine whether a string is a palindrome (i.e. spelled the same
--   both backwards and forwards).
--
-- >>> palindrome "malayalam"
-- True
--
-- >>> palindrome "palindrome"
-- False
--
-- >>> palindrome ""
-- True

palindrome :: String -> Bool
palindrome w = w == listReverse w


-- | `digitsOfInt n` should return `[]` if `n` is not positive,
--    and otherwise returns the list of digits of `n` in the
--    order in which they appear in `n`.
--
-- >>> digitsOfInt 3124
-- [3, 1, 2, 4]
--
-- >>> digitsOfInt 352663
-- [3, 5, 2, 6, 6, 3]
--
-- >>> digitsOfInt -42
-- []

digitsOfInt :: Integer -> [Integer]
digitsOfInt n =
    if n <= 0
        then []
        else let digit = n `mod` 10
             in digitsOfInt (n `div` 10) ++ [digit]


-- | `digitsOfInts xs` should return a list containing all of the digits
--   for every number in `xs`, in the same order they appeared in `xs`.
--
-- >>> digitsOfInts [3124, 52, -42, 8]
-- [3, 1, 2, 4, 5, 2, 8]
--
-- >>> digitsOfInts []
-- []

digitsOfInts :: [Integer] -> [Integer]
digitsOfInts [] = []
digitsOfInts (n:ns) = digitsOfInt (n) ++ digitsOfInts (ns)


-- | Doubles every other integer in a list,
--   starting with the second element.
--
-- >>> doubleEveryOther [8,7,6,5]
-- [8,14,6,10]
--
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]
--
-- >>> doubleEveryOther []
-- []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : (2 * y) : doubleEveryOther zs

-- | Sum the elements of a list
--
-- >>> sumList [1, 2, 3, 4]
-- 10
--
-- >>> sumList [1, -2, 3, 5]
-- 7
--
-- >>> sumList [1, 3, 5, 7, 9, 11]
-- 36

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList (xs)


-- | Validate a credit card number
--
-- >>> validateCardNumber 4012888888881881
-- True
--
-- >>> validateCardNumber 4012888888881882
-- False

validateCardNumber :: Integer -> Bool
validateCardNumber n =
  let digits = digitsOfInt n                            
      reversedDigits = listReverse digits               
      doubledDigits = doubleEveryOther reversedDigits   
      allDigits = concatMap correctDigits doubledDigits 
      sumOfDigits = sumList allDigits                   
  in sumOfDigits `mod` 10 == 0                          

correctDigits :: Integer -> [Integer]
correctDigits d
  | d < 10    = [d]
  | otherwise = [d `div` 10, d `mod` 10] 
