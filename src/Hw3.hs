{- | CSE 130: All about fold.

     For this assignment, you may use the following library functions:

     length
     append (++)
     map
     foldl'
     foldr
     unzip
     zip
     reverse

  Use www.haskell.org/hoogle to learn more about the above.

  Do not change the skeleton code! The point of this assignment
  is to figure out how the functions can be written this way
  (using fold). You may only replace the `error "TBD:..."` terms.

-}

{-

Cited helpfull source matterial for clearity:

 -- Used resources from http://learnyouhaskell.com/chapters.
 -- Used resources from https://www.schoolofhaskell.com
 -- Used resources form https://en.wikibooks.org/wiki/Haskell/Control_structures
 -- Used resources from the chapters 1, 2, 4 as suggested on piazza. 
 -- Also got help form TA's in section and group disscussion.
 -- Also got help form MSI instructor and group disscussion.

-}

module Hw3 where

import Prelude hiding (replicate, sum)
import Data.List (foldl')

foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft = foldl'

--------------------------------------------------------------------------------
-- | sqSum [x1, ... , xn] should return (x1^2 + ... + xn^2)
--
-- >>> sqSum []
-- 0
--
-- >>> sqSum [1,2,3,4]
-- 30
--
-- >>> sqSum [(-1), (-2), (-3), (-4)]
-- 30

sqSum :: [Int] -> Int
sqSum xs = foldLeft f base xs
  where
   f a x = a + x * x
   base  = 0
--   f a x = error "TBD: sqSum f"
--   base  = error "TBD: sqSum base"

--------------------------------------------------------------------------------
-- | `pipe [f1,...,fn] x` should return `f1(f2(...(fn x)))`
--
-- >>> pipe [] 3
-- 3
--
-- >>> pipe [(\x -> x+x), (\x -> x + 3)] 3
-- 12
--
-- >>> pipe [(\x -> x * 4), (\x -> x + x)] 3
-- 24

pipe :: [(a -> a)] -> (a -> a)
pipe fs   = foldLeft f base fs
  where
    f a x fn = a (x fn)
    base fb = fb
--    f a x = error "TBD: pipe: f"
--    base  = error "TBD: pipe: base"

--------------------------------------------------------------------------------
-- | `sepConcat sep [s1,...,sn]` returns `s1 ++ sep ++ s2 ++ ... ++ sep ++ sn`
--
-- >>> sepConcat "---" []
-- ""
--
-- >>> sepConcat ", " ["foo", "bar", "baz"]
-- "foo, bar, baz"
--
-- >>> sepConcat "#" ["a","b","c","d","e"]
-- "a#b#c#d#e"

sepConcat :: String -> [String] -> String
sepConcat sep []     = ""
sepConcat sep (x:xs) = foldLeft f base l
  where
    f a x            = a ++ sep ++ x
    base             = x
    l                = xs

--    f a x            = error "TBD:sepConcat:f"
--    base             = error "TBD:sepConcat:base"
--    l                = error "TBD:l"

intString :: Int -> String
intString = show

--------------------------------------------------------------------------------
-- | `stringOfList pp [x1,...,xn]` uses the element-wise printer `pp` to
--   convert the element-list into a string:
--
-- >>> stringOfList intString [1, 2, 3, 4, 5, 6]
-- "[1, 2, 3, 4, 5, 6]"
--
-- >>> stringOfList (\x -> x) ["foo"]
-- "[foo]"
--
-- >>> stringOfList (stringOfList show) [[1, 2, 3], [4, 5], [6], []]
-- "[[1, 2, 3], [4, 5], [6], []]"

stringOfList :: (a -> String) -> [a] -> String
stringOfList f xs = "[" ++ sepConcat ", " (map f xs) ++ "]"
--stringOfList f xs = error "TBD:stringOfList"

--------------------------------------------------------------------------------
-- | `clone x n` returns a `[x,x,...,x]` containing `n` copies of `x`
--
-- >>> clone 3 5
-- [3,3,3,3,3]
--
-- >>> clone "foo" 2
-- ["foo", "foo"]

clone :: a -> Int -> [a]
clone x n = if 0 >= n then [] else x:(clone x (n-1))
--clone x n = error "TBD:clone"

type BigInt = [Int]

--------------------------------------------------------------------------------
-- | `padZero l1 l2` returns a pair (l1', l2') which are just the input lists,
--   padded with extra `0` on the left such that the lengths of `l1'` and `l2'`
--   are equal.
--
-- >>> padZero [9,9] [1,0,0,2]
-- [0,0,9,9] [1,0,0,2]
--
-- >>> padZero [1,0,0,2] [9,9]
-- [1,0,0,2] [0,0,9,9]

padZero :: BigInt -> BigInt -> (BigInt, BigInt)
padZero l1 l2 = if (length l1 > length l2) then padZero l1 (0:l2) else if (length l1 < length l2) then padZero (0:l1) l2 else (l1, l2)
--padZero l1 l2 = error "TBD:padZero"

--------------------------------------------------------------------------------
-- | `removeZero ds` strips out all leading `0` from the left-side of `ds`.
--
-- >>> removeZero [0,0,0,1,0,0,2]
-- [1,0,0,2]
--
-- >>> removeZero [9,9]
-- [9,9]
--
-- >>> removeZero [0,0,0,0]
-- []

removeZero :: BigInt -> BigInt
removeZero [] = []
removeZero (x:xs) = if 0 == x then removeZero xs else (x:xs)
--removeZero ds = error "TBD:removeZero"


--------------------------------------------------------------------------------
-- | `bigAdd n1 n2` returns the `BigInt` representing the sum of `n1` and `n2`.
--
-- >>> bigAdd [9, 9] [1, 0, 0, 2]
-- [1, 1, 0, 1]
--
-- >>> bigAdd [9, 9, 9, 9] [9, 9, 9]
-- [1, 0, 9, 9, 8]

bigAdd :: BigInt -> BigInt -> BigInt
bigAdd l1 l2     = removeZero res
  where
    (l1', l2')   = padZero l1 l2
    res          = foldLeft f base args
--    f a x        = error "TBD:bigAdd:f"
--    base         = error "TBD:bigAdd:base"
--    args         = error "TBD:bigAdd:args"
 

    f a (x1, x2) = ( if((x1 + x2) > 9) then reverse (g (reverse a)) else a) ++ [(mod (x1 + x2) 10)] 
       where 
          g []     = [1]
          g (x:xs) = if (x == 9) then ( 0:(g xs) ) else ((1 + x): xs) 

    base         = [] 
    args         = zip l1' l2'


--    base         = 0
--    base         = (0, [])
--    args         = (reverse l1) zip (reverse l2)


--------------------------------------------------------------------------------
-- | `mulByDigit i n` returns the result of multiplying
--   the digit `i` (between 0..9) with `BigInt` `n`.
--
-- >>> mulByDigit 9 [9,9,9,9]
-- [8,9,9,9,1]

mulByDigit :: Int -> BigInt -> BigInt
mulByDigit i n = if 0 >= i then [0] else if 1 == i then n else bigAdd (mulByDigit (i-1) n) n
--mulByDigit i n = error "TBD:mulByDigit"

--------------------------------------------------------------------------------
-- | `bigMul n1 n2` returns the `BigInt` representing the product of `n1` and `n2`.
--
-- >>> bigMul [9,9,9,9] [9,9,9,9]
-- [9,9,9,8,0,0,0,1]
--
-- >>> bigMul [9,9,9,9,9] [9,9,9,9,9]
-- [9,9,9,9,8,0,0,0,0,1]

bigMul :: BigInt -> BigInt -> BigInt
bigMul l1 l2 = res
  where
    (_, res) = foldLeft f base args
--    f a x    = error "TBD:bigMul:f"
--    base     = error "TBD:bigMul:base"
--    args     = error "TBD:bigMul:args"

    f a x     = ( next, (bigAdd m (snd a)))
       where
          next = ((fst a) + 1)
          m    = ((mulByDigit x l2) ++ (clone 0 (fst a)))

    base      = (0, []) 
    args      = l1

