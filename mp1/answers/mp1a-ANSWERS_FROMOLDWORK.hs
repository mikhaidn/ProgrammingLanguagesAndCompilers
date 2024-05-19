--- Getting Started
--- ===============

--- Relevant Files
--- --------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P
import Data.Bits (Bits(xor))
import GHC.Conc (pseq)

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake n [] = []
mytake n (x:xs) = if n <=0
    then []
    else x : mytake (n-1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop n [] = []
mydrop 0 x = x
mydrop n (x:xs) = if n <=0
    then x:xs
    else mydrop (n-1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev x = revhelp [] x
    where revhelp x [] = x
          revhelp revlist (x:xs) = revhelp (x:revlist) xs

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] [] = []
app [] x = x
app x [] = x
app (x:xs) y= x : app xs y

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = x+1 : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist x = sumlistHelp x 0
    where sumlistHelp [] n = n
          sumlistHelp (x:xs) n = sumlistHelp xs x+n

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip [] [] = []
myzip [] x = []
myzip x [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs x y = addpairsHelp (myzip x y)
    where addpairsHelp [] = []
          addpairsHelp ((x,y):xs) = x+y : addpairsHelp xs

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = 0 : addpairs nats ones
--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)


--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add n [] = [n]
add n (x:xs) -- "use guards" auto-suggested this syntax!
  | n < x = n:x:xs
  | n == x = x:xs
  | otherwise = x : add n xs

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union [] x = x
union x [] = x
union (x:xs) (y:ys)
  | x < y = x : union xs (y:ys)
  | x == y = x : union xs ys
  | otherwise = y : union (x:xs) ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] [] = []
intersect [] x = []
intersect x [] = []
intersect (x:xs) (y:ys)
  | x < y = intersect xs (y:ys)
  | x == y = x : intersect xs ys
  | otherwise = intersect (x:xs) ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] `union` powerset xs

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' n = P.map (+1) n

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' n = P.foldr (+) 0 n