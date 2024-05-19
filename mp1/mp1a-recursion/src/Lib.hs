--- Getting Started
--- ===============
--- Relevant Files
--- --------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map" #-}

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding
  ( cycle,
    drop,
    foldl,
    foldr,
    iterate,
    map,
    repeat,
    replicate,
    reverse,
    take,
    zip,
    zipWith,
    (++),
  )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake n (x : xs) =
  if n <= 0
    then []
    else x : mytake (n - 1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n l@(x : xs) =
  if n <= 0
    then l
    else mydrop (n - 1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev l = aux l []
  where
    aux [] r = r
    aux (x : xs) r = aux xs (x : r)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] l = l
app l [] = l
app (x : xs) l = x : app xs l

-- don't forget to put the type declaration or you will lose points!
inclist :: (Num a) => [a] -> [a]
inclist [] = []
inclist (x : xs) = (x + 1) : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: (Num a) => [a] -> a
sumlist [] = 0
sumlist (x : xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a, b)]
myzip [] l = []
myzip l [] = []
myzip (x : xs) (y : ys) = (x, y) : myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs [] l = []
addpairs l [] = []
addpairs (x : xs) (y : ys) = (x + y) : addpairs xs ys

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = 0 : inclist nats

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: (Ord a) => a -> [a] -> [a]
add n [] = [n]
add n (x : xs)
  | n < x = n : x : xs
  | n == x = x : xs
  | otherwise = x : add n xs

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: (Ord a) => [a] -> [a] -> [a]
union [] l = l
union l [] = l
union (x : xs) (y : ys)
  | x < y = x : union xs (y : ys)
  | x == y = x : union xs ys
  | x > y = y : union (x : xs) ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: (Ord a) => [a] -> [a] -> [a]
intersect [] l = []
intersect l [] = []
intersect (x : xs) (y : ys)
  | x < y = intersect xs (y : ys)
  | x == y = x : intersect xs ys
  | x > y = intersect (x : xs) ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: (Ord a) => [a] -> [[a]]
powerset [] = [[]]
powerset (x : xs) =
  let ps = powerset xs
   in union (P.map (\s -> add x s) ps) ps

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: (Num a) => [a] -> [a]
inclist' a = P.map (\x -> x + 1) a

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' a = P.foldl (\x y -> x + y) 0 a
