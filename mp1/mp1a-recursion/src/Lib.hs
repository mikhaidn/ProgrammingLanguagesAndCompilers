--- Getting Started
--- ===============

--- Relevant Files
--- --------------

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

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake n (x:xs) = if n <=0
  then []
  else x: mytake (n-1) xs 
--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a] 
mydrop _ [] = []
mydrop n l@(x:xs) = if n <=0
    then l
    else mydrop (n-1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev l = aux l []
  where aux [] r = r
        aux (x:xs) r = aux xs (x:r)


--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] l = l
app l [] = l
app (x:xs) l = x : app xs l
-- don't forget to put the type declaration or you will lose points!
inclist = undefined

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist = undefined

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip = undefined

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs = undefined

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones = undefined

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats = undefined

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib = undefined

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add = undefined

--- ### union

-- don't forget to put the type declaration or you will lose points!
union = undefined

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect = undefined

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset = undefined

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' = undefined

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' = undefined
