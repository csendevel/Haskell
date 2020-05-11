module Main where

import  Prelude  hiding  (head,  tail)

--typeclass (interface be like)
class Queue q where
    empty :: q a
    isEmpty :: q a -> Bool
    snoc :: q a -> a -> q a
    head :: q a -> a
    tail :: q a -> q a

--your datatype       constructor arguments magic
data BankersQueue a = BQ Int [a] Int [a] deriving (Show)

check lenf f lenr r =
    if lenr <= lenr
        then BQ lenf f lenr r
        else BQ (lenf + lenr) (f ++ reverse r) 0 []

--typeclass implementation
instance Queue BankersQueue where
    empty =  BQ 0 [] 0 []
    isEmpty (BQ lenf f     lenr r) = (lenf == 0)
    
    --insert new element
    snoc    (BQ lenf f     lenr r) x = check lenf f (lenr + 1) (x:r)

    head    (BQ lenf []    lenr r) = error "empty queue"
    head    (BQ lenf (x:f) lenr r) = x

    tail    (BQ lenf []    lenr r) = error "empty queue"
    tail    (BQ lenf (x:f') lenr r) = check (lenf - 1) f' lenr r


main :: IO ()
main = let q = BQ 3 [2, 1, 3] 4 [4, 4, 4, 4]
    in --print . head . tail $ snoc q 3
        print $ snoc q 3