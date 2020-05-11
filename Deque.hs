module Main where

import Prelude hiding (head, tail, last, init)

class Deque q where
    empty   :: q a
    isEmpty :: q a -> Bool

    cons    :: a -> q a -> q a
    head    :: q a -> a
    tail    :: q a -> q a
    
    snoc    :: q a -> a -> q a
    last    :: q a -> a
    init    :: q a -> q a

data BankersDeque a = BD Int [a] Int [a] deriving (Show)

c = 3

check lenf f lenr r =
    if lenf > c*lenr + 1 then
        let i  = (lenf + lenr) `div` 2
            j  = lenf + lenr - i
            f' = take i f
            r' = r ++ reverse(drop i f)
        in BD i f' j r'
    else if lenr > c*lenf + 1 then
            let j  = (lenf + lenr) `div` 2
                i  = lenf + lenr - j
                r' = take j r
                f' = f ++ reverse(drop j r)
            in BD i f' j r'
        else BD lenf f lenr r

instance Deque BankersDeque where
    empty = BD 0 [] 0 []

    isEmpty (BD lenf f    lenr r)      = (lenr+lenf == 0)
    
    --insert forward
    cons x  (BD lenf f    lenr r)      = check(lenf + 1) (x:f) lenr r

    head (BD lenf []      lenr r)      = error "empty deque"
    head (BD lenf (x:f')  lenr r)      = x

    tail (BD lenf []      lenr r)      = error "empty deque"
    tail (BD lenf (x:f')  lenr r)      = check(lenf - 1) f' lenr r

    --insert back
    snoc (BD lenf f       lenr r) x    = check lenf f (lenr + 1) (x:r)

    last (BD lenf f       lenr [])     = error "empty deque"
    last (BD lenf f       lenr (x:r')) = x

    --first elements
    init (BD lenf f       lenr [])     = error "empty deque"
    init (BD lenf f       lenr (x:r')) = check lenf f (lenr - 1) r'

main :: IO ()
main = let q = BD 4 [1, 2, 3, 4] 2 [3, 2]
    in print $ last q