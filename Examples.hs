{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.List
import Data.Char

{- multiline 
    comment -}
-- one line comment

--MultiIf
multiIf :: String -> String
multiIf t = 
    if  | t == "Hi" -> "HIII"
        | t == "Bye" -> "Bye"
        | otherwise -> " Hmmm"


--compairing
multiEx :: String -> String
multiEx t  
        | t == "Hi" = "HIII"
        | t == "Bye" = "Bye"
        | otherwise = " Hmmm"


--pattern maching
multiFunc :: String -> String
multiFunc "HI" = "HELLO"
multiFunc "Bo" = "BOOOO"
multiFunc _ = "So good"
--Rem: pattern mstching up -> down


--case .. of
alice :: String -> String
alice pattern = 
    case pattern of
        "HI" -> "Hello, King"
        _    -> "Emm...What?"


--let it be in
yourMark :: Int -> Int
yourMark mark = 
    let florinskiy_question = True
        new_mark = 2
    in 
        if  | florinskiy_question -> new_mark
            | otherwise -> mark


--new types
addToList :: String -> [String] -> [String]
addToList str dict = str : dict


--lambda functions
ten ::[Double]->[Double]
ten = map (\n->n*10)


--your own operator
(<+>) f g = \x -> f (g x)


--composition (map) with lambda
pretty :: [String] -> [String]
pretty = map (stars . big)
    where
        big = map toUpper
        stars = \x -> "* " ++ x ++ " *" 


main :: IO ()
main = 
    print (multiIf "Hi")

    --print (multiEx "wow")
    
    --print (multiFunc "a")

    --print (alice "HI")

    --print . yourMark $ 5

    {-print ( "incredible" `addToList` hosts )
        where hosts = ["amazing", "wow"]-}

    --print . ten $ [1.2, 0.2, 0.99]

    {-print (sq2 5) 
        where sq2 = \x -> x * x-}

    {-putStrLn ( head functions "Hi")
        where 
            functions = [   \x -> x ++ " Bro",
                            \x -> x ++ " Sister"
                        ]-}

    --print <+> yourMark $ 5

    --print . pretty $ ["c++", "haskell", "python"] 

    {-let
        fvalue = 3.14 / 2
        coeffs = [fvalue]
    in print $coeffs !! 0 -}