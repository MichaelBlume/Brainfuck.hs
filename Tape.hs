module Tape
( Tape ()
, advance
, retreat
, increment
, decrement
, writeTape
, readTape
, blankTape
) where

data Tape = Tape {
    left :: [Int],
    val :: Int,
    right :: [Int]}

advance :: Tape -> Tape
advance (Tape l c []) = Tape (c:l) 0 []
advance (Tape l c (r:rs)) = Tape (c:l) r rs

retreat :: Tape -> Tape
retreat (Tape [] c r) = Tape [] 0 (c:r)
retreat (Tape (l:ls) c r) = Tape ls l (c:r)

increment :: Tape -> Tape
increment (Tape l c r) = Tape l (c + 1) r

decrement :: Tape -> Tape
decrement (Tape l c r) = Tape l (c - 1) r

writeTape :: Int -> Tape -> Tape
writeTape nc tape = tape {val = nc}

readTape :: Tape -> Int
readTape = val

blankTape :: Tape
blankTape = Tape [] 0 []

