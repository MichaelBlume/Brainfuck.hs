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

advance (Tape l c []) = Tape (c:l) 0 []
advance (Tape l c (r:rs)) = Tape (c:l) r rs

retreat (Tape [] c r) = Tape [] 0 (c:r)
retreat (Tape (l:ls) c r) = Tape ls l (c:r)

increment (Tape l c r) = Tape l (c + 1) r

decrement (Tape l c r) = Tape l (c - 1) r

writeTape nc tape = tape {val = nc}

readTape = val

blankTape = Tape [] 0 []

