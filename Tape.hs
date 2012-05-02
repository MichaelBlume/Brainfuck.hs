module Tape (Tape, advance, retreat, increment
            ,decrement, readTape, writeTape, blankTape) where

data Tape = Tape [Int] Int [Int]

advance (Tape l c []) = Tape (c:l) 0 []
advance (Tape l c (r:rs)) = Tape (c:l) r rs

retreat (Tape [] c r) = Tape [] 0 (c:r)
retreat (Tape (l:ls) c r) = Tape ls l (c:r)

increment (Tape l c r) = Tape l (c + 1) r

decrement (Tape l c r) = Tape l (c - 1) r

writeTape (Tape l c r) nc = Tape l nc r

readTape (Tape _ c _) = c

blankTape = Tape [] 0 []

