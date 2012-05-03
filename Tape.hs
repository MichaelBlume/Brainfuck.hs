module Tape where

import Control.Monad.State

data Tape = Tape [Int] Int [Int]

advance (Tape l c []) = Tape (c:l) 0 []
advance (Tape l c (r:rs)) = Tape (c:l) r rs

retreat (Tape [] c r) = Tape [] 0 (c:r)
retreat (Tape (l:ls) c r) = Tape ls l (c:r)

increment (Tape l c r) = Tape l (c + 1) r

decrement (Tape l c r) = Tape l (c - 1) r

writeTape nc (Tape l c r) = Tape l nc r

readTape (Tape _ c _) = c

blankTape = Tape [] 0 []

type BFState a = State (Tape, Int) a

modTape :: (Tape -> Tape) -> BFState ()
modTape tf = do
  (tape, ip) <- get
  put (tf tape, ip)

readTapeM :: BFState Int
readTapeM = do
  (tape, _ip) <- get
  return $ readTape tape

getIP :: BFState Int
getIP = do
  (_tape, ip) <- get
  return ip

setIP :: Int -> BFState ()
setIP n = do
  (tape, _ip) <- get
  put (tape, n)

incIP :: BFState ()
incIP = do
  (tape, ip) <- get
  put (tape, ip + 1)

