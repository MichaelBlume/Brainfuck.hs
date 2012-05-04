module BFState
( modTape
, readTapeM
, getIP
, setIP
, incIP
, blankState
, tapeZero
, BFState ()
) where

import RSI
import Tape
import Control.Monad

type BFState = (Tape, Int)

modTape :: (Tape -> Tape) -> RSI read BFState ()
modTape tf = do
  (tape, ip) <- get
  put (tf tape, ip)

readTapeM :: RSI read BFState Int
readTapeM = do
  (tape, _ip) <- get
  return $ readTape tape

getIP :: RSI read BFState Int
getIP = do
  (_tape, ip) <- get
  return ip

setIP :: Int -> RSI read BFState ()
setIP n = do
  (tape, _ip) <- get
  put (tape, n)

incIP :: RSI read BFState ()
incIP = do
  (tape, ip) <- get
  put (tape, ip + 1)

tapeZero :: RSI read BFState Bool
tapeZero = liftM (==0) readTapeM

blankState :: BFState
blankState = (blankTape, 0)
