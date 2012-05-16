module BFState
( modTape
, readTapeM
, getIP
, setIP
, incIP
, blankState
, tapeZero
, BFState ()
, getCharS
, MC
) where

import RS
import Tape
import Control.Monad

data BFState = BFState { tape :: Tape
                       , instructionPointer :: Int
                       , remainingInput :: String
                       }

type MC = Maybe Char

modTape :: (Tape -> Tape) -> RS read BFState MC
modTape tf = do
  state <- get
  put state {tape = tf . tape $ state}
  return Nothing

readTapeM :: RS read BFState Int
readTapeM = liftM (readTape . tape) get

getIP :: RS read BFState Int
getIP = liftM instructionPointer get

setIP :: Int -> RS read BFState ()
setIP n = do
  state <- get
  put state {instructionPointer = n}

incIP :: RS read BFState ()
incIP = getIP >>= (setIP . (+1))

tapeZero :: RS read BFState Bool
tapeZero = liftM (==0) readTapeM

blankState :: String -> BFState
blankState = BFState blankTape 0

getCharS :: RS read BFState Char
getCharS = do
  state <- get
  case remainingInput state of
    [] -> return '\n'
    (ic:ics) -> do
      put state {remainingInput = ics}
      return ic
