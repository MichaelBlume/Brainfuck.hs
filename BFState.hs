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
, printTape
, getOutput
) where

import RS
import Tape
import Control.Monad
import Data.Char

data BFState = BFState { tape :: Tape
                       , instructionPointer :: Int
                       , prependChars :: String -> String
                       , remainingInput :: String
                       }

type MC = Maybe Char

getOutput :: BFState -> String
getOutput s = prependChars s []

modTape :: (Tape -> Tape) -> RS read BFState MC
modTape tf = do
  state <- get
  put state {tape = tf . tape $ state}
  return Nothing

printTape :: RS read BFState MC
printTape = liftM chr readTapeM >>= writeChar >> return Nothing

writeChar :: Char -> RS read BFState ()
writeChar c = do
  state <- get
  put state {prependChars = (prependChars state) . (c:)}

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
blankState = BFState blankTape 0 id

getCharS :: RS read BFState Char
getCharS = do
  state <- get
  case remainingInput state of
    [] -> return '\n'
    (ic:ics) -> do
      put state {remainingInput = ics}
      return ic
