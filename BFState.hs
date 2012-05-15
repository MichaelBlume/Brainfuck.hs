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

type BFState = (Tape, Int, String, Maybe Char)

type MC = Maybe Char

modTape :: (Tape -> Tape) -> RS read BFState MC
modTape tf = do
  (tape, ip, i, c) <- get
  put (tf tape, ip, i, c)
  return Nothing

readTapeM :: RS read BFState Int
readTapeM = do
  (tape, _ip, _i, _c) <- get
  return $ readTape tape

getIP :: RS read BFState Int
getIP = do
  (_tape, ip, _i, _c) <- get
  return ip

setIP :: Int -> RS read BFState ()
setIP n = do
  (tape, _ip, i, c) <- get
  put (tape, n, i, c)

incIP :: RS read BFState ()
incIP = getIP >>= (setIP . (+1))

tapeZero :: RS read BFState Bool
tapeZero = liftM (==0) readTapeM

blankState :: String -> BFState
blankState inputString = (blankTape, 0, inputString, Nothing)

getCharS :: RS read BFState Char
getCharS = do
  (tape, ip, i, c) <- get
  case i of
    [] -> return '\n'
    (ic:ics) -> do
      put (tape, ip, ics, c)
      return ic
