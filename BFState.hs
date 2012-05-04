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
, putCharS
, popCharS
) where

import RS
import Tape
import Control.Monad

type BFState = (Tape, Int, String, Maybe Char)

modTape :: (Tape -> Tape) -> RS read BFState ()
modTape tf = do
  (tape, ip, i, c) <- get
  put (tf tape, ip, i, c)

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
incIP = do
  (tape, ip, i, c) <- get
  put (tape, ip + 1, i, c)

tapeZero :: RS read BFState Bool
tapeZero = liftM (==0) readTapeM

blankState :: String -> BFState
blankState inputString = (blankTape, 0, inputString, Nothing)

putCharS :: Char -> RS read BFState ()
putCharS c = do
  (tape, ip, i, oc) <- get
  case oc of
    Nothing -> put (tape, ip, i, Just c)
    Just _ -> error "character collision!"

getCharS :: RS read BFState Char
getCharS = do
  (tape, ip, i, c) <- get
  case i of
    [] -> return '\n'
    (ic:ics) -> do
      put (tape, ip, ics, c)
      return ic

popCharS :: RS read BFState (Maybe Char)
popCharS = do
  (tape, ip, i, c) <- get
  put (tape, ip, i, Nothing)
  return c
