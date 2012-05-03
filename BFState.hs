module BFState where
import Types
import BFMon
import Tape


modTape :: (Tape -> Tape) -> BFMon ()
modTape tf = do
  (tape, ip) <- get
  put (tf tape, ip)

readTapeM :: BFMon Int
readTapeM = do
  (tape, _ip) <- get
  return $ readTape tape

getIP :: BFMon Int
getIP = do
  (_tape, ip) <- get
  return ip

setIP :: Int -> BFMon ()
setIP n = do
  (tape, _ip) <- get
  put (tape, n)

incIP :: BFMon ()
incIP = do
  (tape, ip) <- get
  put (tape, ip + 1)

blankState :: BFState
blankState = (blankTape, 0)
