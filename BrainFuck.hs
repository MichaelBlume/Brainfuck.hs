import Tape
import BFMon
import BFRead
import BFState

doCommand :: Char -> BFMon ()
doCommand '<' = modTape retreat
doCommand '>' = modTape advance
doCommand '+' = modTape increment
doCommand '-' = modTape decrement
doCommand '.' = getCharBF >>= (modTape . writeTape)
doCommand ',' = readTapeM >>= putCharBF
doCommand '[' = do
  ts <- readTapeM
  if ts == 0
    then doJump
    else return ()
doCommand ']' = do
  ts <- readTapeM
  if ts == 0
    then return ()
    else doJump

loopBF :: BFMon () = do
  ins <- getIn
  doCommand ins
  incIP
  ip <- getIP
  l <- getLength
  if ip == l
    then return ()
    else runBF

doJump :: BFMon ()
doJump = do
  ip <- getIP
  ip' <- lookupJumpM ip
  setIP ip'

