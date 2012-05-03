import Tape
import Control.Monad.Reader

doCommand :: Char -> BFMon ()
doCommand '<' = modTape retreat
doCommand '>' = modTape advance
doCommand '+' = modTape increment
doCommand '-' = modTape decrement
doCommand '.' = getChar >>= (modTape . writeTape)
doCommand ',' = readTapeM >>= putChar
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

lookupJump :: JumpTable -> Int -> Int
lookupJump ((a, b): js) n
  | a == n = b
  | b == n = a
  | otherwise = lookupJump js n

type JumpTable = [(Int, Int)]
type BFProg = [Char]

type BFRead a = Reader (BFProg, JumpTable) a

lookupJumpM :: Int -> BFRead Int
lookupJumpM i = do
  (_prog, jt) <- ask
  return $ lookupJump jt i

doJump :: BFMon ()
doJump = do
  ip <- getIP
  ip' <- lookupJumpM ip
  setIP ip'
