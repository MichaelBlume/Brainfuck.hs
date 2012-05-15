module Main (main) where

import Tape
import BFRead
import BFState
import RS

import Data.Char
import System.Environment
import Control.Monad
import System.IO

type BFMon = RS BFRead BFState

doCommand :: Char -> BFMon MC
doCommand '<' = modTape retreat
doCommand '>' = modTape advance
doCommand '+' = modTape increment
doCommand '-' = modTape decrement
doCommand ',' = getCharS >>= (modTape . writeTape . ord)
doCommand '.' = liftM (Just . chr) readTapeM
doCommand '[' = tapeZero >>= (`when` doJump) >> return Nothing
doCommand ']' = tapeZero >>= (`unless` doJump) >> return Nothing

loopBF :: BFMon String
loopBF = do
  ins <- getIn
  mc <- doCommand ins
  incIP
  result <- endLoop
  case mc of
    Nothing -> return result
    Just c -> return $ c:result

endLoop :: BFMon String
endLoop = do
  l <- getLength
  ip <- getIP
  if ip == l
    then return []
    else loopBF

doJump :: BFMon ()
doJump = do
  ip <- getIP
  ip' <- lookupJumpM ip
  setIP ip'

getIn = getIP >>= lookupIns

runProg :: String -> String -> String
runProg progSrc inputS = result where
  (_state, result) = runRS loopBF prog state
  prog = parseProg progSrc
  state = blankState inputS

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStr "Must include filename of BF program"
    else do
      progSrc <- readFile $ head args
      inputS <- getContents
      fastPutStr $ runProg progSrc inputS

fastPutStr :: String -> IO ()
fastPutStr = foldr helper (return ()) where
  helper c rst = putChar c >> hFlush stdout >> rst



