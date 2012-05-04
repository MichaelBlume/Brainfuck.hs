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

doCommand :: Char -> BFMon ()
doCommand '<' = modTape retreat
doCommand '>' = modTape advance
doCommand '+' = modTape increment
doCommand '-' = modTape decrement
doCommand ',' = getCharS >>= (modTape . writeTape . ord)
doCommand '.' = readTapeM >>= (putCharS . chr)
doCommand '[' = do
  tz <- tapeZero
  when tz doJump
doCommand ']' = do
  tz <- tapeZero
  unless tz doJump

loopBF :: BFMon String
loopBF = do
  ins <- getIn
  doCommand ins
  incIP
  mc <- popCharS
  result <- endLoop
  case mc of
    Nothing -> return result
    Just c -> return $ c:result

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
fastPutStr [] = return ()
fastPutStr (c:cs) = putChar c >> hFlush stdout >> fastPutStr cs



