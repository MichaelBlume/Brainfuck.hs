module Main (main) where

import Tape
import BFMon
import BFRead
import BFState

import Data.Char
import System.Environment

doCommand :: Char -> BFMon ()
doCommand '<' = modTape retreat
doCommand '>' = modTape advance
doCommand '+' = modTape increment
doCommand '-' = modTape decrement
doCommand ',' = getCharBF >>= (modTape . writeTape . ord)
doCommand '.' = readTapeM >>= (putCharBF . chr)
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

loopBF :: BFMon ()
loopBF = do
  ins <- getIn
  doCommand ins
  incIP
  ip <- getIP
  l <- getLength
  if ip == l
    then return ()
    else loopBF

doJump :: BFMon ()
doJump = do
  ip <- getIP
  ip' <- lookupJumpM ip
  setIP ip'

getIn = do
  ip <- getIP
  lookupIns ip

main = do
  args <- getArgs
  progSrc <- readFile $ args !! 0
  let prog = parseProg progSrc
  let state = blankState
  runBF loopBF prog state
  return ()


