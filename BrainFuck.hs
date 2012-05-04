module Main (main) where

import Tape
import BFRead
import BFState
import RSI

import Data.Char
import System.Environment
import Control.Monad

type BFMon = RSI BFRead BFState

doCommand :: Char -> BFMon ()
doCommand '<' = modTape retreat
doCommand '>' = modTape advance
doCommand '+' = modTape increment
doCommand '-' = modTape decrement
doCommand ',' = getCharRSI >>= (modTape . writeTape . ord)
doCommand '.' = readTapeM >>= (putCharRSI . chr)
doCommand '[' = do
  tz <- tapeZero
  when tz doJump
doCommand ']' = do
  tz <- tapeZero
  unless tz doJump

loopBF :: BFMon ()
loopBF = do
  ins <- getIn
  doCommand ins
  incIP
  ip <- getIP
  l <- getLength
  unless (ip == l) loopBF

doJump :: BFMon ()
doJump = do
  ip <- getIP
  ip' <- lookupJumpM ip
  setIP ip'

getIn = getIP >>= lookupIns

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Must include filename of BF program"
    else do
      progSrc <- readFile $ head args
      runProg progSrc

runProg :: String -> IO ()
runProg progSrc = do
  let prog = parseProg progSrc
  let state = blankState
  runRSI loopBF prog state
  return ()


