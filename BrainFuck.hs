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
doCommand '.' = printTape
doCommand '[' = tapeZero >>= (`when` doJump) >> return Nothing
doCommand ']' = tapeZero >>= (`unless` doJump) >> return Nothing
doCommand _ = error "Nonsensical command -- was program parsed correctly?"

doJump :: BFMon ()
doJump = getIP >>= lookupJump >>= setIP

loopBF :: BFMon ()
loopBF = do
  getIP >>= lookupIns >>= doCommand
  incIP
  endLoop

endLoop :: BFMon String
endLoop = do
  done <- liftM2 (==) getLength getIP
  if done
    then return ""
    else loopBF

runProg :: String -> String -> String
runProg progSrc inputS = getOutput endState where
  prog = parseProg progSrc
  state = blankState inputS
  (endState, _result) = runRS loopBF prog state


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  if null args
    then putStr "Must include filename of BF program\n"
    else (readFile $ head args) >>= (interact . runProg)
