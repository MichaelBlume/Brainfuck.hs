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
doCommand ',' = readToTape
doCommand '.' = printTape
doCommand '[' = tapeZero >>= (`when` doJump)
doCommand ']' = tapeZero >>= (`unless` doJump)
doCommand _ = error "Nonsensical command -- was program parsed correctly?"

doJump :: BFMon ()
doJump = getIP >>= lookupJump >>= setIP

loopBF :: BFMon ()
loopBF = do
  getIP >>= lookupIns >>= doCommand
  incIP
  endLoop

endLoop :: BFMon ()
endLoop = do
  done <- liftM2 (==) getLength getIP
  unless done loopBF

runProg :: String -> String -> String
runProg progSrc inputS = putChars [] where
  prog = parseProg progSrc
  state = blankState inputS
  (_endState, _result, putChars) = runRS loopBF prog state


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  when (null args) $ error "Must include filename of BF program"
  (readFile $ head args) >>= (interact . runProg)
