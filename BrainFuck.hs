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
doCommand _ = error "Nonsensical command -- was program parsed correctly?"

doJump :: BFMon ()
doJump = getIP >>= lookupJump >>= setIP

loopBF :: BFMon String
loopBF = do
  mc <- getIP >>= lookupIns >>= doCommand
  incIP
  liftM (mayPush mc) endLoop

mayPush :: Maybe a -> [a] -> [a]
mayPush Nothing s = s
mayPush (Just c) s = c:s

endLoop :: BFMon String
endLoop = do
  done <- liftM2 (==) getLength getIP
  if done
    then return ""
    else loopBF

runProg :: String -> String -> String
runProg progSrc inputS = result where
  prog = parseProg progSrc
  state = blankState inputS
  (_state, result) = runRS loopBF prog state

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  when (null args) $ error "Must include filename of BF program"
  (readFile $ head args) >>= (interact . runProg)
