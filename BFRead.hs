module BFRead
( lookupJumpM
, lookupIns
, getLength
, parseProg
, BFRead ()
) where

import RSI
import Data.Array
import qualified Data.Map as Map

type JumpTable = Map.Map Int Int
type BFProg = Array Int Char

type BFRead = (BFProg, JumpTable, Int)

lookupJump :: JumpTable -> Int -> Int
lookupJump = (Map.!)


getJT :: RSI BFRead state JumpTable
getJT = do
  (_prog, jt, _length) <- ask
  return jt

lookupJumpM :: Int -> RSI BFRead state Int
lookupJumpM i = do
  jt <- getJT
  return $ lookupJump jt i

lookupIns :: Int -> RSI BFRead state Char
lookupIns i = do
  (prog, _jt, _length) <- ask
  return $ prog ! i

getLength :: RSI BFRead state Int
getLength = do
  (_prog, _jt, length) <- ask
  return length

parseProg :: String -> BFRead
parseProg src = (fromList terseSrc, jt, length terseSrc) where
  terseSrc = filter (`elem` "<>+-.,[]") src
  jt = Map.fromList $ getJT terseSrc [] [] 0

  getJT :: String -> [(Int, Int)] -> [Int] -> Int -> [(Int, Int)]
  getJT [] jt [] _ = jt
  getJT [] _ lStack _ = error "unmatched left brackets"
  getJT ('[':is) jt lStack ip = getJT is jt (ip:lStack) (ip+1)
  getJT (']':is) jt (l:lStack) ip = getJT is ((ip, l):(l, ip):jt) lStack (ip+1)
  getJT (_:is) jt lStack ip = getJT is jt lStack (ip+1)

  fromList l = array (0, length l - 1) $ zip [0..] l
