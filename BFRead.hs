module BFRead
( lookupJumpM
, lookupIns
, getLength
, parseProg
) where

import Types
import BFMon

import Array

lookupJump :: JumpTable -> Int -> Int
lookupJump [] n = error "program parse fail?"
lookupJump ((a, b): js) n
  | a == n = b
  | b == n = a
  | otherwise = lookupJump js n


getJT :: BFMon JumpTable
getJT = do
  (_prog, jt, _length) <- ask
  return jt

lookupJumpM :: Int -> (BFMon Int)
lookupJumpM i = do
  jt <- getJT
  return $ lookupJump jt i

lookupIns :: Int -> (BFMon Char)
lookupIns i = do
  (prog, _jt, _length) <- ask
  return $ prog ! i

getLength :: (BFMon Int)
getLength = do
  (prog, _jt, length) <- ask
  return length

parseProg :: String -> BFRead
parseProg src = (fromList terseSrc, jt, length terseSrc) where
  terseSrc = filter (`elem` "<>+-.,[]") src
  jt = getJT terseSrc [] [] 0

  getJT :: String -> JumpTable -> [Int] -> Int -> JumpTable
  getJT [] jt [] _ = jt
  getJT [] _ lStack _ = error "unmatched left brackets"
  getJT ('[':is) jt lStack ip = getJT is jt (ip:lStack) (ip+1)
  getJT (']':is) jt (l:lStack) ip = getJT is ((l, ip):jt) lStack (ip+1)
  getJT (_:is) jt lStack ip = getJT is jt lStack (ip+1)

  fromList l = array (0, (length l) - 1) $ zip [0..] l
