module BFRead
( lookupJumpM
, lookupIns
, getLength
, parseProg
, BFRead ()
) where

import RS
import Data.Array
import Control.Monad
import qualified Data.Map as Map

type JumpTable = Map.Map Int Int
type BFProg = Array Int Char

data BFRead = BFRead {
    prog :: BFProg,
    jumps :: JumpTable,
    len :: Int}

lookupJump :: JumpTable -> Int -> Int
lookupJump = (Map.!)


getJT :: RS BFRead state JumpTable
getJT = liftM jumps ask

lookupJumpM :: Int -> RS BFRead state Int
lookupJumpM i = liftM (`lookupJump` i) getJT

lookupIns :: Int -> RS BFRead state Char
lookupIns i = liftM ((!i) . prog) ask

getLength :: RS BFRead state Int
getLength = liftM len ask

parseProg :: String -> BFRead
parseProg src = BFRead (fromList terseSrc) jt (length terseSrc) where
  terseSrc = filter (`elem` "<>+-.,[]") src
  jt = Map.fromList $ getJT terseSrc [] [] 0

  getJT :: String -> [(Int, Int)] -> [Int] -> Int -> [(Int, Int)]
  getJT [] jt [] _ = jt
  getJT [] _ lStack _ = error "unmatched left bracket"
  getJT ('[':is) jt lStack ip = getJT is jt (ip:lStack) (ip+1)
  getJT (']':_) _ [] _ = error "unmatched right bracket"
  getJT (']':is) jt (l:lStack) ip = getJT is ((ip, l):(l, ip):jt) lStack (ip+1)
  getJT (_:is) jt lStack ip = getJT is jt lStack (ip+1)

  fromList l = array (0, length l - 1) $ zip [0..] l
