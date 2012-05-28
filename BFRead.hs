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

lookupJump :: Int -> JumpTable -> Int
lookupJump n = (Map.! n)

lookupJumpM :: Int -> RS BFRead state Int
lookupJumpM i = liftM (lookupJump i . jumps) ask

lookupIns :: Int -> RS BFRead state Char
lookupIns i = liftM ((!i) . prog) ask

getLength :: RS BFRead state Int
getLength = liftM len ask

parseProg :: String -> BFRead
parseProg src = BFRead (fromList terseSrc) mappedJT (length terseSrc) where
  terseSrc = filter (`elem` "<>+-.,[]") src
  mappedJT = Map.fromList $ buildJT terseSrc [] [] 0

  buildJT :: String -> [(Int, Int)] -> [Int] -> Int -> [(Int, Int)]
  buildJT [] jt [] _ = jt
  buildJT [] _  _  _ = error "unmatched left bracket"
  buildJT ('[':is) jt lStack ip = buildJT is jt (ip:lStack) (ip+1)
  buildJT (']':_) _ [] _ = error "unmatched right bracket"
  buildJT (']':is) jt (l:lStack) ip = buildJT is ((ip, l):(l, ip):jt) lStack (ip+1)
  buildJT (_:is) jt lStack ip = buildJT is jt lStack (ip+1)

  fromList l = array (0, length l - 1) $ zip [0..] l
