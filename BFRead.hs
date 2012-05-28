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
parseProg src = BFRead srcArray mappedJT srcLength where
  terseSrc = filter (`elem` "<>+-.,[]") src

  srcLength = length terseSrc
  enumSrc = zip [0..] terseSrc
  srcArray = array (0, srcLength - 1) $ enumSrc

  mappedJT = Map.fromList $ buildJT enumSrc [] []

  buildJT :: [(Int,Char)] -> [(Int, Int)] -> [Int] -> [(Int, Int)]
  buildJT [] jt [] = jt
  buildJT [] _  _  = error "unmatched left bracket"
  buildJT ((_,']'):_) _ [] = error "unmatched right bracket"
  buildJT ((ip,'['):is) jt lStack = buildJT is jt (ip:lStack)
  buildJT ((ip,']'):is) jt (l:lStack) = buildJT is ((ip, l):(l, ip):jt) lStack
  buildJT (_:is) jt lStack = buildJT is jt lStack

