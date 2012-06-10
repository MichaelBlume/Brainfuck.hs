module BFRead
( lookupJump
, lookupIns
, getLength
, parseProg
, BFRead ()
) where

import RS
import Data.Array
import Data.Tuple
import Control.Monad
import qualified Data.Map as Map

type JumpTable = Map.Map Int Int
type BFProg = Array Int Char

data BFRead = BFRead {
    prog :: BFProg,
    jumps :: JumpTable,
    len :: Int}

lookupJumpF :: Int -> JumpTable -> Int
lookupJumpF n = (Map.! n)

lookupJump :: Int -> RS BFRead state Int
lookupJump i = liftM (lookupJumpF i . jumps) ask

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

  mappedJT = Map.fromList . appReverse $ buildJT enumSrc [] []
  appReverse l = l ++ (map swap l)

  buildJT :: [(Int,Char)] -> [(Int, Int)] -> [Int] -> [(Int, Int)]
  buildJT []            jt []         = jt
  buildJT []            _  _          = error "unmatched left bracket"
  buildJT ((_, ']'):_)  _  []         = error "unmatched right bracket"
  buildJT ((ip,'['):is) jt lStack     = buildJT is jt           (ip:lStack)
  buildJT ((ip,']'):is) jt (l:lStack) = buildJT is ((l, ip):jt) lStack
  buildJT (_:is)        jt lStack     = buildJT is jt           lStack

