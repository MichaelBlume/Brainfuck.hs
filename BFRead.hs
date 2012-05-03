module BFRead where
import Types
import BFMon

lookupJump :: JumpTable -> Int -> Int
lookupJump ((a, b): js) n
  | a == n = b
  | b == n = a
  | otherwise = lookupJump js n


getJT :: BFMon JumpTable
getJT = do
  (_prog, jt) <- ask
  return jt

lookupJumpM :: Int -> (BFMon Int)
lookupJumpM i = do
  jt <- getJT
  return $ lookupJump jt i
