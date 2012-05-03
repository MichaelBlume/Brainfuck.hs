lookupJump :: JumpTable -> Int -> Int
lookupJump ((a, b): js) n
  | a == n = b
  | b == n = a
  | otherwise = lookupJump js n

type JumpTable = [(Int, Int)]
type BFProg = [Char]

type BFRead a = Reader (BFProg, JumpTable) a

lookupJumpM :: Int -> BFRead Int
lookupJumpM i = do
  (_prog, jt) <- ask
  return $ lookupJump jt i

doJump :: BFMon ()
doJump = do
  ip <- getIP
  ip' <- lookupJumpM ip
  setIP ip'
