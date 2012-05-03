type BFState a = State (Tape, Int) a

modTape :: (Tape -> Tape) -> BFState ()
modTape tf = do
  (tape, ip) <- get
  put (tf tape, ip)

readTapeM :: BFState Int
readTapeM = do
  (tape, _ip) <- get
  return $ readTape tape

getIP :: BFState Int
getIP = do
  (_tape, ip) <- get
  return ip

setIP :: Int -> BFState ()
setIP n = do
  (tape, _ip) <- get
  put (tape, n)

incIP :: BFState ()
incIP = do
  (tape, ip) <- get
  put (tape, ip + 1)
