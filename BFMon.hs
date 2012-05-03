{-# LANGUAGE TypeSynonymInstances #-}
module BFMon where
import Types

newtype BFMon a = BFMon {
  runBF :: BFRead -> BFState -> IO (BFState, a)
}

bind :: (BFMon a) -> (a -> BFMon b) -> BFMon b
bind m1 f r s = do
  (s', a) <- runBF m1 r s
  f a r s'

instance Monad BFMon where
  (>>=) m f = BFMon $ \r -> \s -> bind m f r s
  return x = BFMon $ \r -> \s -> (s, x)

-- emulate read
ask :: BFMon BFRead
ask = BFMon $ \r -> \s -> return (s, r)

-- emulate state
put :: BFState -> BFMon ()
put s' = BFMon $ \r -> \_s -> return (s', ())
get :: BFMon BFState
get = BFMon $ \r -> \s -> return (s, s)

-- emulate IO
putCharBF c = BFMon $ \r -> \s -> putChar c
getCharBF = BFMon $ \r -> \s -> getChar
