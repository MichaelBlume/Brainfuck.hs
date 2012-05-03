{-# LANGUAGE TypeSynonymInstances #-}
module BFMon where
import Types

type BFMon a = BFRead -> BFState -> IO (BFState, a)

bind :: (BFMon a) -> (a -> BFMon b) -> BFMon b
bind m1 f r s = do
  (s', a) <- m1 r s
  f a r s'

instance (Monad a) (BFMon a) where
  (>>=) = bind
  return x r s = (s, x)

-- emulate read
ask :: BFMon BFRead
ask r s = return (s, r)

-- emulate state
put :: BFState -> BFMon ()
put s' r _s = return (s', ())
get :: BFMon BFState
get r s = return (s, s)

-- emulate IO
putCharBF c r s = putChar c
getCharBF r s = getChar
