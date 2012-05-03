{-# LANGUAGE TypeSynonymInstances #-}
module BFMon
( BFMon (runBF)
, ask
, put
, get
, putCharBF
, getCharBF
) where

import Types

newtype BFMon a = BFMon {
  runBF :: BFRead -> BFState -> IO (BFState, a)
}

bind :: (BFMon a) -> (a -> BFMon b) -> BFMon b
bind m1 f = BFMon $ \r -> \s -> do
  (s', a) <- runBF m1 r s
  runBF (f a) r s'

instance Monad BFMon where
  (>>=) = bind
  return x = BFMon $ \r -> \s -> return (s, x)

-- emulate read
ask :: BFMon BFRead
ask = BFMon $ \r -> \s -> return (s, r)

-- emulate state
put :: BFState -> BFMon ()
put s' = BFMon $ \r -> \_s -> return (s', ())
get :: BFMon BFState
get = BFMon $ \r -> \s -> return (s, s)

-- emulate IO
putCharBF :: Char -> BFMon ()
putCharBF c = BFMon $ \r -> \s -> do
  putChar c
  return (s, ())
getCharBF :: BFMon Char
getCharBF = BFMon $ \r -> \s -> do
  c <- getChar
  return (s, c)
