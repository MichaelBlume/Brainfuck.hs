module RSI
( RSI (runRSI)
, ask
, put
, get
, putCharRSI
, getCharRSI
) where

import System.IO

newtype RSI read state a = RSI {
  runRSI :: read -> state -> IO (state, a)
}

bind :: RSI read state a -> (a -> RSI read state b) -> RSI read state b
bind m1 f = RSI $ \r -> \s -> do
  (s', a) <- runRSI m1 r s
  runRSI (f a) r s'

instance Monad (RSI read state) where
  (>>=) = bind
  return x = RSI $ \r -> \s -> return (s, x)

-- emulate read
ask :: RSI read state read
ask = RSI $ \r -> \s -> return (s, r)

-- emulate state
put :: state -> RSI read state ()
put s' = RSI $ \r -> \_s -> return (s', ())
get :: RSI read state state
get = RSI $ \r -> \s -> return (s, s)

-- emulate IO
putCharRSI :: Char -> RSI read state ()
putCharRSI c = RSI $ \r -> \s -> do
  putChar c
  hFlush stdout
  return (s, ())
getCharRSI :: RSI read state Char
getCharRSI = RSI $ \r -> \s -> do
  c <- getChar
  return (s, c)
