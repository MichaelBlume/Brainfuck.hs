module RS
( RS (runRS)
, ask
, put
, get
) where

import System.IO

newtype RS read state a = RS {
  runRS :: read -> state -> (state, a)
}

bind :: RS read state a -> (a -> RS read state b) -> RS read state b
bind m1 f = RS helper where
  helper r s = runRS (f a) r s' where
    (s', a) = runRS m1 r s

instance Monad (RS read state) where
  (>>=) = bind
  return x = RS $ \r s -> (s, x)

-- emulate read
ask :: RS read state read
ask = RS $ \r s -> (s, r)

-- emulate state
put :: state -> RS read state ()
put s' = RS $ \r _s -> (s', ())
get :: RS read state state
get = RS $ \r s -> (s, s)
