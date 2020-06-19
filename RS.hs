module RS
( RS (runRS)
, ask
, put
, get
) where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

newtype RS read state a = RS {
  runRS :: read -> state -> (state, a)
}

bind :: RS read state a -> (a -> RS read state b) -> RS read state b
bind m1 f = RS helper where
  helper r s = runRS (f a) r s' where
    (s', a) = runRS m1 r s

instance Monad (RS read state) where
  (>>=) = bind
  return x = RS $ \_r s -> (s, x)

instance Functor (RS read state) where
  fmap = liftM

instance Applicative (RS read state) where
  pure  = return
  (<*>) = ap

-- emulate read
ask :: RS read state read
ask = RS $ \r s -> (s, r)

-- emulate state
put :: state -> RS read state ()
put s' = RS $ \_r _s -> (s', ())
get :: RS read state state
get = RS $ \_r s -> (s, s)
