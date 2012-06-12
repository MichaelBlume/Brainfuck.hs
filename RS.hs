module RS
( RS (runRS)
, ask
, put
, get
, writeChar
) where

newtype RS read state a = RS {
  runRS :: read -> state -> (state, a, String -> String)
}

bind :: RS read state a -> (a -> RS read state b) -> RS read state b
bind m1 f = RS helper where
  helper r s = (s'', a', cf . cf') where
    (s'', a', cf') = runRS (f a) r s'
    (s', a, cf) = runRS m1 r s

instance Monad (RS read state) where
  (>>=) = bind
  return x = RS $ \_r s -> (s, x, id)

-- emulate read
ask :: RS read state read
ask = RS $ \r s -> (s, r, id)

-- emulate state
put :: state -> RS read state ()
put s' = RS $ \_r _s -> (s', (), id)
get :: RS read state state
get = RS $ \_r s -> (s, s, id)

-- emulate write
writeChar :: Char -> RS read state ()
writeChar c = RS $ \_r s -> (s, (), (c:))
