module Password where

newtype PwdOp a = PwdOp { runPwd :: String -> (a, String) }

-- Functor
instance Functor PwdOp where
    fmap f (PwdOp g) = PwdOp $ \s -> let (a, s') = g s in (f a, s')

-- Applicative
instance Applicative PwdOp where
    pure a = PwdOp $ \s -> (a, s)
    (PwdOp f) <*> (PwdOp g) = PwdOp $ \s -> let (h, s') = f s
                                                (a, s'') = g s' in (h a, s'')

-- Monad
instance Monad PwdOp where
    (PwdOp g) >>= f = PwdOp $ \s -> let (a, s') = g s
                                        PwdOp g' = f a in g' s'

setPassword :: String -> PwdOp ()
setPassword new = PwdOp $ \_ -> ((), new)

checkPassword :: String -> PwdOp Bool
checkPassword attempt = PwdOp $ \current -> (attempt == current, current)

runPwdOp :: PwdOp a -> a
runPwdOp (PwdOp f) = fst $ f ""