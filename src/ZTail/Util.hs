module ZTail.Util (
    split,
    join,
    initlast,
    (>.=), (>.), (=.<),
    nop,
    justIf,
    whenJust, justWhen,
    catchWhen
  ) where

import qualified Control.Exception
import Control.Monad hiding (join)

split :: (a -> Bool) -> [a] -> [[a]]
{-# SPECIALIZE split :: (Char -> Bool) -> String -> [String] #-}
split p = splitp where
  splitp [] = [[]]
  splitp (x:l)
    | p x       = []     : splitp  l
    | otherwise = (x:l1) : l'
	where (l1:l') = splitp l

join :: a -> [[a]] -> [a]
{-# SPECIALIZE join :: Char -> [String] -> String #-}
join p = joinp where
  joinp [] = []
  joinp [x] = x
  joinp (x:l) = x ++ p : joinp l

initlast :: [a] -> ([a], a)
initlast [] = error "initlast: empty list"
initlast [x] = ([], x)
initlast (x:l) = (x:l1, r) where
  (l1, r) = initlast l

infixl 1 >., >.=
infixr 1 =.<
(>.) :: Monad m => m a -> b -> m b
(>.=) :: Monad m => m a -> (a -> b) -> m b
(=.<) :: Monad m => (a -> b) -> m a -> m b

(>.) e r = e >> return r
(>.=) e r = e >>= return . r
(=.<) r e = return . r =<< e -- fmap, <$>, liftM

nop :: Monad m => m ()
nop = return ()

-- Data.Foldable.mapM_
whenJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
whenJust = maybe nop

-- (>.) . guard
justIf :: Bool -> a -> Maybe a
justIf False = const Nothing
justIf True = Just

justWhen :: Monad m => Bool -> m a -> m (Maybe a)
justWhen False _ = return Nothing
justWhen True r = Just =.< r

catchWhen :: Control.Exception.Exception e => (e -> Bool) -> IO a -> IO a -> IO a
catchWhen t f h = Control.Exception.catchJust (guard . t) f (\() -> h)
