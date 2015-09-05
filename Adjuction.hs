{-# LANGUAGE FunctionalDependencies #-}
module Adjunction where

import Control.Applicative
import Control.Monad hiding (join)
import Control.Comonad

class (Functor f, Functor g) => Adjunction f g | f -> g, g -> f where
    phiLeft  :: (f a -> b) -> (a -> g b)
    phiRight :: (a -> g b) -> (f a -> b)

type Hom x y = x -> y

data Writer s a = Writer s a

instance Functor (Writer s) where
  fmap f (Writer s x) = Writer s (f x)

newtype Reader s a = Reader (s -> a)

instance Functor (Reader s) where
  fmap f (Reader g) = Reader (f . g)

instance Adjunction (Writer s) (Reader s) where
  phiLeft  f = \x -> Reader $ \s -> f $ Writer s x
  phiRight g = \(Writer s x) -> let Reader r = g x in r s


newtype Compose f g a = Compose { getCompose :: g (f a) }

instance (Functor f, Functor g, Adjunction f g) => Functor (Compose f g) where
  fmap f (Compose m) = Compose $ fmap (fmap f) m

join :: (Functor f, Functor g, Adjunction f g) => Compose f g (Compose f g a) -> Compose f g a
join (Compose m) = Compose $ fmap (phiRight getCompose) m

instance (Functor f, Functor g, Adjunction f g) => Monad (Compose f g) where
  return x = Compose $ phiLeft id x
  m >>= f = join $ fmap f m

type State s = Compose (Writer s) (Reader s)

put :: s -> State s ()
put s = Compose . Reader $ \_ -> Writer s ()

get :: State s s
get = Compose . Reader $ \s -> Writer s s

runState :: State s a -> s -> (s, a)
runState (Compose (Reader r)) s = let Writer s' a = r s in (s', a)

fib :: Int -> State Int Int
fib n = do
  i <- get
  put (i + 1)
  if n <= 1
  then return 1
  else do
    x <- fib (n - 1)
    y <- fib (n - 2)
    return (x + y)

main :: IO ()
main = print $ runState (fib 10) 0

newtype Cocompose f g x = Cocompose { getCocompose :: f (g x) }

instance (Functor f, Functor g, Adjunction f g) => Functor (Cocompose f g) where
  fmap f (Cocompose w) = Cocompose $ fmap (fmap f) w

instance (Functor f, Functor g, Adjunction f g) => Comonad (Cocompose f g) where
  extract (Cocompose w) = phiRight id w
  duplicate (Cocompose w) = Cocompose $ fmap (phiLeft Cocompose) w

type Store s = Cocompose (Writer s) (Reader s)

