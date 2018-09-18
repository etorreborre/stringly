{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Stringly where

import           Control.Monad.Trans.Either
import           Data.Either.Combinators
import           Prelude                    as P (Read, String, head, read,
                                                  show)
import           Protolude                  hiding (head, show)

type StringifiedUnsafe = [String] -> String

-- | Typeclass for lifting well-typed functions to stringly-typed ones
class StringifyUnsafe a where
  stringifyUnsafe :: a -> StringifiedUnsafe

instance (Show a) => StringifyUnsafe a where
  stringifyUnsafe a = const (show a)

instance (StringifyUnsafe a', Read a) => StringifyUnsafe (a -> a') where
  stringifyUnsafe f args = stringifyUnsafe (f (read (P.head args))) (drop 1 args)

type Stringified = [String] -> EitherT String IO String

-- | Typeclass for lifting well-typed functions to stringly-typed ones
class Stringify a where
  stringify :: a -> Stringified

instance (Show a) => Stringify a where
  stringify a = const (pure (show a))

instance (Stringify a', Read a) => Stringify (a -> a') where
  stringify f args = do
    a <- EitherT (mapLeft show <$> (try (evaluate . read . P.head $ args) :: IO (Either SomeException a)))
    stringify (f a) (drop 1 args)
