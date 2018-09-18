{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{- |

 This module gives the possibility to transform well-typed
 functions in functions taking String arguments and returning
 a String value provided that the arguments types have a 'Read' instance
 and the output type has a 'Show' instance.

 Functions are added with the &: operator but can only be added if
  their type signature is not already present in the registry
  because functions are identified through their type signature.

 The type signature of a function can be retrieve with the 'getSignature'
 function.

 Then the 'runFunction' uses: the registry, the signature and a list of String
 arguments to execute a function. The result is an `Either String IO String`
 since parsing arguments could throw an exception.

-}
module Data.Stringly where

import           Control.Monad.Trans.Either
import           Data.Dynamic
import           Data.Either.Combinators
import           Data.Registry
import           Data.Registry.Internal.Dynamic
import           Data.Registry.Internal.Types
import           Prelude                           (Read, String, head, read, show)
import           Protolude                         as P hiding (head, show, typeRep)
import           Unsafe.Coerce

type StringifiedUnsafe = [String] -> String

-- | Typeclass for lifting well-typed functions to stringly-typed ones
--   But unsafely in the sense that reading arguments could throw an exception
class StringifyUnsafe a where
  stringifyUnsafe :: a -> StringifiedUnsafe

instance (Show a) => StringifyUnsafe a where
  stringifyUnsafe a = const (show a)

instance (StringifyUnsafe a', Read a) => StringifyUnsafe (a -> a') where
  stringifyUnsafe f args = stringifyUnsafe (f (read (Prelude.head args))) (drop 1 args)

type Stringified = [String] -> EitherT String IO String

-- | Typeclass for lifting well-typed functions to stringly-typed ones
class Stringify a where
  stringify :: a -> Stringified

instance (Show a) => Stringify a where
  stringify a = const (pure (show a))

instance (Stringify a', Read a, Typeable a, Typeable a') => Stringify (a -> a') where
  stringify f [] =
    left $ "no arguments left to apply to the function: "
           <> toS (funDescriptionToText (describeFunction f))

  stringify f args =
    do
       let firstArg = Prelude.head args
       arg <- EitherT (mapLeft (showError firstArg) <$> (try (evaluate . read $ firstArg) :: IO (Either SomeException a)))
       stringify (f arg) (drop 1 args)

       where
        showError :: String -> SomeException -> String
        showError arg1 e =
             "cannot read the value "
          <> arg1
          <> ": "
          <> show e

newtype StringResult a = StringResult { _result :: EitherT String IO String }
type StringFunction a = [String] -> StringResult a

-- | Representation of a function as a Typed value which can be inserted in the Registry
funString :: forall a . (Stringify a, Typeable a) => a -> Typed (StringFunction a)
funString f = fun (StringResult @a . stringify f)

newtype Signature = Signature FunctionDescription

-- | Return the type signature of a function
getSignature :: forall a . (Stringify a, Typeable a) => a -> Signature
getSignature f =
  let runIt args = StringResult @a (stringify f args)
  in  Signature (describeFunction runIt)

-- | Add a string function to the registry
registerStringFunction :: (Typeable a, Stringify a, Absent a out)
  => Typed (StringFunction a)
  -> Registry ins out
  -> Registry (Inputs (StringFunction a) :++ ins) (Output (StringResult a) ': out)
registerStringFunction = register

-- | Typeclass describing that a type is missing from a list of types
class Absent a (out :: [*])
instance Absent a '[]
instance (Absent a rest) => Absent a (b ': rest)

-- | Add an element to the Registry - Alternative to register where the parentheses can be ommitted
infixr 5 &:
(&:) :: forall a ins out . (Typeable a, Stringify a, Absent a out) =>
     a
  -> Registry ins out
  -> Registry (Inputs (StringFunction a) :++ ins) (Output (StringResult a) ': out)
(&:) f = registerStringFunction (funString f)


-- | Run a function which is inside a registry if it can be found with the proper signature
runFunction ::
     Registry ins out
  -> Signature
  -> [String]
  -> EitherT String IO String

runFunction registry@(Registry _ (Functions []) _ _) (Signature signature) _ =
  left $ "cannot find a function with such signature "
    <> toS (funDescriptionToText signature)
    <> "in the registry "
    <> show registry

runFunction (Registry vs (Functions (function : fs)) ss ms) (Signature signature) args =
  if funDescriptionToText (funDescription function) == funDescriptionToText signature then
    case applyFunction function [CreatedValue (toDyn args) (ValueDescription "" Nothing)] of
      Left e ->
        left (toS e)

      Right (CreatedValue (Dynamic _ v) _) ->
        -- this is ugly as hell but a simple fromDyn doesn't work
        _result (unsafeCoerce v)

      -- this is just to get exhaustive pattern matching, we should not get there
      Right (ProvidedValue (Dynamic _ v) _) ->
        _result (unsafeCoerce v)
  else
    runFunction (Registry vs (Functions fs) ss ms) (Signature signature) args
