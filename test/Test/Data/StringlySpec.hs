{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.StringlySpec where

import           Control.Monad.Trans.Either
import           Data.Stringly
import           Data.Text                  as T
import           Protolude
import           Test.Tasty.Extensions

test_stringify_unsafe  = test "do it " $ do
  let add (i :: Int) (j :: Text) = i + T.length j
  let f = stringifyUnsafe add
  f ["1", "\"text\""] === "5"

test_stringify  = test "do it " $ do
  let add (i :: Int) (j :: Text) = i + T.length j
  let f = stringify add
  result <- liftIO $ runEitherT $ f ["1", "\"text\""]
  result === Right "5"


----
tests = $(testGroupGenerator)
