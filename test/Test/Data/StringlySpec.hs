{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.StringlySpec where

import           Control.Monad.Trans.Either
import           Data.Registry
import           Data.Stringly
import           Data.Text                  as T
import           Protolude
import           Test.Tasty.Extensions

test_stringify_unsafe  = test "stringify a function" $ do
  let f = stringifyUnsafe add
  f ["1", "\"text\""] === "5"

test_stringify = test "stringify a function safely" $ do
  let f = stringify add
  result <- liftIO $ runEitherT $ f ["1", "\"text\""]
  result === Right "5"

test_store_function = test "store a stringified function in a registry" $ do
  let registry =
           add
        &: end

  let signature = getSignature add

  result <- liftIO $ runEitherT $ runFunction registry signature ["1", "\"text\""]
  result === Right "5"

test_store_several_functions = test "store more stringified functions in a registry" $ do
  let registry =
           add2
        &: add
        &: end

  let signature = getSignature add2

  result <- liftIO $ runEitherT $ runFunction registry signature ["\"text\"", "1"]
  result === Right "5"

-- * helpers
add :: Int -> Text -> Int
add i j = i + T.length j

add2 :: Text -> Int -> Int
add2 i j = j + T.length i

----
tests = $(testGroupGenerator)
