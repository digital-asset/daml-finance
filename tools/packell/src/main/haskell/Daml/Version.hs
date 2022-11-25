-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE LambdaCase #-}

module Daml.Version (
  update
) where

import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime(utctDay), getCurrentTime)
import Package.Yaml (IncrementVersion(SNAPSHOT, PATCH, MINOR, MAJOR))
import qualified Git.Commands as Git (tagExists)

-- | Checks if a tag/version already exists in the git repo
-- When it doesn't, it will compute the increment of the version.
update :: String -> String -> IncrementVersion -> IO (Maybe String)
update name version inc = Git.tagExists (name <> "/" <> version) >>= \case
  True -> update' version inc >>= \version -> pure $ Just version
  False -> pure Nothing

-- | Increments a specific part of a version.
update' :: String -> IncrementVersion -> IO String
update' version inc = case (inc, get version) of
  (MAJOR, x:_)        -> pure $ increment x <> ".0.0"
  (MINOR, x:y:_)      -> pure $ x <> "." <> increment y <> ".0"
  (PATCH, x:y:z:_)    -> pure $ x <> "." <> y <> "." <> increment z
  (SNAPSHOT, x:y:z:"99":d:i:_) -> do
    (year, month, day) <- toGregorian . utctDay <$> getCurrentTime
    if d == show year <> show month <> show day then
      pure $ x <> "." <> y <> "." <> z <> ".99." <> d <> "." <> increment i
    else
      pure $ x <> "." <> y <> "." <> z <> ".99." <> show year <> show month <> show day <> ".1"
  (SNAPSHOT, x:y:z:_) -> do
    (year, month, day) <- toGregorian . utctDay <$> getCurrentTime
    pure $ x <> "." <> y <> "." <> z <> ".99." <> show year <> show month <> show day <> ".1"
  _                   -> error $ "Unexpected version format. version=" <> version

-- | Extracts the version numbering from a versioning string, removing the period.
-- > get "123.45.67" = ["123", "45", "67"]
get :: String -> [String]
get = foldr (\c acc -> if c == '.' then []:acc else (c:head acc):tail acc) [[]]

-- | Increments a version by one.
-- Versions are enforced by daml to be integers.
increment :: String -> String
increment = show . (+) 1 . read
