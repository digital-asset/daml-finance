-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Options.Versioning (
  VersioningCommand(..)
  ) where

-- | Commands for versioning.
data VersioningCommand
    = BumpAll
    | Update
    | Validate
    | DryRun
  deriving (Show)
