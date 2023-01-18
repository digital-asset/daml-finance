-- Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Options.DataDependencies (
  DataDependenciesCommand(..)
  ) where

-- | Commands for data-dependencies.
data DataDependenciesCommand
    = Update
    | Validate
    | DryRun
  deriving (Show)
