-- Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Daml.Types (
  UpdatedConfig(..)
) where

import qualified Daml.Package as Daml (Package)
import qualified Daml.Yaml as Daml (Config)

-- | Contains a modified daml config alongside its `package` information in its original
--  form (including the original daml config). This is used to comparing the changes to a daml
-- config before and after processing.
data UpdatedConfig = UpdatedConfig {
    package :: Daml.Package
  , updatedConfig :: Daml.Config
} deriving (Eq, Show)
