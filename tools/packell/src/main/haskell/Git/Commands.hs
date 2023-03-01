-- Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Git.Commands where

import Shelly (run_, shelly, verbosely, run, cmd, CmdArg (toTextArg), silently)
import System.Directory (canonicalizePath)

-- | Fetch latest objects/refs from origin.
fetch :: IO ()
fetch = shelly . silently $ run_ "git" [ "fetch" ]

-- | Determine for a set of files if there has been any changes between the provided tag and the head of the changelog.
-- The function ignores files for which only top-level comments have changed (e.g. copyright headers).
hasDiff :: String -> [FilePath] -> IO Bool
hasDiff tag files = do
  fullPaths <- mapM canonicalizePath files
  (/=) mempty <$> (shelly . silently $ run "git" $ ["diff", "--shortstat", "-I", toTextArg "^[ ]*--", toTextArg tag, "HEAD"] ++ map toTextArg fullPaths)

-- | Checks if a tag already exists.
tagExists :: String -> IO Bool
tagExists tag = (/=) mempty <$> (shelly . silently $ run "git" ["tag", "-l", toTextArg tag])
