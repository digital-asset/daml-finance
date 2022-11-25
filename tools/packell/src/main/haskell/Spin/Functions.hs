-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

module Spin.Functions
  ( withSpinner
  ) where

import Control.Applicative (Alternative((<|>)))
import Control.Concurrent (ThreadId, threadDelay, forkIO, throwTo)
import Control.Exception (throwTo, finally, AsyncException(ThreadKilled))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Maybe (catMaybes, mapMaybe)
import System.Console.ANSI (clearLine, hideCursor, setCursorColumn, showCursor)

import Spin.Types (Config(..), Spec(..))
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

defaultConfig :: Config
defaultConfig = Config
  { configMessage = ""
  , configPrefix = ""
  , configSymbol = ""
  , configSticky = False
  }

withSpinner
  :: Spec
  -> (((Config -> Config) -> IO ()) -> IO a)
  -> IO a
withSpinner spec fn = do
  hideCursor
  configIORef <- newIORef defaultConfig
  threadId <- forkIO $ tick spec configIORef 0
  finally (fn (modifyIORef configIORef)) (cleanup threadId configIORef)

cleanup :: ThreadId -> IORef Config -> IO ()
cleanup threadId configIORef = do
  throwTo threadId ThreadKilled
  resetLine
  finalLine <- renderFinal <$> readIORef configIORef
  case finalLine of
    Just line -> putStrLn line
    Nothing -> return ()
  showCursor

resetLine :: IO ()
resetLine = do
  hFlush stdout
  clearLine
  setCursorColumn 0

usInMs :: Int
usInMs = 1000

tick :: Spec -> IORef Config -> Int -> IO ()
tick spec@Spec{..} configIORef i = do
  config <- readIORef configIORef
  putStr $ renderTick spec i config
  threadDelay $ usInMs * specInterval
  resetLine
  tick spec configIORef $ i + 1

renderTick :: Spec -> Int -> Config -> String
renderTick Spec{..} i Config{..} = unwords $ catMaybes
  [ emptyMaybe configPrefix
  , emptyMaybe configSymbol <|> (emptyMaybe $ cycle specFrames !! i)
  , emptyMaybe configMessage
  ]

renderFinal :: Config -> Maybe String
renderFinal Config{..} =
  case configSticky of
    True -> Just $ unwords $ mapMaybe emptyMaybe
      [ configPrefix
      , configSymbol
      , configMessage
      ]
    False -> Nothing

emptyMaybe :: [a] -> Maybe [a]
emptyMaybe [] = Nothing
emptyMaybe x = Just x

