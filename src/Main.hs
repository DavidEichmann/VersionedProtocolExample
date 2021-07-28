{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Codec.Serialise as S
import Core
import Core.Run
import Data.Kind
import Data.Void
import qualified Protocol.V1 as V1
import qualified Protocol.V2 as Latest
import qualified Protocol.V2 as V2
import System.Environment (getArgs)

main :: IO ()
main = do
  [arg, versionStr] <- getArgs
  let version = read versionStr
  case arg of
    's' : _ -> case version of
      1 -> runServer V1.protocolVersion (\(ProtocolVersion _) -> SomePeer V1.codec V1.server)
      2 -> runServer V2.protocolVersion (\(ProtocolVersion _) -> SomePeer V2.codec V2.server)
      _ -> error $ "Unknown version: " ++ versionStr
    'c' : _ -> case version of
      1 -> runClient V1.protocolVersion (\(ProtocolVersion _) -> SomePeer V1.codec V1.client)
      2 -> runClient V2.protocolVersion (\(ProtocolVersion _) -> SomePeer V2.codec V2.client)
      _ -> error $ "Unknown version: " ++ versionStr
    _ -> putStrLn "Usage: cabal run -- (c[lient]|s[erver]) protocolVersion"
