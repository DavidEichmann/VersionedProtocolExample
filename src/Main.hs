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

import Core.Run
import qualified Protocol.V1 as V1
import qualified Protocol.V2 as V2
import qualified Protocol.V2Compat as V2
import qualified Protocol.V3 as V3
import qualified Protocol.V3Compat as V3
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Now we have a backwards compatible server :-)
    ['s' : _, versionStr] -> case read versionStr :: Int of
      -- Act as an old server (in practice this is just simulating an old
      -- release of the server)
      1 ->
        runServer
          V1.protocolVersion
          ( \(ProtocolVersion v) -> case v of
              -- use V1 codec (decoder) and downgrad v2 server to v1 server
              1 -> SomePeer V1.codec V1.server
              _ -> error $ "Unknown version: " ++ show v
          )
      2 ->
        runServer
          V2.protocolVersion
          ( \(ProtocolVersion v) -> case v of
              -- use V1 codec (decoder) and downgrad v2 server to v1 server
              1 -> SomePeer V1.codec server1
              -- use V2 codec and server
              2 -> SomePeer V2.codec server2
              _ -> error $ "Unknown version: " ++ show v
          )
        where
          server1 = V2.downgradeServerV2ToV1 server2
          server2 = V2.server
      -- In practice we'll only have this case i.e. the latest backwards
      -- compatible version of the server
      3 ->
        runServer
          V3.protocolVersion
          ( \(ProtocolVersion v) -> case v of
              -- use V1 codec (decoder) and downgrad v2 server to v1 server
              1 -> SomePeer V1.codec server1
              -- use V2 codec and server
              2 -> SomePeer V2.codec server2
              3 -> SomePeer V3.codec server3
              _ -> error $ "Unknown version: " ++ show v
          )
        where
          server1 = V2.downgradeServerV2ToV1 server2
          server2 = V3.downgradeServerV3ToV2 server3
          server3 = V3.server
      _ -> error $ "Unknown version: " ++ versionStr
    ['c' : _, versionStr] -> case read versionStr :: Int of
      -- Act as an old client (in practice this is just simulating an old
      -- release of the client)
      1 ->
        runClient
          V1.protocolVersion
          ( \(ProtocolVersion v) -> case v of
              1 -> SomePeer V1.codec V1.client
              _ -> error $ "Unknown version: " ++ show v
          )
      2 ->
        runClient
          V2.protocolVersion
          ( \(ProtocolVersion v) -> case v of
              1 -> SomePeer V1.codec (V2.downgradeClientV2ToV1 V2.client)
              2 -> SomePeer V2.codec V2.client
              _ -> error $ "Unknown version: " ++ show v
          )
      -- In practice we'll only have this case i.e. the latest version of the
      -- client
      3 ->
        runClient
          V3.protocolVersion
          ( \(ProtocolVersion v) -> case v of
              3 -> SomePeer V3.codec V3.client
              _ -> error $ "Unknown version: " ++ show v
          )
      _ -> error $ "Unknown version: " ++ versionStr
    _ ->
      putStrLn $
        unlines
          [ "Usage:",
            "cabal run -- c[lient] protocolVersion",
            "cabal run -- s[erver]"
          ]
