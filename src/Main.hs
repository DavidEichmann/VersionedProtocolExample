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

import Core (HasUpgradePath (upgradePath))
import Core.Run
import qualified Peer.V1 as V1
import qualified Peer.V2 as V2
import qualified Peer.V3 as V3
import qualified Protocol.V1 as V1
import qualified Protocol.V2 as V2
import qualified Protocol.V3 as V3
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
          (ProtocolVersion 1)
          (ProtocolVersion 1)
          ( \(ProtocolVersion v) -> case v of
              -- use V1 codec (decoder) and downgrad v2 server to v1 server
              1 -> SomePeer V1.codec V1.server
              _ -> error $ "Unsupported version: " ++ show v
          )
      2 ->
        runServer
          (ProtocolVersion 1)
          (ProtocolVersion 2)
          ( \(ProtocolVersion v) -> case v of
              -- use V1 codec (decoder) and downgrad v2 server to v1 server
              1 -> SomePeer V1.codec server1
              -- use V2 codec and server
              2 -> SomePeer V2.codec server2
              _ -> error $ "Unsupported version: " ++ show v
          )
        where
          server1 = V2.downgradeServerV2ToV1 server2
          server2 = V2.server
      -- In practice we'll only have this case i.e. the latest backwards
      -- compatible version of the server
      3 ->
        runServer
          (ProtocolVersion 1)
          (ProtocolVersion 3)
          ( \(ProtocolVersion v) -> case v of
              -- use V1 codec (decoder) and downgrad v2 server to v1 server
              1 -> SomePeer V1.codec server1
              -- use V2 codec and server
              2 -> SomePeer V2.codec server2
              3 -> SomePeer V3.codec server3
              _ -> error $ "Unsupported version: " ++ show v
          )
        where
          server1 = V2.downgradeServerV2ToV1 server2
          server2 = V3.downgradeServerV3ToV2 server3
          server3 = V3.server
      _ -> error $ "Unsupported version: " ++ versionStr
    ['c' : _, versionStr, maxVStr] | let maxV = read maxVStr -> case read versionStr :: Int of
      -- Act as an old client (in practice this is just simulating an old
      -- release of the client)
      1 ->
        runClient
          (ProtocolVersion 1)
          (ProtocolVersion (min maxV 3))
          ( \(ProtocolVersion v) -> case v of
              1 -> SomeDecoderAndUpgradePath V1.codec upgradePath
              2 -> SomeDecoderAndUpgradePath V2.codec upgradePath
              3 -> SomeDecoderAndUpgradePath V3.codec upgradePath
              _ -> error $ "Unsupported version: " ++ show v
          )
          V1.client
      2 ->
        runClient
          (ProtocolVersion 2)
          (ProtocolVersion (min maxV 3))
          ( \(ProtocolVersion v) -> case v of
              2 -> SomeDecoderAndUpgradePath V2.codec upgradePath
              3 -> SomeDecoderAndUpgradePath V3.codec upgradePath
              _ -> error $ "Unsupported version: " ++ show v
          )
          V2.client
      -- In practice we'll only have this case i.e. the latest version of the
      -- client
      3 ->
        runClient
          (ProtocolVersion 3)
          (ProtocolVersion (min maxV 3))
          ( \(ProtocolVersion v) -> case v of
              3 -> SomeDecoderAndUpgradePath V3.codec upgradePath
              _ -> error $ "Unsupported version: " ++ show v
          )
          V3.client
      _ -> error $ "Unsupported version: " ++ versionStr
    _ ->
      putStrLn $
        unlines
          [ "Usage:",
            "cabal run -- c[lient] clientProtocolVersion maxSupportedVersion",
            "cabal run -- s[erver] serverVersion"
          ]
