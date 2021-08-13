{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Core.Run
  ( runServer,
    runClient,
    ProtocolVersion (..),
    SomeDecoderAndUpgradePath (..),
    SomePeer (..),
  )
where

import qualified Codec.Serialise as S
import Control.Monad.ST
  ( RealWorld,
    stToIO,
  )
import Core
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromJust, fromMaybe)
import Data.Typeable
import Network.Simple.TCP

port :: ServiceName
port = "8005"

data SomePeer r a
  = forall protocol st.
    (Protocol protocol, S.Serialise (SomeMessage protocol)) =>
    SomePeer
      (MessageDecoder protocol)
      (Peer protocol r (st :: protocol) IO a)

data SomeDecoderAndUpgradePath r st
  = forall protocol' (st' :: protocol').
    (S.Serialise (SomeMessage protocol'), Protocol protocol') =>
    SomeDecoderAndUpgradePath
      (MessageDecoder protocol')
      (UpgradePath r st st')

newtype ProtocolVersion = ProtocolVersion Int

maxCommonVersion ::
  Int -> -- Peer A's min supported version
  Int -> -- Peer A's max supported version
  Int -> -- Peer B's min supported version
  Int -> -- Peer B's max supported version
  Maybe Int
maxCommonVersion minA maxA minB maxB =
  if lo <= hi
    then Just hi
    else Nothing
  where
    lo = max minA minB
    hi = min maxA maxB

handShake ::
  -- | Min protocol version supported
  ProtocolVersion ->
  -- | Max protocol version supported
  ProtocolVersion ->
  Socket ->
  -- | Negotiated version
  IO ProtocolVersion
handShake (ProtocolVersion minA) (ProtocolVersion maxA) socket = do
  -- Hand Shake
  send socket (BS.toStrict (S.serialise @Int minA))
  send socket (BS.toStrict (S.serialise @Int maxA))
  minB <- S.deserialise @Int . BS.fromStrict . fromJust <$> recv socket 1
  maxB <- S.deserialise @Int . BS.fromStrict . fromJust <$> recv socket 1

  putStrLn $ "My      supported version range: [" ++ show minA ++ ".." ++ show maxA ++ "]"
  putStrLn $ "Other's supported version range: [" ++ show minB ++ ".." ++ show maxB ++ "]"
  let negotiatedVersion = fromMaybe (error "FAILED to find a common version") (maxCommonVersion minA maxA minB maxB)
  putStrLn $ "negotiated version             : " ++ show negotiatedVersion
  return (ProtocolVersion negotiatedVersion)

runClient ::
  forall pPeer st a.
  (Protocol pPeer, Typeable a) =>
  -- | Min protocol version supported
  ProtocolVersion ->
  -- | Max protocol version supported
  ProtocolVersion ->
  (ProtocolVersion -> SomeDecoderAndUpgradePath AsClient st) ->
  Peer pPeer AsClient st IO a ->
  IO a
runClient minVersion maxVersion getUpgradePath peer =
  connect "127.0.0.1" port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr
    negotiatedVersion <- handShake minVersion maxVersion connectionSocket
    case getUpgradePath negotiatedVersion of
      SomeDecoderAndUpgradePath msgDecoder path ->
        runPeer
          msgDecoder
          connectionSocket
          peer
          path

runServer ::
  -- | Min protocol version supported
  ProtocolVersion ->
  -- | Max protocol version supported
  ProtocolVersion ->
  (ProtocolVersion -> SomePeer AsServer ()) ->
  IO a
runServer minVersion maxVersion mkPeer =
  serve (Host "127.0.0.1") port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr
    negotiatedVersion <- handShake minVersion maxVersion connectionSocket
    case mkPeer negotiatedVersion of
      SomePeer msgDecoder (peer :: Peer p r st m a) ->
        runPeer
          msgDecoder
          connectionSocket
          peer
          UpgradeComplete

runPeer ::
  forall pPeer pWire (r :: PeerRole) (st :: pPeer) (st0Wire :: pWire) a.
  ( Protocol pWire,
    Typeable a,
    Typeable r,
    S.Serialise (SomeMessage pWire)
  ) =>
  MessageDecoder pWire ->
  Socket ->
  Peer pPeer r st IO a ->
  -- | Upgrade path from the peer to initial state of the on-the-wire protocol
  UpgradePath r st st0Wire ->
  IO a
runPeer msgDecoder socket peerTop pathTop = go BS.empty (applyUpgradePath pathTop peerTop)
  where
    go ::
      forall (st' :: pWire).
      ByteString ->
      Peer pWire r st' IO a ->
      IO a
    go recvBuff peer = case peer of
      Effect peer'M -> do
        peer' <- peer'M
        go recvBuff peer'
      Done _ a -> return a
      Yield _ msg peer' -> do
        send socket (BS.toStrict $ S.serialise (SomeMessage msg))
        go recvBuff peer'
      Await hasAgency peer' -> do
        let recvMsg :: ByteString -> S.IDecode RealWorld (SomeMessage pWire) -> IO a
            recvMsg recvBuff' idecode = do
              case idecode of
                S.Done recvBuff'' _ someMessage ->
                  case msgDecoder hasAgency someMessage of
                    SomeMessageInSt msg ->
                      go
                        (BS.fromStrict recvBuff'')
                        (peer' msg)
                S.Partial inc -> do
                  moreBuff <- recv socket 1024
                  let recvBuff'' =
                        maybe recvBuff' ((recvBuff' <>) . BS.fromStrict) moreBuff
                  idecode' <- stToIO (inc (Just (BS.toStrict recvBuff'')))
                  recvMsg recvBuff'' idecode'
                S.Fail {} -> error "FAILED to deserialise a message!"
        (recvMsg recvBuff =<< stToIO S.deserialiseIncremental)
      DowngradeVersion path peer' -> go recvBuff (applyUpgradePath path peer')
      -- Peer is already at the wire version, so we can't use the upgraded peer
      -- unless it's equal to the wire version.
      UpgradeVersion path (peerUp :: Peer pUp r stUp IO a) peerAlt -> case path of
        UpgradeComplete -> go recvBuff peerUp
        _ -> go recvBuff peerAlt
