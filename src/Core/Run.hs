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
import Network.Simple.TCP

port :: ServiceName
port = "8005"

data SomePeer r a
  = forall protocol st.
    (Protocol protocol, S.Serialise (SomeMessage protocol)) =>
    SomePeer
      (MessageDecoder protocol)
      (Peer protocol r (st :: protocol) IO a)

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
  -- | Min protocol version supported
  ProtocolVersion ->
  -- | Max protocol version supported
  ProtocolVersion ->
  PeerUpgrader r pLo pHi -> -- TODO this and the min/max protocol versions seems redundant
  (ProtocolVersion -> SomePeer AsClient a) ->
  IO a
runClient minVersion maxVersion upgrader mkPeer =
  connect "127.0.0.1" port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr
    negotiatedVersion <- handShake minVersion maxVersion connectionSocket
    case mkPeer negotiatedVersion of
      SomePeer msgDecoder peer ->
        runPeer
          msgDecoder
          connectionSocket
          upgrader
          peer

runServer ::
  -- | Min protocol version supported
  ProtocolVersion ->
  -- | Max protocol version supported
  ProtocolVersion ->
  PeerUpgrader r pLo pHi -> -- TODO this and the min/max protocol versions seems redundant
  (ProtocolVersion -> SomePeer AsServer ()) ->
  IO a
runServer minVersion maxVersion upgrader mkPeer =
  serve (Host "127.0.0.1") port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr
    negotiatedVersion <- handShake minVersion maxVersion connectionSocket
    case mkPeer negotiatedVersion of
      SomePeer msgDecoder peer -> runPeer msgDecoder connectionSocket upgrader peer

runPeer ::
  forall pLo pHi pPeer pWire (r :: PeerRole) (st :: pPeer) a.
  ( Protocol pWire,
    S.Serialise (SomeMessage pWire),
    Typeable st
  ) =>
  MessageDecoder pWire ->
  Socket ->
  PeerUpgrader r pLo pHi ->
  Peer pPeer r st IO a ->
  IO a
runPeer msgDecoder socket upgrader =
  go BS.empty
    . fromMaybe (error "PeerUpgrader unable to upgrade to protocol on the wire")
    . upgradePeer upgrader
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
      -- Too late to change versions... that should be handled by the upgrade function.
      TryChangeVersion _ _ peer' -> go recvBuff peer'
