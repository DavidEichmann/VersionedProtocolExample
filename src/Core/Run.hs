{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Core.Run where

import qualified Codec.Serialise as S
import Control.Monad.ST
  ( RealWorld,
    runST,
    stToIO,
  )
import Core
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromJust)
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

newtype ProtocolVersion = ProtocolVersion Int

runClient ::
  -- | Max protocol version supported
  ProtocolVersion ->
  (ProtocolVersion -> SomePeer AsClient a) ->
  IO a
runClient (ProtocolVersion maxVersion) mkPeer =
  connect "127.0.0.1" port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr

    -- Hand Shake
    send connectionSocket (BS.toStrict (S.serialise @Int maxVersion))
    v <- S.deserialise @Int . BS.fromStrict . fromJust <$> recv connectionSocket 8

    putStrLn $ "Max supported version: " ++ show maxVersion
    let negotiatedVersion = min maxVersion v
    putStrLn $ "negotiated version   : " ++ show negotiatedVersion

    case mkPeer (ProtocolVersion negotiatedVersion) of
      SomePeer msgDecoder peer -> runPeer msgDecoder BS.empty connectionSocket peer

runServer ::
  -- | Max protocol version supported
  ProtocolVersion ->
  (ProtocolVersion -> SomePeer AsServer ()) ->
  IO a
runServer (ProtocolVersion maxVersion) mkPeer =
  serve (Host "127.0.0.1") port $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr

    -- Hand Shake
    send connectionSocket (BS.toStrict (S.serialise @Int maxVersion))
    v <- S.deserialise @Int . BS.fromStrict . fromJust <$> recv connectionSocket 8

    putStrLn $ "Max supported version: " ++ show maxVersion
    let negotiatedVersion = min maxVersion v
    putStrLn $ "negotiated version   : " ++ show negotiatedVersion

    case mkPeer (ProtocolVersion negotiatedVersion) of
      SomePeer msgDecoder peer -> runPeer msgDecoder BS.empty connectionSocket peer

runPeer ::
  forall protocol r st a.
  (Protocol protocol, S.Serialise (SomeMessage protocol)) =>
  MessageDecoder protocol ->
  ByteString ->
  Socket ->
  Peer protocol r st IO a ->
  IO a
runPeer msgDecoder recvBuff socket peer = case peer of
  Effect peer'M -> do
    peer' <- peer'M
    runPeer msgDecoder recvBuff socket peer'
  Done _ a -> return a
  Yield _ msg peer' -> do
    send socket (BS.toStrict $ S.serialise (SomeMessage msg))
    runPeer msgDecoder recvBuff socket peer'
  Await hasAgency peer' -> do
    let recvMsg ::
          ByteString -> S.IDecode RealWorld (SomeMessage protocol) -> IO a
        recvMsg recvBuff' idecode = do
          case idecode of
            S.Done recvBuff'' _ someMessage ->
              case msgDecoder hasAgency someMessage of
                SomeMessageInSt msg ->
                  runPeer
                    msgDecoder
                    (BS.fromStrict recvBuff'')
                    socket
                    (peer' msg)
            S.Partial inc -> do
              moreBuff <- recv socket 1024
              let recvBuff'' =
                    maybe recvBuff' ((recvBuff' <>) . BS.fromStrict) moreBuff
              idecode' <- stToIO (inc (Just (BS.toStrict recvBuff'')))
              recvMsg recvBuff'' idecode'
            S.Fail {} -> error "FAILED to deserialise a message!"
    (recvMsg recvBuff =<< stToIO S.deserialiseIncremental)
