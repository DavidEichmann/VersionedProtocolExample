{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Protocol.V2 where

import qualified Codec.Serialise as S
import qualified Codec.Serialise.Decoding as S
import qualified Codec.Serialise.Encoding as S
import Core
import Core.Run
import Data.Text
import qualified Protocol.V1 as V1

---------------------------------------------------------------
-- Protocol
---------------------------------------------------------------

protocolVersion :: ProtocolVersion
protocolVersion = ProtocolVersion 2

data MyProtocol
  = StIdle -- Server is waiting for a message
  | StPinged -- Server has received a ping message
  | StEchoed -- Server has received an echo message
  | StDone -- protocol finished

instance Protocol MyProtocol where
  data Message MyProtocol s s' where
    Ping :: Message MyProtocol StIdle StPinged
    Echo :: Text -> Message MyProtocol StIdle StEchoed
    EchoResp :: Text -> Message MyProtocol StEchoed StIdle
    Pong :: Message MyProtocol StPinged StIdle
    Stop :: Message MyProtocol StIdle StDone

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency StIdle

  data ServerHasAgency st where
    TokPinged :: ServerHasAgency StPinged
    TokEchoed :: ServerHasAgency StEchoed

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle x = case x of
  exclusionLemma_NobodyAndClientHaveAgency TokDone x = case x of
  exclusionLemma_NobodyAndServerHaveAgency TokDone x = case x of

instance S.Serialise (SomeMessage MyProtocol) where
  encode (SomeMessage msg) = mconcat $ case msg of
    Ping -> [S.encodeListLen 1, S.encode @Int 0]
    Echo str -> [S.encodeListLen 2, S.encode @Int 1, S.encodeString str]
    Pong -> [S.encodeListLen 1, S.encode @Int 2]
    EchoResp str -> [S.encodeListLen 2, S.encode @Int 3, S.encodeString str]
    Stop -> [S.encodeListLen 1, S.encode @Int 4]
  decode = do
    len <- S.decodeListLen
    tag <- S.decode @Int
    case (len, tag) of
      (1, 0) -> return (SomeMessage Ping)
      (2, 1) -> do
        str <- S.decodeString
        return (SomeMessage (Echo str))
      (1, 2) -> return (SomeMessage Pong)
      (2, 3) -> do
        str <- S.decodeString
        return (SomeMessage (EchoResp str))
      (1, 4) -> return (SomeMessage Stop)
      _ -> error $ "Unknown (len, tag): " ++ show (len, tag)

codec :: MessageDecoder MyProtocol
codec (ClientAgency TokIdle) (SomeMessage Ping) = SomeMessageInSt Ping
codec (ClientAgency TokIdle) (SomeMessage msg@Echo {}) = SomeMessageInSt msg
codec (ClientAgency TokIdle) (SomeMessage Pong) = error "unexpected message"
codec (ClientAgency TokIdle) (SomeMessage EchoResp {}) = error "unexpected message"
codec (ClientAgency TokIdle) (SomeMessage Stop) = SomeMessageInSt Stop
codec (ServerAgency _) (SomeMessage Ping) = error "unexpected message"
codec (ServerAgency _) (SomeMessage Echo {}) = error "unexpected message"
codec (ServerAgency tok) (SomeMessage Pong) = case tok of
  TokPinged -> SomeMessageInSt Pong
  _ -> error "unexpected message"
codec (ServerAgency tok) (SomeMessage msg@(EchoResp {})) = case tok of
  TokEchoed -> SomeMessageInSt msg
  _ -> error "unexpected message"
codec (ServerAgency _) (SomeMessage Stop) = error "unexpected message"

---------------------------------------------------------------
-- Compatability
---------------------------------------------------------------

downgradeServerV2ToV1 ::
  forall m a.
  Monad m =>
  Peer MyProtocol AsServer StIdle m a ->
  Peer V1.MyProtocol AsServer V1.StIdle m a
downgradeServerV2ToV1 server2Top = goIdle server2Top
  where
    goIdle ::
      Peer MyProtocol 'AsServer 'StIdle m a ->
      Peer V1.MyProtocol 'AsServer 'V1.StIdle m a
    goIdle server2 = case server2 of
      Effect eff -> Effect (goIdle <$> eff)
      Await (ClientAgency TokIdle) server2' ->
        Await
          (ClientAgency V1.TokIdle)
          ( \msg1 -> case msg1 of
              V1.Ping -> goPinged (server2' Ping)
              V1.Stop -> goDone (server2' Stop)
          )

    goDone ::
      Peer MyProtocol 'AsServer 'StDone m a ->
      Peer V1.MyProtocol 'AsServer 'V1.StDone m a
    goDone server2 = case server2 of
      Effect eff -> Effect (goDone <$> eff)
      Done _ a -> Done V1.TokDone a

    goPinged ::
      Peer MyProtocol 'AsServer 'StPinged m a ->
      Peer V1.MyProtocol 'AsServer 'V1.StPinged m a
    goPinged server2 = case server2 of
      Effect eff -> Effect (goPinged <$> eff)
      Yield (ServerAgency TokPinged) msg server2' -> case msg of
        Pong ->
          Yield
            (ServerAgency V1.TokPinged)
            V1.Pong
            (goIdle server2')

-- upgradeClientV1ToV2 ::
--   forall m a.
--   (Typeable a, Monad m) =>
--   Peer V1.MyProtocol AsClient V1.StIdle m a ->
--   Peer MyProtocol AsClient StIdle m a
-- upgradeClientV1ToV2 server1Top = goIdle server1Top
--   where
--     goIdle ::
--       Peer V1.MyProtocol AsClient V1.StIdle m a ->
--       Peer MyProtocol AsClient StIdle m a
--     goIdle client1 = case client1 of
--       Effect eff -> Effect (goIdle <$> eff)
--       Yield (ClientAgency _) msg client1' -> case msg of
--         V1.Ping -> Yield (ClientAgency TokIdle) Ping (goPinged client1')
--         V1.Stop -> Yield (ClientAgency TokIdle) Stop (goStopped client1')
--       DowngradeVersion v peer -> DowngradeVersion v peer
--       UpgradeVersion v peerUp peerAlt -> undefined

--     goPinged ::
--       Peer V1.MyProtocol AsClient V1.StPinged m a ->
--       Peer MyProtocol AsClient StPinged m a
--     goPinged client1 = case client1 of
--       Effect eff -> Effect (goPinged <$> eff)
--       Await (ServerAgency V1.TokPinged) client1' ->
--         Await
--           (ServerAgency TokPinged)
--           ( \msg1 -> case msg1 of
--               Pong -> goIdle (client1' V1.Pong)
--           )

--     goStopped ::
--       Peer V1.MyProtocol AsClient V1.StDone m a ->
--       Peer MyProtocol AsClient StDone m a
--     goStopped client1 = case client1 of
--       Effect eff -> Effect (goStopped <$> eff)
--       Done V1.TokDone a -> Done TokDone a

instance Upgradeable AsClient V1.StIdle StIdle where
  upgradePeer client1 = case client1 of
    Effect eff -> Effect (upgradePeer <$> eff)
    Yield (ClientAgency _) msg client1' -> case msg of
      V1.Ping -> Yield (ClientAgency TokIdle) Ping (upgradePeer client1')
      V1.Stop -> Yield (ClientAgency TokIdle) Stop (upgradePeer client1')
    DowngradeVersion path peer -> DowngradeVersion (appendUpgradePath path) peer
    UpgradeVersion path peerUp peerAlt -> case path of
      UpgradeComplete -> upgradePeer peerUp
      UpgradePath path' ->
        UpgradeVersion
          path'
          peerUp
          (upgradePeer peerAlt)
