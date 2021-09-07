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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Protocol.V1 where

import qualified Codec.Serialise as S
import Core
import Core.Run
import qualified Protocol.V2 as V2

---------------------------------------------------------------
-- Protocol
---------------------------------------------------------------

protocolVersion :: ProtocolVersion
protocolVersion = ProtocolVersion 1

data MyProtocol
  = StIdle -- Server is waiting for ping message
  | StPinged -- Server has received a ping message
  | StDone -- protocol finished

instance Protocol MyProtocol where
  data Message MyProtocol s s' where
    Ping :: Message MyProtocol StIdle StPinged
    Pong :: Message MyProtocol StPinged StIdle
    Stop :: Message MyProtocol StIdle StDone

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency StIdle

  data ServerHasAgency st where
    TokPinged :: ServerHasAgency StPinged

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle x = case x of
  exclusionLemma_NobodyAndClientHaveAgency TokDone x = case x of
  exclusionLemma_NobodyAndServerHaveAgency TokDone x = case x of

instance S.Serialise (SomeMessage MyProtocol) where
  encode (SomeMessage msg) = case msg of
    Ping -> S.encode @Int 0
    Pong -> S.encode @Int 1
    Stop -> S.encode @Int 2
  decode = do
    tag <- S.decode @Int
    return $ case tag of
      0 -> SomeMessage Ping
      1 -> SomeMessage Pong
      2 -> SomeMessage Stop
      _ -> error $ "Unknown tag: " ++ show tag

codec :: MessageDecoder MyProtocol
codec (ClientAgency TokIdle) (SomeMessage Ping) = SomeMessageInSt Ping
codec (ClientAgency TokIdle) (SomeMessage Pong) = error "unexpected message"
codec (ClientAgency TokIdle) (SomeMessage Stop) = SomeMessageInSt Stop
codec (ServerAgency TokPinged) (SomeMessage Ping) = error "unexpected message"
codec (ServerAgency TokPinged) (SomeMessage Pong) = SomeMessageInSt Pong
codec (ServerAgency TokPinged) (SomeMessage Stop) = error "unexpected message"

---------------------------------------------------------------
-- Compatability
---------------------------------------------------------------

downgradeServerV2ToV1 ::
  forall m a.
  Monad m =>
  Peer V2.MyProtocol AsServer V2.StIdle m a ->
  Peer MyProtocol AsServer StIdle m a
downgradeServerV2ToV1 server2Top = goIdle server2Top
  where
    goIdle ::
      Peer V2.MyProtocol AsServer V2.StIdle m a ->
      Peer MyProtocol AsServer StIdle m a
    goIdle server2 = case server2 of
      Effect eff -> Effect (goIdle <$> eff)
      Await (ClientAgency V2.TokIdle) server2' ->
        Await
          (ClientAgency TokIdle)
          ( \msg1 -> case msg1 of
              Ping -> goPinged (server2' V2.Ping)
              Stop -> goDone (server2' V2.Stop)
          )

    goDone ::
      Peer V2.MyProtocol AsServer V2.StDone m a ->
      Peer MyProtocol 'AsServer StDone m a
    goDone server2 = case server2 of
      Effect eff -> Effect (goDone <$> eff)
      Done _ a -> Done TokDone a

    goPinged ::
      Peer V2.MyProtocol AsServer V2.StPinged m a ->
      Peer MyProtocol AsServer StPinged m a
    goPinged server2 = case server2 of
      Effect eff -> Effect (goPinged <$> eff)
      Yield (ServerAgency V2.TokPinged) msg server2' -> case msg of
        V2.Pong ->
          Yield
            (ServerAgency TokPinged)
            Pong
            (goIdle server2')

-- upgradeClientV1ToV2 ::
--   forall m a.
--   (Typeable a, Monad m) =>
--   Peer V1.MyProtocol AsClient V1.StIdle m a ->
--   Peer V2.MyProtocol AsClient StIdle m a
-- upgradeClientV1ToV2 server1Top = goIdle server1Top
--   where
--     goIdle ::
--       Peer V1.MyProtocol AsClient V1.StIdle m a ->
--       Peer V2.MyProtocol AsClient StIdle m a
--     goIdle client1 = case client1 of
--       Effect eff -> Effect (goIdle <$> eff)
--       Yield (ClientAgency _) msg client1' -> case msg of
--         V1.Ping -> Yield (ClientAgency TokIdle) Ping (goPinged client1')
--         V1.Stop -> Yield (ClientAgency TokIdle) Stop (goStopped client1')
--       DowngradeVersion v peer -> DowngradeVersion v peer
--       UpgradeVersion v peerUp peerAlt -> undefined

--     goPinged ::
--       Peer V1.MyProtocol AsClient V1.StPinged m a ->
--       Peer V2.MyProtocol AsClient StPinged m a
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
--       Peer V2.MyProtocol AsClient StDone m a
--     goStopped client1 = case client1 of
--       Effect eff -> Effect (goStopped <$> eff)
--       Done V1.TokDone a -> Done TokDone a

type instance UpgradeProtocol1 MyProtocol = V2.MyProtocol

$(pure [])

type instance UpgradeSt1 pr 'StIdle = V2.StIdle

type instance UpgradeSt1 pr 'StPinged = V2.StPinged

type instance UpgradeSt1 pr 'StDone = V2.StDone

instance UpgradePeer AsClient StIdle V2.MyProtocol where
  upgradePeer client1 = case client1 of
    Effect eff -> undefined -- Effect (upgradePeer <$> eff)
    Yield (ClientAgency _) msg client1' -> case msg of
      Ping -> Yield (ClientAgency V2.TokIdle) V2.Ping undefined -- (upgradePeer client1')
      Stop -> Yield (ClientAgency V2.TokIdle) V2.Stop undefined -- (upgradePeer client1')
      -- DowngradeVersion proxy peer -> DowngradeVersion proxy peer
    UpgradeVersion proxy peerUp peerAlt -> undefined -- UpgradeVersion proxy peerUp peerAlt
