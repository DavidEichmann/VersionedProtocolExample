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

module Protocol.V3 where

import qualified Codec.Serialise as S
import qualified Codec.Serialise.Decoding as S
import qualified Codec.Serialise.Encoding as S
import Core
import Core.Run
import Data.Text
import qualified Protocol.V2 as V2

---------------------------------------------------------------
-- Protocol
---------------------------------------------------------------

protocolVersion :: ProtocolVersion
protocolVersion = ProtocolVersion 3

data MyProtocol
  = StIdle -- Server is waiting for a message
  | StPinged -- Server has received a ping message
  | StEchoed -- Server has received an echo message
  | StQueried -- Server has received an echo message
  | StDone -- protocol finished

instance Protocol MyProtocol where
  data Message MyProtocol s s' where
    Ping :: Message MyProtocol StIdle StPinged
    Echo :: Text -> Message MyProtocol StIdle StEchoed
    EchoResp :: Text -> Message MyProtocol StEchoed StIdle
    Pong :: Message MyProtocol StPinged StIdle
    Query :: Query result -> Message MyProtocol StIdle StQueried
    Result :: Query result -> result -> Message MyProtocol StQueried StIdle
    Stop :: Message MyProtocol StIdle StDone

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency StIdle

  data ServerHasAgency st where
    TokPinged :: ServerHasAgency StPinged
    TokEchoed :: ServerHasAgency StEchoed
    TokQueried :: ServerHasAgency StQueried

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle x = case x of
  exclusionLemma_NobodyAndClientHaveAgency TokDone x = case x of
  exclusionLemma_NobodyAndServerHaveAgency TokDone x = case x of

data SomeQuery = forall result. SomeQuery (Query result)

data Query result where
  IncrementCount :: Int -> Query ()
  GetCount :: Query Int

instance S.Serialise SomeQuery where
  encode (SomeQuery q) = mconcat $ case q of
    IncrementCount i -> [S.encodeListLen 2, S.encodeInt 0, S.encodeInt i]
    GetCount -> [S.encodeListLen 1, S.encodeInt 1]
  decode = do
    len <- S.decodeListLen
    tag <- S.decode @Int
    case (len, tag) of
      (2, 0) -> SomeQuery <$> (IncrementCount <$> S.decodeInt)
      (1, 1) -> return (SomeQuery GetCount)
      _ -> error $ "Unknown (len, tag): " ++ show (len, tag)

instance S.Serialise (SomeMessage MyProtocol) where
  encode (SomeMessage msg) = mconcat $ case msg of
    Ping -> [S.encodeListLen 1, S.encode @Int 0]
    Echo str -> [S.encodeListLen 2, S.encode @Int 1, S.encodeString str]
    Pong -> [S.encodeListLen 1, S.encode @Int 2]
    EchoResp str -> [S.encodeListLen 2, S.encode @Int 3, S.encodeString str]
    Query q -> [S.encodeListLen 2, S.encode @Int 4, S.encode (SomeQuery q)]
    Result q result ->
      [ S.encodeListLen 3,
        S.encode @Int 5,
        S.encode (SomeQuery q),
        case q of
          IncrementCount {} -> S.encode @() result
          GetCount -> S.encode @Int result
      ]
    Stop -> [S.encodeListLen 1, S.encode @Int 6]
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
      (2, 4) -> do
        SomeQuery q <- S.decode @SomeQuery
        return (SomeMessage (Query q))
      (3, 5) -> do
        SomeQuery q <- S.decode @SomeQuery
        case q of
          IncrementCount {} -> SomeMessage <$> (Result q <$> S.decode @())
          GetCount -> SomeMessage <$> (Result q <$> S.decode @Int)
      (1, 6) -> return (SomeMessage Stop)
      _ -> error $ "Unknown (len, tag): " ++ show (len, tag)

codec :: MessageDecoder MyProtocol
codec (ClientAgency TokIdle) (SomeMessage Ping) = SomeMessageInSt Ping
codec (ClientAgency TokIdle) (SomeMessage msg@Echo {}) = SomeMessageInSt msg
codec (ClientAgency TokIdle) (SomeMessage Pong) = error "unexpected message"
codec (ClientAgency TokIdle) (SomeMessage EchoResp {}) = error "unexpected message"
codec (ClientAgency TokIdle) (SomeMessage Stop) = SomeMessageInSt Stop
codec (ClientAgency TokIdle) (SomeMessage msg@Query {}) = SomeMessageInSt msg
codec (ClientAgency TokIdle) (SomeMessage Result {}) = error "unexpected message"
codec (ServerAgency _) (SomeMessage Ping) = error "unexpected message"
codec (ServerAgency _) (SomeMessage Echo {}) = error "unexpected message"
codec (ServerAgency tok) (SomeMessage Pong) = case tok of
  TokPinged -> SomeMessageInSt Pong
  _ -> error "unexpected message"
codec (ServerAgency tok) (SomeMessage msg@(EchoResp {})) = case tok of
  TokEchoed -> SomeMessageInSt msg
  _ -> error "unexpected message"
codec (ServerAgency _) (SomeMessage Stop) = error "unexpected message"
codec (ServerAgency _) (SomeMessage Query {}) = error "unexpected message"
codec (ServerAgency tok) (SomeMessage msg@Result {}) = case tok of
  TokQueried -> SomeMessageInSt msg
  _ -> error "unexpected message"

---------------------------------------------------------------
-- Compat
---------------------------------------------------------------

downgradeServerV3ToV2 ::
  forall m a.
  Monad m =>
  Peer MyProtocol AsServer StIdle m a ->
  Peer V2.MyProtocol AsServer V2.StIdle m a
downgradeServerV3ToV2 server3Top = goIdle server3Top
  where
    goIdle ::
      Peer MyProtocol 'AsServer 'StIdle m a ->
      Peer V2.MyProtocol 'AsServer 'V2.StIdle m a
    goIdle server3 = case server3 of
      Effect eff -> Effect (goIdle <$> eff)
      Await (ClientAgency TokIdle) server3' ->
        Await
          (ClientAgency V2.TokIdle)
          ( \msg2 -> case msg2 of
              V2.Ping -> goPinged (server3' Ping)
              V2.Echo txt -> goEchoed (server3' (Echo txt))
              V2.Stop -> goDone (server3' Stop)
          )

    goDone ::
      Peer MyProtocol 'AsServer 'StDone m a ->
      Peer V2.MyProtocol 'AsServer 'V2.StDone m a
    goDone server3 = case server3 of
      Effect eff -> Effect (goDone <$> eff)
      Done _ a -> Done V2.TokDone a

    goPinged ::
      Peer MyProtocol 'AsServer 'StPinged m a ->
      Peer V2.MyProtocol 'AsServer 'V2.StPinged m a
    goPinged server3 = case server3 of
      Effect eff -> Effect (goPinged <$> eff)
      Yield (ServerAgency TokPinged) msg server3' -> case msg of
        Pong ->
          Yield
            (ServerAgency V2.TokPinged)
            V2.Pong
            (goIdle server3')

    goEchoed ::
      Peer MyProtocol 'AsServer 'StEchoed m a ->
      Peer V2.MyProtocol 'AsServer 'V2.StEchoed m a
    goEchoed server3 = case server3 of
      Effect eff -> Effect (goEchoed <$> eff)
      Yield (ServerAgency TokEchoed) msg server3' -> case msg of
        EchoResp txt ->
          Yield
            (ServerAgency V2.TokEchoed)
            (V2.EchoResp txt)
            (goIdle server3')

upgradeClientV2ToV3 ::
  forall m a.
  Monad m =>
  Peer V2.MyProtocol AsClient V2.StIdle m a ->
  Peer MyProtocol AsClient StIdle m a
upgradeClientV2ToV3 client2Top = goIdle client2Top
  where
    goIdle ::
      Peer V2.MyProtocol 'AsClient 'V2.StIdle m a ->
      Peer MyProtocol 'AsClient 'StIdle m a
    goIdle client2 = case client2 of
      Effect eff -> Effect (goIdle <$> eff)
      Yield (ClientAgency V2.TokIdle) msg client2' -> case msg of
        V2.Ping -> Yield (ClientAgency TokIdle) Ping (goPinged client2')
        V2.Echo txt -> Yield (ClientAgency TokIdle) (Echo txt) (goEchoed client2')
        V2.Stop -> Yield (ClientAgency TokIdle) Stop (goDone client2')

    goDone ::
      Peer V2.MyProtocol 'AsClient 'V2.StDone m a ->
      Peer MyProtocol 'AsClient 'StDone m a
    goDone client2 = case client2 of
      Effect eff -> Effect (goDone <$> eff)
      Done _ a -> Done TokDone a

    goPinged ::
      Peer V2.MyProtocol 'AsClient 'V2.StPinged m a ->
      Peer MyProtocol 'AsClient 'StPinged m a
    goPinged client2 = case client2 of
      Effect eff -> Effect (goPinged <$> eff)
      Await (ServerAgency V2.TokPinged) client2' ->
        Await
          (ServerAgency TokPinged)
          ( \msg3 -> case msg3 of
              Pong -> goIdle (client2' (V2.Pong))
          )

    goEchoed ::
      Peer V2.MyProtocol 'AsClient 'V2.StEchoed m a ->
      Peer MyProtocol 'AsClient 'StEchoed m a
    goEchoed client2 = case client2 of
      Effect eff -> Effect (goEchoed <$> eff)
      Await (ServerAgency V2.TokEchoed) client2' ->
        Await
          (ServerAgency TokEchoed)
          ( \msg3 -> case msg3 of
              EchoResp txt -> goIdle (client2' (V2.EchoResp txt))
          )