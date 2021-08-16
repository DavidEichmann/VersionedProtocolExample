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

module Protocol.V2 where

import qualified Codec.Serialise as S
import qualified Codec.Serialise.Decoding as S
import qualified Codec.Serialise.Encoding as S
import Core
import Core.Run
import Data.Text

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
