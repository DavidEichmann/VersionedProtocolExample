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

module Protocol.V2Compat where

import Core
import Data.Text
import qualified Protocol.V1 as V1
import qualified Protocol.V2 as V2

---------------------------------------------------------------
-- Protocol
---------------------------------------------------------------

downgradeServerV2ToV1 ::
  forall m a.
  Monad m =>
  Peer V2.MyProtocol AsServer V2.StIdle m a ->
  Peer V1.MyProtocol AsServer V1.StIdle m a
downgradeServerV2ToV1 server2Top = goIdle server2Top
  where
    goIdle ::
      Peer V2.MyProtocol 'AsServer 'V2.StIdle m a ->
      Peer V1.MyProtocol 'AsServer 'V1.StIdle m a
    goIdle server2 = case server2 of
      Effect eff -> Effect (goIdle <$> eff)
      Await (ClientAgency V2.TokIdle) server2' ->
        Await
          (ClientAgency V1.TokIdle)
          ( \msg1 -> case msg1 of
              V1.Ping -> goPinged (server2' V2.Ping)
              V1.Stop -> goDone (server2' V2.Stop)
          )

    goDone ::
      Peer V2.MyProtocol 'AsServer 'V2.StDone m a ->
      Peer V1.MyProtocol 'AsServer 'V1.StDone m a
    goDone server2 = case server2 of
      Effect eff -> Effect (goDone <$> eff)
      Done _ a -> Done V1.TokDone a

    goPinged ::
      Peer V2.MyProtocol 'AsServer 'V2.StPinged m a ->
      Peer V1.MyProtocol 'AsServer 'V1.StPinged m a
    goPinged server2 = case server2 of
      Effect eff -> Effect (goPinged <$> eff)
      Yield (ServerAgency V2.TokPinged) msg server2' -> case msg of
        V2.Pong ->
          Yield
            (ServerAgency V1.TokPinged)
            V1.Pong
            (goIdle server2')

downgradeClientV2ToV1 ::
  forall m a.
  Monad m =>
  Peer V2.MyProtocol AsClient V2.StIdle m a ->
  Peer V1.MyProtocol AsClient V1.StIdle m a
downgradeClientV2ToV1 server2Top = goIdle server2Top
  where
    goIdle ::
      Peer V2.MyProtocol AsClient V2.StIdle m a ->
      Peer V1.MyProtocol AsClient V1.StIdle m a
    goIdle client2 = case client2 of
      Effect eff -> Effect (goIdle <$> eff)
      Yield (ClientAgency tok) msg client2' -> case msg of
        V2.Ping -> Yield (ClientAgency V1.TokIdle) V1.Ping (goPinged client2')
        V2.Stop -> Yield (ClientAgency V1.TokIdle) V1.Stop (goStopped client2')
        V2.Echo txt -> goEchoed txt client2'

    goPinged ::
      Peer V2.MyProtocol AsClient V2.StPinged m a ->
      Peer V1.MyProtocol AsClient V1.StPinged m a
    goPinged client2 = case client2 of
      Effect eff -> Effect (goPinged <$> eff)
      Await (ServerAgency V2.TokPinged) client2' ->
        Await
          (ServerAgency V1.TokPinged)
          ( \msg1 -> case msg1 of
              V1.Pong -> goIdle (client2' V2.Pong)
          )

    goStopped ::
      Peer V2.MyProtocol AsClient V2.StDone m a ->
      Peer V1.MyProtocol AsClient V1.StDone m a
    goStopped client2 = case client2 of
      Effect eff -> Effect (goStopped <$> eff)
      Done V2.TokDone a -> Done V1.TokDone a

    -- Here we use domain knowledge of how the server should act in order to
    -- make the client backwards compatible. This will only work in simple cases
    -- I'm afraid. More commonly we might want to write a client that can check
    -- the version and provide different behaviors accordingly.
    goEchoed ::
      Text ->
      Peer V2.MyProtocol AsClient V2.StEchoed m a ->
      Peer V1.MyProtocol AsClient V1.StIdle m a
    goEchoed txt client2 = case client2 of
      Effect eff -> Effect (goEchoed txt <$> eff)
      Await (ServerAgency V2.TokEchoed) client2' -> goIdle (client2' (V2.EchoResp txt))
