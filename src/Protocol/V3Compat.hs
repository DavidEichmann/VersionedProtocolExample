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

module Protocol.V3Compat where

import Core
import qualified Protocol.V2 as V2
import qualified Protocol.V3 as V3

---------------------------------------------------------------
-- Protocol
---------------------------------------------------------------

downgradeServerV3ToV2 ::
  forall m a.
  Monad m =>
  Peer V3.MyProtocol AsServer V3.StIdle m a ->
  Peer V2.MyProtocol AsServer V2.StIdle m a
downgradeServerV3ToV2 server3Top = goIdle server3Top
  where
    goIdle ::
      Peer V3.MyProtocol 'AsServer 'V3.StIdle m a ->
      Peer V2.MyProtocol 'AsServer 'V2.StIdle m a
    goIdle server3 = case server3 of
      Effect eff -> Effect (goIdle <$> eff)
      Await (ClientAgency V3.TokIdle) server3' ->
        Await
          (ClientAgency V2.TokIdle)
          ( \msg2 -> case msg2 of
              V2.Ping -> goPinged (server3' V3.Ping)
              V2.Echo txt -> goEchoed (server3' (V3.Echo txt))
              V2.Stop -> goDone (server3' V3.Stop)
          )

    goDone ::
      Peer V3.MyProtocol 'AsServer 'V3.StDone m a ->
      Peer V2.MyProtocol 'AsServer 'V2.StDone m a
    goDone server3 = case server3 of
      Effect eff -> Effect (goDone <$> eff)
      Done _ a -> Done V2.TokDone a

    goPinged ::
      Peer V3.MyProtocol 'AsServer 'V3.StPinged m a ->
      Peer V2.MyProtocol 'AsServer 'V2.StPinged m a
    goPinged server3 = case server3 of
      Effect eff -> Effect (goPinged <$> eff)
      Yield (ServerAgency V3.TokPinged) msg server3' -> case msg of
        V3.Pong ->
          Yield
            (ServerAgency V2.TokPinged)
            V2.Pong
            (goIdle server3')

    goEchoed ::
      Peer V3.MyProtocol 'AsServer 'V3.StEchoed m a ->
      Peer V2.MyProtocol 'AsServer 'V2.StEchoed m a
    goEchoed server3 = case server3 of
      Effect eff -> Effect (goEchoed <$> eff)
      Yield (ServerAgency V3.TokEchoed) msg server3' -> case msg of
        V3.EchoResp txt ->
          Yield
            (ServerAgency V2.TokEchoed)
            (V2.EchoResp txt)
            (goIdle server3')

upgradeClientV2ToV3 ::
  forall m a.
  Monad m =>
  Peer V2.MyProtocol AsClient V2.StIdle m a ->
  Peer V3.MyProtocol AsClient V3.StIdle m a
upgradeClientV2ToV3 client2Top = goIdle client2Top
  where
    goIdle ::
      Peer V2.MyProtocol 'AsClient 'V2.StIdle m a ->
      Peer V3.MyProtocol 'AsClient 'V3.StIdle m a
    goIdle client2 = case client2 of
      Effect eff -> Effect (goIdle <$> eff)
      Yield (ClientAgency V2.TokIdle) msg client2' -> case msg of
        V2.Ping -> Yield (ClientAgency V3.TokIdle) V3.Ping (goPinged client2')
        V2.Echo txt -> Yield (ClientAgency V3.TokIdle) (V3.Echo txt) (goEchoed client2')
        V2.Stop -> Yield (ClientAgency V3.TokIdle) V3.Stop (goDone client2')

    goDone ::
      Peer V2.MyProtocol 'AsClient 'V2.StDone m a ->
      Peer V3.MyProtocol 'AsClient 'V3.StDone m a
    goDone client2 = case client2 of
      Effect eff -> Effect (goDone <$> eff)
      Done _ a -> Done V3.TokDone a

    goPinged ::
      Peer V2.MyProtocol 'AsClient 'V2.StPinged m a ->
      Peer V3.MyProtocol 'AsClient 'V3.StPinged m a
    goPinged client2 = case client2 of
      Effect eff -> Effect (goPinged <$> eff)
      Await (ServerAgency V2.TokPinged) client2' ->
        Await
          (ServerAgency V3.TokPinged)
          ( \msg3 -> case msg3 of
              V3.Pong -> goIdle (client2' (V2.Pong))
          )

    goEchoed ::
      Peer V2.MyProtocol 'AsClient 'V2.StEchoed m a ->
      Peer V3.MyProtocol 'AsClient 'V3.StEchoed m a
    goEchoed client2 = case client2 of
      Effect eff -> Effect (goEchoed <$> eff)
      Await (ServerAgency V2.TokEchoed) client2' ->
        Await
          (ServerAgency V3.TokEchoed)
          ( \msg3 -> case msg3 of
              V3.EchoResp txt -> goIdle (client2' (V2.EchoResp txt))
          )