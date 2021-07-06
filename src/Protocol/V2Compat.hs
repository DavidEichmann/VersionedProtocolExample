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
