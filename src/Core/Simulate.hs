{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Core.Simulate where

import Core

simulate ::
  forall protocol st m result.
  (Monad m, Protocol protocol) =>
  Peer protocol AsClient st m result ->
  Peer protocol AsServer st m result ->
  m (result, result)
simulate client server = do
  case client of
    Effect effectClient -> do
      client' <- effectClient
      simulate client' server
    Done noAgency resultClient -> do
      let finishServer :: Peer protocol AsServer st m result -> m result
          finishServer server' = case server' of
            Effect effectServer -> finishServer =<< effectServer
            Done _ serverResult -> return serverResult
            Yield (ServerAgency serverAgency) _ _ ->
              case exclusionLemma_NobodyAndServerHaveAgency noAgency serverAgency of
            Await (ClientAgency clientAgency) _ ->
              case exclusionLemma_NobodyAndClientHaveAgency noAgency clientAgency of

      resultServer <- finishServer server
      return (resultClient, resultServer)
    Yield (ClientAgency clientAgency) (msg :: Message protocol st st') client' ->
      simulate client' =<< do
        let continueServer ::
              Peer protocol AsServer st m result ->
              m (Peer protocol AsServer st' m result)
            continueServer server' = case server' of
              Effect effectServer -> continueServer =<< effectServer
              Done noAgency _serverResult ->
                case exclusionLemma_NobodyAndClientHaveAgency
                  noAgency
                  clientAgency of

              Yield (ServerAgency serverAgency) _ _ ->
                case exclusionLemma_ClientAndServerHaveAgency
                  clientAgency
                  serverAgency of

              Await _ server'' -> return (server'' msg)
        continueServer server
    Await (ServerAgency serverAgency) client' -> do
      let awaitServer :: Peer protocol AsServer st m result -> m (result, result)
          awaitServer server' = case server' of
            Effect effectServer -> awaitServer =<< effectServer
            Done noAgency _serverResult ->
              case exclusionLemma_NobodyAndServerHaveAgency noAgency serverAgency of
            Yield _ msg server'' -> simulate (client' msg) server''
            Await (ClientAgency clientAgency) _ ->
              case exclusionLemma_ClientAndServerHaveAgency
                clientAgency
                serverAgency of

      awaitServer server