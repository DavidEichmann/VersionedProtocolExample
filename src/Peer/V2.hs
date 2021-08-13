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

module Peer.V2 where

import Core
import Protocol.V2

client :: Peer MyProtocol AsClient StIdle IO ()
client = Effect $ do
  putStrLn "Ping"
  return (Yield (ClientAgency TokIdle) Ping goPinged)
  where
    recvPinged :: Message MyProtocol StPinged st -> Peer MyProtocol AsClient st IO ()
    recvPinged Pong = Effect $ do
      putStrLn "Pong"
      putStrLn "Echo \"boop\""
      return (Yield (ClientAgency TokIdle) (Echo "boop") goEchoed)

    recvEchoed :: Message MyProtocol StEchoed st -> Peer MyProtocol AsClient st IO ()
    recvEchoed (EchoResp str) = Effect $ do
      putStrLn $ "EchoResp" ++ show str
      return (Yield (ClientAgency TokIdle) Stop goDone)

    goPinged :: Peer MyProtocol AsClient StPinged IO ()
    goPinged = Await (ServerAgency TokPinged) recvPinged

    goEchoed :: Peer MyProtocol AsClient StEchoed IO ()
    goEchoed = Await (ServerAgency TokEchoed) recvEchoed

    goDone :: Peer MyProtocol AsClient StDone IO ()
    goDone = Done TokDone ()

server :: Peer MyProtocol AsServer StIdle IO ()
server = goIdle
  where
    goIdle :: Peer MyProtocol AsServer StIdle IO ()
    goIdle = Await (ClientAgency TokIdle) recvIdle

    recvIdle :: Message MyProtocol StIdle st -> Peer MyProtocol AsServer st IO ()
    recvIdle Ping = Effect $ do
      putStrLn "Ping"
      putStrLn "Pong"
      return (Yield (ServerAgency TokPinged) Pong goIdle)
    recvIdle (Echo str) = Effect $ do
      putStrLn $ "Echo " ++ show str
      putStrLn $ "EchoResp " ++ show str
      return (Yield (ServerAgency TokEchoed) (EchoResp str) goIdle)
    recvIdle Stop = goDone

    goDone :: Peer MyProtocol AsServer StDone IO ()
    goDone = Done TokDone ()
