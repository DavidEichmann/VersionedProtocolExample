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

module Peer.V3 where

import Core
import Protocol.V3

client :: Peer MyProtocol AsClient StIdle IO ()
client = goIdle_0
  where
    goIdle_0 :: Peer MyProtocol AsClient StIdle IO ()
    goIdle_0 = Effect $ do
      putStrLn "Ping"
      return (Yield (ClientAgency TokIdle) Ping goPinged)

    goIdle_1 :: Message MyProtocol StPinged st -> Peer MyProtocol AsClient st IO ()
    goIdle_1 Pong = Effect $ do
      putStrLn "Pong"
      putStrLn "Echo \"boop\""
      return (Yield (ClientAgency TokIdle) (Echo "boop") goEchoed)

    goIdle_2 :: Message MyProtocol StEchoed st -> Peer MyProtocol AsClient st IO ()
    goIdle_2 (EchoResp str) = Effect $ do
      putStrLn $ "EchoResp" ++ show str
      return (Yield (ClientAgency TokIdle) (Query (IncrementCount 999)) goQueried_0)

    goIdle_3 :: Message MyProtocol StQueried st -> Peer MyProtocol AsClient st IO ()
    goIdle_3 (Result q result) = Effect $ do
      printResult q result
      return (Yield (ClientAgency TokIdle) (Query GetCount) goQueried_1)

    goIdle_4 :: Message MyProtocol StQueried st -> Peer MyProtocol AsClient st IO ()
    goIdle_4 (Result q result) = Effect $ do
      printResult q result
      return (Yield (ClientAgency TokIdle) Stop goDone)

    goPinged :: Peer MyProtocol AsClient StPinged IO ()
    goPinged = Await (ServerAgency TokPinged) goIdle_1

    goEchoed :: Peer MyProtocol AsClient StEchoed IO ()
    goEchoed = Await (ServerAgency TokEchoed) goIdle_2

    goQueried_0 :: Peer MyProtocol AsClient StQueried IO ()
    goQueried_0 = Await (ServerAgency TokQueried) goIdle_3

    goQueried_1 :: Peer MyProtocol AsClient StQueried IO ()
    goQueried_1 = Await (ServerAgency TokQueried) goIdle_4

    goDone :: Peer MyProtocol AsClient StDone IO ()
    goDone = Done TokDone ()

    printResult :: Query result -> result -> IO ()
    printResult q r = case q of
      IncrementCount {} -> print r
      GetCount -> print r

server :: Peer MyProtocol AsServer StIdle IO ()
server = goIdle 0
  where
    goIdle :: Int -> Peer MyProtocol AsServer StIdle IO ()
    goIdle c = Await (ClientAgency TokIdle) (recvIdle c)

    recvIdle :: Int -> Message MyProtocol StIdle st -> Peer MyProtocol AsServer st IO ()
    recvIdle c Ping = Effect $ do
      putStrLn "Ping"
      putStrLn "Pong"
      return (Yield (ServerAgency TokPinged) Pong (goIdle c))
    recvIdle c (Echo str) = Effect $ do
      putStrLn $ "Echo " ++ show str
      putStrLn $ "EchoResp " ++ show str
      return (Yield (ServerAgency TokEchoed) (EchoResp str) (goIdle c))
    recvIdle c (Query q) = case q of
      IncrementCount i -> Effect $ do
        let c' = c + i
        putStrLn $ "Increment count from " ++ show c ++ " to " ++ show c'
        return (Yield (ServerAgency TokQueried) (Result q ()) (goIdle c'))
      GetCount -> Yield (ServerAgency TokQueried) (Result q c) (goIdle c)
    recvIdle _ Stop = goDone

    goDone :: Peer MyProtocol AsServer StDone IO ()
    goDone = Done TokDone ()
