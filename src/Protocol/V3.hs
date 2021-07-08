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
