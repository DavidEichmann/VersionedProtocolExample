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

module Protocol.V1 where

import qualified Codec.Serialise as S
import Core
import Core.Run
import Data.Proxy
import qualified Data.Text as T
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

-- client :: Peer MyProtocol AsClient StIdle IO ()
-- client = Effect $ do
--   putStrLn "Ping"
--   return (Yield (ClientAgency TokIdle) Ping goPinged_0)
--   where
--     recvPinged_,
--       recvPinged_2 ::
--         Message MyProtocol StPinged st -> Peer MyProtocol AsClient st IO ()
--     recvPinged_ Pong = Effect $ do
--       putStrLn "Pong"
--       putStrLn "Ping"
--       return (Yield (ClientAgency TokIdle) Ping goPinged_)
--     recvPinged_2 Pong = Effect $ do
--       putStrLn "Pong"
--       putStrLn "Done"
--       return (Yield (ClientAgency TokIdle) Stop goDone)

--     goPinged_0, goPinged_ :: Peer MyProtocol AsClient StPinged IO ()
--     goPinged_0 = Await (ServerAgency TokPinged) recvPinged_
--     goPinged_ = Await (ServerAgency TokPinged) recvPinged_2

--     goDone :: Peer MyProtocol AsClient StDone IO ()
--     goDone = Done TokDone ()

client :: Peer MyProtocol AsClient StIdle IO ()
client = Effect $ do
  putStrLn "Ping"
  return (Yield (ClientAgency TokIdle) Ping goPinged_0)
  where
    recvPinged_1,
      recvPinged_2 ::
        Message MyProtocol StPinged st -> Peer MyProtocol AsClient st IO ()
    recvPinged_1 Pong = Effect $ do
      putStrLn "Pong"
      putStrLn "Ping"
      return (Yield (ClientAgency TokIdle) Ping goPinged_1)
    recvPinged_2 Pong = Effect $ do
      putStrLn "Pong"
      putStrLn "Trying to upgrade to V2 protocol and use Echo"
      return $
        TryChangeVersion
          (Proxy @(V2.MyProtocol))
          -- TODO there is a subtle implication that the V1 and V2 StIdle are in
          -- correspondance!
          ( Effect $ do
              putStrLn "SUCCESS upgraded to V2 protocol... doing Echo"
              return $
                Yield
                  (ClientAgency V2.TokIdle)
                  (V2.Echo "Versioned protocols are cool")
                  ( Await
                      (ServerAgency V2.TokEchoed)
                      ( \msgV2 -> case msgV2 of
                          V2.EchoResp respText -> Effect $ do
                            putStrLn $ "V2 Echo response: " ++ T.unpack respText
                            -- Change back to V1 protocol
                            return $
                              TryChangeVersion
                                (Proxy @MyProtocol)
                                ( Yield
                                    -- TODO there is a subtle implication that
                                    -- the V1 and V2 StIdle are in
                                    -- correspondance!
                                    (ClientAgency TokIdle)
                                    Stop
                                    goDone
                                )
                                (error "TODO we started at V1 so it should always be possible to return to V1! how can we express this statically?")
                      )
                  )
          )
          ( Effect $ do
              putStrLn "FAILED to upgrade to V2 protocol... skipping Echo"
              return $ Yield (ClientAgency TokIdle) Stop goDone
          )

    goPinged_0, goPinged_1 :: Peer MyProtocol AsClient StPinged IO ()
    goPinged_0 = Await (ServerAgency TokPinged) recvPinged_1
    goPinged_1 = Await (ServerAgency TokPinged) recvPinged_2

    goDone :: Peer MyProtocol AsClient StDone IO ()
    goDone = Effect $ do
      putStrLn "Done"
      return $ Done TokDone ()

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
    recvIdle Stop = goDone

    goDone :: Peer MyProtocol AsServer StDone IO ()
    goDone = Done TokDone ()
