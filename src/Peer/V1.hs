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

module Peer.V1 where

import Core
import qualified Data.Text as T
import Protocol.V1
import qualified Protocol.V2 as V2

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
        UpgradeVersion
          upgradePath
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
                              DowngradeVersion
                                upgradePath
                                ( Yield
                                    -- TODO there is a subtle implication that
                                    -- the V1 and V2 StIdle are in
                                    -- correspondance!
                                    (ClientAgency TokIdle)
                                    Stop
                                    goDone
                                )
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
