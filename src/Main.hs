{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Codec.Serialise as S
import Core
import Core.Run
import Data.Kind
import Data.Void
import Protocol
import System.Environment (getArgs)

main :: IO ()
main = do
  [arg] <- getArgs
  case arg of
    's' : _ -> runServer (ProtocolVersion 0) (\(ProtocolVersion _) -> SomePeer codec server)
    'c' : _ -> runClient (ProtocolVersion 1) (\(ProtocolVersion _) -> SomePeer codec client)
    _ -> putStrLn "Usage: cabal run -- (c[lient]|s[erver])"
