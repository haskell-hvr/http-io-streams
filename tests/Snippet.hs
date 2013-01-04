--
-- Profiling code: exercise http-streams
--
-- Copyright © 2012-2013 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

import Network.Http.Client
import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Network.URI (parseURI)
import Control.Monad
import System.Environment (getArgs)


--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.UTF8 as S
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import Debug.Trace
import System.Exit (exitSuccess)


main :: IO ()
main = do
    as <- getArgs
    let a = head as
    let n = read a :: Int
    forM_ (replicate n True) (\_ -> basic)


basic :: IO ()
basic = do
    c <- openConnection "research.laptop" 80
    
    q <- buildRequest c $ do
        http GET "/~andrew/talks/TheWebProblem,SolvingItInHaskell/"
        setAccept "text/plain"
    putStr $ show q
    
    p <- sendRequest c q emptyBody
    putStr $ show p
    
    b <- receiveResponse c p
    
    x <- Streams.read b

    S.putStr $ fromMaybe "" x

    closeConnection c
