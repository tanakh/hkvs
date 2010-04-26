{-# Language OverloadedStrings #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Functor
import Data.Maybe
import Network
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Timeout
import Text.Printf

import Prelude hiding (catch)

import Command
import Store

defaultTimeout = 10
defaultPort = 1234

data Flag
  = ListenPort Int
  | Timeout Int
  | Help
  deriving (Eq, Show)

options =
  [ Option ['p'] ["portnum"] (ReqArg (ListenPort . read) "PORT") "port-number PORT"
  , Option ['t'] ["timeout"] (ReqArg (Timeout . read) "TIMEOUT") "timeout TIMEOUT sec"
  , Option ['h'] ["help"] (NoArg Help) "show help message"
  ]

parseOpts argv =
  case getOpt Permute options argv of
    (flags, _, [])
      | Help `elem` flags -> error $ usageInfo header options
      | otherwise -> return flags
    (_, _, errs) -> error $ unlines errs ++ usageInfo header options
  where
    header = "Usage: hkvs [OPTION...]"

main :: IO ()
main = do
  argv <- getArgs
  fs <- parseOpts argv
  let portNum = head $ [ fromIntegral p | ListenPort p <- fs] ++ [ defaultPort ]
  let timeout = head $ [ t | Timeout t <- fs] ++ [ defaultTimeout ]
  runServer portNum timeout

runServer portNum timeoutSecond = withSocketsDo $ do
  st <- newStore
  
  printf "listen on port %s\n" (show portNum)
  ssock <- listenOn (PortNumber portNum)
  forever $ do
    (h, host, port) <- accept ssock
    forkIO $ do
      ret <- finally
             (timeout (timeoutSecond * 10^6) $ process h host port st)
             (hClose h)
      when (isNothing ret) $ do
        printf "timeout\n"

process h host port st = do
  printf "connect from %s:%s\n" host (show port)
  cmd <- parse h
  case cmd of
    Left err -> do
      hPutStr h $ "CLIENT_ERROR " ++ err ++ "\r\n"
    Right cmd -> do
      result <- execCommand cmd st
      printResult h result
      `catch` \(SomeException e) -> do
        hPutStr h $ "SERVER_ERROR " ++ (show e) ++ "\r\n"
