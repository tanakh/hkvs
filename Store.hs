{-# Language OverloadedStrings #-}

module Store (
  Store(..),
  Result(..),
  
  newStore,
  printResult,
  execCommand,
  
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO

import Command

data Store
  = Store
    { dat :: TVar (Map ByteString (Int, ByteString))
    }

data Result
  = Stored
  | NotStored
  | Exists
  | NotFound
  | Values [(ByteString, Int, ByteString)]
  deriving (Eq, Show)

newStore :: IO Store
newStore = do
  dat <- newTVarIO (Map.empty)
  return $ Store dat

printResult :: Handle -> Result -> IO ()
printResult h Stored = do
  BS.hPutStr h $ "STORED\r\n"
printResult h NotStored = do
  BS.hPutStr h $ "NOT_STORED\r\n"
printResult h Exists = do
  BS.hPutStr h $ "EXISTS\r\n"
printResult h NotFound = do
  BS.hPutStr h $ "NOT_FOUND\r\n"

printResult h (Values vs) = do
  forM_ vs $ \(key, flags, val) -> do
    BS.hPutStr h $ BS.unwords ["VALUE", key, showBS flags, showBS (BS.length val)] `BS.append` "\r\n"
    BS.hPutStr h $ val `BS.append` "\r\n"
  BS.hPutStr h $ "END\r\n"

execCommand :: Command -> Store -> IO Result
execCommand (Set key flags exptime val) Store { dat = dat } = atomically $ do
  mm <- readTVar dat
  writeTVar dat $! Map.insert key (flags, val) mm
  return Stored

execCommand (Get keys) Store { dat = dat } = atomically $ do
  mm <- readTVar dat
  return $ Values [ (key, flags, val)
                  | key <- keys
                  , let Just (flags, val) = Map.lookup key mm ]

showBS = BS.pack . show
