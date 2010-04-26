{-# Language OverloadedStrings #-}

module Command (
  Command(..),
  parse,
  ) where

import Control.Exception
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Functor
import System.IO

data Command
  = Set ByteString Int Int ByteString
  | Add ByteString Int Int ByteString
  | Replace ByteString Int Int ByteString
  | Append ByteString Int Int ByteString
  | Prepend ByteString Int Int ByteString
  | Cas
  | Get [ByteString]
  | Delete ByteString
  | Incr ByteString Integer
  | Decr ByteString Integer
  | Stats
  | Version
  | Quit
  deriving (Eq, Show)

parse :: Handle -> IO (Either String Command)
parse h = do
  res <- try $ parse' h
  return $ case res of
    Left (SomeException e) -> Left $ show e
    Right cmd -> Right cmd
  
parse' h = do
  cmd <- BS.hGetLine h
  putStrLn $ show cmd
  case BS.words cmd of
    "set" : key : flags : exptime : bytes : _ ->
      Set key (readInt' flags) (readInt' exptime) <$> getStr h bytes
    "add" : key : flags : exptime : bytes : _ ->
      Add key (readInt' flags) (readInt' exptime) <$> getStr h bytes
    "replace" : key : flags : exptime : bytes : _ ->
      Replace key (readInt' flags) (readInt' exptime) <$> getStr h bytes
    "append" : key : flags : exptime : bytes : _ ->
      Append key (readInt' flags) (readInt' exptime) <$> getStr h bytes
    "prepend" : key : flags : exptime : bytes : _ ->
      Prepend key (readInt' flags) (readInt' exptime) <$> getStr h bytes

    "get" : keys ->
      return $ Get keys
    "delete" : key : _ ->
      return $ Delete key
    "incr" : key : num : _ ->
      return $ Incr key (readInteger' num)
    "decr" : key : num : _ ->
      return $ Decr key (readInteger' num)
    "stats" : _ ->
      return Stats
    "version" : _ ->
      return Version
    "quit" : _ ->
      return Quit

getStr h bytes =
  BS.hGet h (readInt' bytes)

readInt' bs =
  let Just (r, "") = BS.readInt bs in r
readInteger' bs =
  let Just (r, "") = BS.readInteger bs in r
