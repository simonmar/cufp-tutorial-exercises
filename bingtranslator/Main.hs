- | Main entry point to the application.
{-# LANGUAGE PatternGuards #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Text.Printf
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as B
import System.Environment
import Prelude hiding (catch)

import BingTranslate as Bing

main = do
--  [text] <- fmap (fmap (B.unpack . UTF8.fromString)) getArgs
  text <- getLine

  languages <- Bing.getLanguages

  fromLang <- Bing.detectLanguage text
  printf "\"%s\" appears to be in language \"%s\"\n" text fromLang

  forM_ (filter (/= fromLang) languages) $ \toLang -> do
     str <- try $ Bing.translateText text fromLang toLang
     case str of
       Left e -> print (e :: SomeException)
       Right str -> printf "%s: %s\n" toLang str
