{-# LANGUAGE OverloadedStrings #-}
module BingTranslate (
    detectLanguage,
    getLanguages,
    translateText
  ) where

import Network.HTTP.Conduit
import Data.List
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.ByteString.Lazy (ByteString)
import System.IO
import Data.Char
import Control.Applicative
import Text.XML.Light
import System.Environment
import Data.Text.Encoding (decodeUtf8)

base = "https://api.datamarket.azure.com/Bing/MicrosoftTranslator/v1"

getlanguagesUri = base ++ "/GetLanguagesForTranslation"
detectUri       = base ++ "/Detect"
translateUri    = base ++ "/Translate"

key = "get-your-own"

langRequest :: IO (Request m)
langRequest = applyBasicAuth "" key <$> parseUrl getlanguagesUri

detectRequest :: String -> IO (Request m)
detectRequest text =
  applyBasicAuth "" key <$> parseUrl (detectUri ++ "?Text=\'" ++ text ++ "\'")

translateRequest :: String -> String -> String -> IO (Request m)
translateRequest text from to =
  applyBasicAuth "" key <$>
    parseUrl (translateUri ++ "?Text=\'" ++ text ++ "\'"
                           ++ "&From=\'" ++ from ++ "\'"
                           ++ "&To=\'"   ++ to ++ "\'")

-- | Returns the list of languages that the translation service supports
getLanguages :: IO [String]
getLanguages = do
  lbs <- getURL =<< langRequest
  return (getCodes (parseXML lbs))

-- | Detect the language of the given string
detectLanguage :: String -> IO String
detectLanguage text = do
  lbs <- getURL =<< detectRequest text
  case getCodes (parseXML lbs) of
    [one] -> return one
    _ -> fail "detect"

-- | Translate some text
translateText
  :: String      -- ^ The text to translate
  -> String      -- ^ The language to translate from
  -> String      -- ^ The language to translate to
  -> IO String   -- ^ Returns: the translated text

translateText text from to = do
  lbs <- getURL =<< translateRequest text from to
  case getText (parseXML (decodeUtf8 (B.concat (L.toChunks lbs)))) of
    [one] -> return one
    _ -> fail "translate"


-- getURL :: Request  -> IO ByteString
getURL request =
  withManager $ \manager ->
    responseBody <$> httpLbs request manager

getCodes :: [Content] -> [String]
getCodes [Elem el] =
  [ str | code <- filterElementsName (\n -> qName n == "Code") el
        , Text (CData {cdData = str}) <- elContent code
  ]
geCodes _ = []

getText :: [Content] -> [String]
getText [Elem el] =
  [ str | code <- filterElementsName (\n -> qName n == "Text") el
        , Text (CData {cdData = str}) <- elContent code
  ]
getText _ = []
