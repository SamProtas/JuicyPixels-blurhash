module Main where

import Control.Exception

import Codec.Picture
import qualified Data.ByteString.Lazy.Char8 as BS
import Options.Applicative

import Codec.Picture.Blurhash
import Codec.Picture.Blurhash.Internal.Encode
import Codec.Picture.Blurhash.Internal.Decode


main :: IO ()
main = execParser cliParser >>= execCommand

execCommand :: Command -> IO ()
execCommand (Encode input config) = encodeFromCommand input config >>= BS.putStrLn
execCommand (Decode blurhash config output) = decodeFromCommand blurhash config output
execCommand (Blur input inConfig outConfig output) = do
  blurhash <- encodeFromCommand input inConfig
  decodeFromCommand blurhash outConfig output

encodeFromCommand :: FilePath -> EncodeConfig -> IO BS.ByteString
encodeFromCommand fp config = do
  imgE <- readImage fp
  img <- either throwStr pure imgE
  either (throwStr . show) pure $ encodeDynamicWithConfig config img


decodeFromCommand :: BS.ByteString -> DecodeConfig -> FilePath -> IO ()
decodeFromCommand blurhash config output = do
  img <- either (throwStr . show) pure $ decodeRGB8WithConfig config blurhash
  writePng output img
  

cliParser :: ParserInfo Command
cliParser =
  info
  (commandParser  <**> helper)
  (fullDesc <> progDesc "Simple CLI for using blurhash-hs library")


commandParser :: Parser Command
commandParser =
  subparser (command "encode" (info encodeParser (progDesc "")) <>
             command "decode" (info decodeParser (progDesc "")) <>
             command "blur" (info blurParser (progDesc "")))
  

encodeParser :: Parser Command
encodeParser = Encode <$> inputPathParser <*> encodingConfigParser

decodeParser :: Parser Command
decodeParser = Decode <$> blurhashParser <*> decodingConfigParser <*> outputPathParser

blurParser :: Parser Command
blurParser =
  Blur <$>
  inputPathParser <*>
  encodingConfigParser <*>
  decodingConfigParser <*>
  outputPathParser

outputPathParser :: Parser FilePath
outputPathParser = strOption (long "output-path" <> value "blurred.png" <> showDefault)

inputPathParser :: Parser FilePath
inputPathParser = strOption (long "input-path")

blurhashParser :: Parser BS.ByteString
blurhashParser = strOption (long "blurhash")

encodingConfigParser :: Parser EncodeConfig
encodingConfigParser =
  EncodeConfig <$>
  option auto (long "componentsX" <> value (componentsX encodeConfigDefault) <> showDefault) <*>
  option auto (long "componentsY" <> value (componentsY encodeConfigDefault) <> showDefault)

decodingConfigParser :: Parser DecodeConfig
decodingConfigParser =
  DecodeConfig <$>
  option auto (long "punch" <> value (punch decodeConfigDefault) <> showDefault) <*>
  option auto (long "width" <> value (outputWidth decodeConfigDefault) <> showDefault) <*>
  option auto (long "height" <> value (outputWidth decodeConfigDefault) <> showDefault)


data Command
  = Encode FilePath EncodeConfig 
  | Decode BS.ByteString DecodeConfig FilePath
  | Blur FilePath EncodeConfig DecodeConfig FilePath


throwStr :: String -> IO a
throwStr = throwIO . StrException

data StrException = StrException String deriving Show
instance Exception StrException
