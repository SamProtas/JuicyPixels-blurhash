{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Blurhash where

import Control.Monad (void)

import Codec.Picture
import System.FilePath
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as V

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.HUnit

import qualified Codec.Picture.Blurhash as BH


imgDir :: FilePath
imgDir = takeDirectory __FILE__ </> ".." </> "imgs"

readAsset :: FilePath -> IO DynamicImage
readAsset fp = either assertFailure pure =<< (readImage $ imgDir </> fp)

unit_testEncodeKnownHashPurePython :: IO ()
unit_testEncodeKnownHashPurePython = do
  img <- readAsset "cool_cat.jpg"
  hash <- either (assertFailure . show) pure $ BH.encodeDynamic img
  assertEqual "Correct hash" "UBMOZfK1GG%LBBNG,;Rj2skq=eE1s9n4S5Na" hash

unit_testBadXComponents :: IO ()
unit_testBadXComponents = do
  img <- readAsset "cool_cat.jpg"
  let res = BH.encodeDynamicWithConfig BH.encodeConfigDefault {BH.componentsX = 0} img
  case res of Left BH.InvalidComponents -> pure ()
              _ -> assertFailure $ "Unexpected response" <> show res

unit_testBadYComponents :: IO ()
unit_testBadYComponents = do
  img <- readAsset "cool_cat.jpg"
  let res = BH.encodeDynamicWithConfig BH.encodeConfigDefault {BH.componentsY = 10} img
  case res of Left BH.InvalidComponents -> pure ()
              _ -> assertFailure $ "Unexpected response " <> show res


unit_testBadHashLength :: IO ()
unit_testBadHashLength = do
  let res = BH.decodeRGB8 "bogus"
  case res of Left BH.InvalidHashLength -> pure ()
              _ -> assertFailure $ "Unexpected response " <> either show showImgRGB8 res

unit_testInvalidCharacter :: IO ()
unit_testInvalidCharacter = do
  let res = BH.decodeRGB8 "``````````````````````"
  case res of
    Left (BH.InvalidCharacterError char) -> assertEqual "Correct byte" (BS.singleton char) "`"
    _ -> assertFailure $ "Unexpected response " <> either show showImgRGB8 res

genValidEncodeConfig :: Gen BH.EncodeConfig
genValidEncodeConfig = do
  compX <- genValidComponent
  compY <- genValidComponent
  pure $ BH.encodeConfigDefault { BH.componentsX = compX, BH.componentsY = compY }

genInvalidEncodeConfigWith :: Gen Int -> Gen Int -> Gen BH.EncodeConfig
genInvalidEncodeConfigWith genX genY = do
  compX <- genX
  compY <- genY
  pure BH.encodeConfigDefault { BH.componentsX = compX, BH.componentsY = compY }

genInvalidEncodeConfig :: Gen BH.EncodeConfig
genInvalidEncodeConfig =
  Gen.frequency [ (4, genInvalidEncodeConfigWith genInvalidComponent genValidComponent)
                , (4, genInvalidEncodeConfigWith genValidComponent genInvalidComponent)
                , (2, genInvalidEncodeConfigWith genInvalidComponent genInvalidComponent)]

genValidComponent :: Gen Int
genValidComponent = Gen.integral $ Range.linear 1 9

genInvalidComponent :: Gen Int
genInvalidComponent =
  Gen.choice [Gen.integral $ Range.linear (-10) 0, Gen.integral $ Range.linear 1 9]

genValidDecodeConfig :: Gen BH.DecodeConfig
genValidDecodeConfig = do
  punch <- Gen.float $ Range.linearFrac 0.5 3
  width <- Gen.integral $ Range.linear 10 50
  height <- Gen.integral $ Range.linear 10 50
  pure $
    BH.decodeConfigDefault { BH.punch = punch, BH.outputWidth = width, BH.outputHeight = height}

genRGB8 :: MonadGen m => m PixelRGB8
genRGB8 = PixelRGB8 <$> Gen.enumBounded <*> Gen.enumBounded <*> Gen.enumBounded

genValidRGB8Img :: Int -> Int -> Gen (Image PixelRGB8)
genValidRGB8Img width height = do
  imgData <- V.fromList <$>
             Gen.list (Range.singleton $ width * height) Gen.enumBounded 
  pure $ Image width height imgData
  
hprop_validComponentsValidBlur :: Property
hprop_validComponentsValidBlur = property $ do

  inConfig <- forAll genValidEncodeConfig
  width <- forAll $ Gen.integral $ Range.linear 10 100
  height <- forAll $ Gen.integral $ Range.linear 10 100
  img <- forAllWith showImgRGB8 $ genValidRGB8Img width height

  blurhash <- evalEither $ BH.encodeRGB8WithConfig inConfig img

  outConfig <- forAll genValidDecodeConfig

  void $ evalEither $ BH.decodeRGB8WithConfig outConfig blurhash


hprop_encodeRGB8DoesNotThrow :: Property
hprop_encodeRGB8DoesNotThrow = property $ do
  config <- forAll $ Gen.choice [genInvalidEncodeConfig, genInvalidEncodeConfig]
  
  imgData <- forAll $ Gen.list (Range.linear 0 400) Gen.enumBounded

  w <- forAll $ Gen.integral $ Range.linear 0 100
  h <- forAll $ Gen.integral $ Range.linear 0 100

  let img = Image w h (V.fromList imgData)

  case BH.encodeRGB8WithConfig config img of
    Left BH.InvalidComponents -> label "Invalid components"
    Left (BH.B83EncodingError _ _) -> label "Base83 encoding error"
    Right _ -> label "Encoded"


hprop_encodeLinearDoesNotThrow :: Property
hprop_encodeLinearDoesNotThrow = property $ do
  config <- forAll $ Gen.choice [genInvalidEncodeConfig, genInvalidEncodeConfig]
  
  imgData <- forAll $ Gen.list (Range.linear 0 400) (Gen.float (Range.linearFrac (-100) 100))

  w <- forAll $ Gen.integral $ Range.linear 0 100
  h <- forAll $ Gen.integral $ Range.linear 0 100

  let img = Image w h (V.fromList imgData)
  case BH.encodeLinearWithConfig config img of
    Left BH.InvalidComponents -> label "Invalid components"
    Left (BH.B83EncodingError _ _) -> label "Base83 encoding error"
    Right _ -> label "Encoded"


hprop_decodeDoesNotThrow :: Property
hprop_decodeDoesNotThrow = property $ do
  blurhash <- forAll $ Gen.bytes $ Range.linear 0 1000
  config <- forAll $ genValidDecodeConfig
  case BH.decodeRGB8WithConfig config $ BS.fromStrict blurhash of
    Left BH.InvalidHashLength -> label "Invalid hash length"
    Left (BH.InvalidCharacterError _) -> label "Invalid char error"
    Right _ -> label "Decoded"
  


showImgRGB8 :: Image PixelRGB8 -> String
showImgRGB8 img =
  "Width: " ++ (show $ imageWidth img) ++
  ", Height: " ++ (show $ imageWidth img) ++
  ", Bytes: " ++ (show $ V.length $ imageData img)
