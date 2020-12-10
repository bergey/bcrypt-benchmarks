{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Monad.IO.Class
import Hedgehog
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as Tasty (testProperty)
import qualified Crypto.BCrypt as B
import qualified Crypto.KDF.BCrypt as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = defaultMain $ testGroup "BCrypt"
  [ testGroup "chosen plaintext"
    [ knownCase 4 "password"
    , knownCase 4 "Simspace1!"
    ]
  , testGroup "hedgehog"
    [ testProperty "ascii-4" (bcryptEquivalent 4 asciiPrintable)
    , testProperty "unicode-4" (bcryptEquivalent 4 Gen.unicode)
    , testProperty "ascii-12" (bcryptEquivalent 12 asciiPrintable)
    , testProperty "unicode-12" (bcryptEquivalent 12 Gen.unicode)
    ]
  ]

asciiPrintable :: Gen Char
asciiPrintable = Gen.frequency
  [ (26+26+10, Gen.alphaNum)
  , (length symbols, Gen.element symbols)
  ]
  where symbols = " !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

bcryptEquivalent :: Int -> Gen Char -> Property
bcryptEquivalent cost char = property do
  password <- forAll $ Gen.utf8 (Range.linear 1 128) char
  cryptonite <- liftIO $ C.hashPassword cost password
  -- Can't just compute hashes two ways, because salt!
  -- bcrypt <- liftIO $ B.hashPasswordUsingPolicy (B.HashingPolicy cost "$2b$") password
  Hedgehog.assert (B.validatePassword cryptonite password)

knownCase :: Int -> String -> TestTree
knownCase cost password = testCase password $ do
  let bs = T.encodeUtf8 . T.pack $ password
  cryptonite <- liftIO $ C.hashPassword cost bs
  -- bcrypt <- liftIO $ B.hashPasswordUsingPolicy (B.HashingPolicy cost "$2b$") bs
  -- assertEqual "knownCase" (Just cryptonite) bcrypt
  assertBool "validate" (B.validatePassword cryptonite bs)
