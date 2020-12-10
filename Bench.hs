{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion
import Criterion.Main (defaultMain)
import Data.ByteString (ByteString)
import qualified Crypto.BCrypt as B
import qualified Crypto.KDF.BCrypt as C

main :: IO ()
main = defaultMain
  [ bgroup "cost=4" (bcryptWithCost 4)
  , bgroup "cost=8" (bcryptWithCost 8)
  , bgroup "cost=12" (bcryptWithCost 12)
  , bgroup "cost=16" (bcryptWithCost 16)
  -- , bgroup "cost=31" (bcryptWithCost 31)
  ]

bcryptWithCost :: Int -> [Benchmark]
bcryptWithCost cost =
  [ bench "cryptonite" $ nfIO
    (C.hashPassword cost ("Simspace1!" :: ByteString) :: IO ByteString)
  , bench "bcrypt" $ nfIO $
    B.hashPasswordUsingPolicy (B.HashingPolicy cost "$2b$") "Simspace1!"
  ]
