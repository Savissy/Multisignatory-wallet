module Multisig where

import Data.Time.Clock.POSIX (POSIXTime)
import Data.List (nub)

data WalletDatum = WalletDatum
  { signers  :: [String]
  , required :: Int
  , deadline :: POSIXTime
  } deriving Show

newtype WalletRedeemer = Spend [String]
  deriving Show

data TxContext = TxContext
  { actualSigners :: [String]
  , currentTime   :: POSIXTime
  } deriving Show

validate :: WalletDatum -> WalletRedeemer -> TxContext -> Bool
validate (WalletDatum allowed required deadline) (Spend claimed) (TxContext actual now) =
    let valid = filter (\s -> s `elem` allowed && s `elem` actual) claimed
        uniqueValid = nub valid
    in length uniqueValid >= required && now >= deadline
