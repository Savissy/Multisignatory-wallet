module Multisig where

import Data.Time.Clock.POSIX (POSIXTime)
import Data.List (nub)
import Plutus.V2.Ledger.Api  -- Add Plutus imports
import Plutus.V2.Ledger.Contexts
import PlutusTx (unstableMakeIsData)
import PlutusTx.Prelude hiding (nub)

-- Enable serialization for custom types
unstableMakeIsData ''WalletDatum
unstableMakeIsData ''WalletRedeemer

data WalletDatum = WalletDatum
  { signers  :: [PubKeyHash]  -- Changed to PubKeyHash
  , required :: Integer       -- Changed to Integer
  , deadline :: POSIXTime
  } deriving Show

newtype WalletRedeemer = Spend [PubKeyHash]  -- Changed to PubKeyHash
  deriving Show

validate :: WalletDatum -> WalletRedeemer -> ScriptContext -> Bool  -- Changed context type
validate (WalletDatum allowed required deadline) (Spend claimed) ctx =
    let actualSigners = txInfoSignatories $ scriptContextTxInfo ctx
        currentTime = case ivTo (txInfoValidRange $ scriptContextTxInfo ctx) of
            Just t  -> t
            Nothing -> 0  -- Handle always valid case
        
        valid = filter (\s -> s `elem` allowed && s `elem` actualSigners) claimed
        uniqueValid = nub valid
    in length uniqueValid >= required && currentTime >= deadline

-- Add test helper functions below
-- ===============================

-- Mock public key hashes for testing
alice :: PubKeyHash
alice = "alice_pkh"  -- In real tests, generate properly

bob :: PubKeyHash
bob = "bob_pkh"

charlie :: PubKeyHash
charlie = "charlie_pkh"

-- Create test datum
testDatum :: WalletDatum
testDatum = WalletDatum
  { signers = [alice, bob, charlie]
  , required = 2
  , deadline = 1700000000  -- POSIX timestamp
  }

-- Create test redeemer
testRedeemer :: WalletRedeemer
testRedeemer = Spend [alice, bob]

-- Create a test context
testContext :: [PubKeyHash] -> POSIXTime -> ScriptContext
testContext signers time = ScriptContext
  { scriptContextTxInfo = TxInfo
      { txInfoSignatories = signers
      , txInfoValidRange = to time
      , txInfoInputs = []
      , txInfoOutputs = []
      , txInfoFee = mempty
      , txInfoMint = mempty
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoData = []
      , txInfoId = "test_tx_id"
      }
  , scriptContextPurpose = Spending (TxOutRef "test" 0)
  }

-- Test function
runTest :: Bool
runTest = validate testDatum testRedeemer (testContext [alice, bob] 1700000001)
