{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Monad              (void)
import           Data.Aeson                 (ToJSON, FromJSON)
import           GHC.Generics               (Generic)
import           Ledger
import           Ledger.TimeSlot            (slotToEndPOSIXTime)
import           Ledger.Typed.Scripts       (ValidatorTypes(..))
import           Plutus.Contract
import           Plutus.Trace.Emulator      as Emulator
import           Plutus.V2.Ledger.Api       hiding (Validator)
import qualified Plutus.V2.Ledger.Api       as PlutusV2
import           PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, Semigroup(..), Show(..), String)
import qualified Prelude

--------------------------------------------------
-- Data Types
--------------------------------------------------

data WalletDatum = WalletDatum
  { signers  :: [PubKeyHash]
  , required :: Integer
  , deadline :: POSIXTime
  } deriving Show

PlutusTx.unstableMakeIsData ''WalletDatum
PlutusTx.makeLift ''WalletDatum

newtype WalletRedeemer = Spend [PubKeyHash]
  deriving Show

PlutusTx.unstableMakeIsData ''WalletRedeemer
PlutusTx.makeLift ''WalletRedeemer

--------------------------------------------------
-- Validator
--------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: WalletDatum -> WalletRedeemer -> ScriptContext -> Bool
mkValidator dat (Spend sigs) ctx =
  let
    info = scriptContextTxInfo ctx
    validSignatures = length (filter (\pkh -> txSignedBy info pkh && pkh `elem` signers dat) sigs)
    timeOk = contains (from $ deadline dat) (txInfoValidRange info)
  in
    traceIfFalse "Not enough signatures" (validSignatures >= required dat) &&
    traceIfFalse "Deadline not reached" timeOk

data MultiSig
instance ValidatorTypes MultiSig where
    type DatumType MultiSig = WalletDatum
    type RedeemerType MultiSig = WalletRedeemer

typedValidator :: TypedValidator MultiSig
typedValidator = mkTypedValidator @MultiSig
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator

validator :: Validator
validator = validatorScript typedValidator

valHash :: ValidatorHash
valHash = validatorHash typedValidator

scrAddress :: Address
scrAddress = scriptAddress validator

--------------------------------------------------
-- Testing in Emulator
--------------------------------------------------

main :: IO ()
main = runEmulatorTraceIO testTrace

testTrace :: EmulatorTrace ()
testTrace = do
  let wallet1 = knownWallet 1
      wallet2 = knownWallet 2
      wallet3 = knownWallet 3
      wallet4 = knownWallet 4

  let pkh1 = mockWalletPaymentPubKeyHash wallet1
      pkh2 = mockWalletPaymentPubKeyHash wallet2
      pkh3 = mockWalletPaymentPubKeyHash wallet3

  h1 <- activateContractWallet wallet1 endpoints

  let datum = WalletDatum
                { signers  = [unPaymentPubKeyHash pkh1, unPaymentPubKeyHash pkh2, unPaymentPubKeyHash pkh3]
                , required = 2
                , deadline = slotToEndPOSIXTime def 5
                }

      redeemer = Spend [unPaymentPubKeyHash pkh1, unPaymentPubKeyHash pkh2]

  -- Deposit
  callEndpoint @"deposit" h1 (datum, Ada.lovelaceValueOf 10_000_000)

  void $ Emulator.waitUntilSlot 6

  -- Spend
  callEndpoint @"spend" h1 (datum, redeemer)

  void $ Emulator.waitNSlots 1

--------------------------------------------------
-- Contract endpoints
--------------------------------------------------

type MultiSigSchema =
        Endpoint "deposit" (WalletDatum, Value)
    .\/ Endpoint "spend" (WalletDatum, WalletRedeemer)

endpoints :: Contract () MultiSigSchema Text ()
endpoints = awaitPromise (deposit' `select` spend') >> endpoints
  where
    deposit' = endpoint @"deposit" $ \(d, v) -> deposit d v
    spend'   = endpoint @"spend" $ \(d, r) -> spend d r

deposit :: WalletDatum -> Value -> Contract w s Text ()
deposit datum value = do
  let tx = mustPayToTheScript datum value
  void $ submitTxConstraints typedValidator tx

spend :: WalletDatum -> WalletRedeemer -> Contract w s Text ()
spend datum redeemer = do
  utxos <- utxosAt scrAddress
  let orefs = fst <$> Map.toList utxos
      lookups = Constraints.unspentOutputs utxos <>
                Constraints.otherScript validator
      tx :: TxConstraints (RedeemerType MultiSig) (DatumType MultiSig)
      tx = mconcat [mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData redeemer) | oref <- orefs]
  void $ submitTxConstraintsWith @MultiSig lookups tx
