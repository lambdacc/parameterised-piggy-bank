{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE NoImplicitPrelude             #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE TypeOperators                 #-}
{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Plutus.Contracts.PiggyBank2 (endpoints, PiggyBank2Schema, MyRedeemer (..)) where

import           Control.Monad        hiding (fmap)
import           Data.Map             as Map hiding (empty)
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Plutus.Contract
import           PlutusTx             (toBuiltinData)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import           Text.Printf          (printf)

newtype MyRedeemer = MyRedeemer Bool
    deriving (FromJSON, ToJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''MyRedeemer [('MyRedeemer, 0)]

{-# INLINABLE mkValidator #-}
mkValidator :: () -> MyRedeemer -> ScriptContext -> Bool
mkValidator _ (MyRedeemer isValid) ctx =
    isValid &&
    hasSufficientAmount

    where
      ownOutput :: TxOut
      ownOutput = case getContinuingOutputs ctx of
          [o] -> o
          _   -> traceError "No utxo output found"

      hasSufficientAmount :: Bool
      hasSufficientAmount =
        let
          outVal = txOutValue ownOutput
        in
          (Ada.getLovelace (Ada.fromValue outVal)) > 100000000000



data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed    = ()
    type instance RedeemerType Typed = MyRedeemer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @MyRedeemer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type PiggyBank2Schema =
            Endpoint "put" Integer
        .\/ Endpoint "empty" MyRedeemer

put :: AsContractError e => Integer -> Contract w s e ()
put amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "Put %d lovelaces in the piggy bank" amount

empty :: forall w s e. AsContractError e => MyRedeemer -> Contract w s e ()
empty r = do
    utxos <- utxoAt scrAddress
    let orefs   = fst <$> Map.toList utxos     
        lookups = Constraints.unspentOutputs utxos <> Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ toBuiltinData r | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "Emptied piggy bank."

put' :: Promise () PiggyBank2Schema Text ()
put' = endpoint @"put" put

empty' :: Promise () PiggyBank2Schema Text ()
empty' = endpoint @"empty" empty

endpoints :: AsContractError e => Contract () PiggyBank2Schema Text e
endpoints = do
    logInfo @String "Waiting for put or empty."
    selectList [put', empty'] >>  endpoints

-- these functions are used in the simulator
mkSchemaDefinitions ''PiggyBank2Schema
mkKnownCurrencies []
