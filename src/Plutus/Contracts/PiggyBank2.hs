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

module Plutus.Contracts.PiggyBank2
(inValue,checkAmount,endpoints, PiggyBank2Schema, MyRedeemer (..)) where

import           Control.Monad        hiding (fmap)
import           Data.Map             as Map hiding (empty)
import           Data.Text            (Text, unpack)
import           Data.Monoid          (Last (..))
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
import           Data.Text.Prettyprint.Doc.Extras (PrettyShow (..))
import           Prelude              (Semigroup (..), Show (..))
import           Plutus.Contract       as Contract
import           Plutus.V1.Ledger.Value (Value (..), assetClass, assetClassValueOf)

newtype MyRedeemer = MyRedeemer Bool
    deriving (FromJSON, ToJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''MyRedeemer [('MyRedeemer, 0)]

{-# INLINABLE mkValidator #-}
mkValidator :: () -> MyRedeemer -> ScriptContext -> Bool
mkValidator _ (MyRedeemer isValid) ctx =
    isValid &&
    hasSufficientAmount

    where
      hasSufficientAmount :: Bool
      hasSufficientAmount =
          traceIfFalse "Sorry. Not enough lovelace" $ checkAmount $ inValue ctx

{-# INLINABLE inValue #-}
inValue :: ScriptContext -> Value
inValue ctx = valueSpent (scriptContextTxInfo ctx)

{-# INLINABLE checkAmount #-}
checkAmount :: Value -> Bool
checkAmount val = (assetClassValueOf val $ assetClass Ada.adaSymbol Ada.adaToken) > 1000000

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
        .\/ Endpoint "inspect" MyRedeemer

put :: AsContractError e => Integer -> Contract w s e ()
put amount = do
    utxos <- utxoAt scrAddress
    let totalVal = Plutus.foldMap (txOutValue . txOutTxOut) $ snd <$> Map.toList utxos
        numInputs = Map.size utxos
    logInfo @String $ "Putting to piggy bank currently holding "
            ++ show numInputs
            ++ " outputs with a total value of "
            ++ show totalVal
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "Put %d lovelaces in the piggy bank" amount

empty :: forall w s e. AsContractError e => MyRedeemer -> Contract w s e ()
empty r = do
    utxos <- utxoAt scrAddress
    let totalVal = Plutus.foldMap (txOutValue . txOutTxOut) $ snd <$> Map.toList utxos
        numInputs = Map.size utxos
    logInfo @String
        $ "Attempting to empty piggy bank currently holding "
            <> show numInputs
            <> " outputs with a total value of "
            <> show totalVal
    let orefs   = fst <$> Map.toList utxos     
        lookups = Constraints.unspentOutputs utxos <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ toBuiltinData r | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    handleError (\err -> Contract.logInfo $ "caught error: " ++ unpack err) $ void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "Emptied piggy bank."

inspect :: forall w s e. AsContractError e => MyRedeemer -> Contract w s e ()
inspect r = do
    utxos <- utxoAt scrAddress
    let totalVal = Plutus.foldMap (txOutValue . txOutTxOut) $ snd <$> Map.toList utxos
        numInputs = Map.size utxos
    logInfo @String
        $ "Inspeting utxos at script address"
            <> show numInputs
            <> " outputs with a total value of "
            <> show totalVal
            <> " with total Ada quantity of "
            <> show (assetClassValueOf totalVal $ assetClass Ada.adaSymbol Ada.adaToken)
    logInfo @String $ "Inspect complete"

put' :: Promise () PiggyBank2Schema Text ()
put' = endpoint @"put" put

empty' :: Promise () PiggyBank2Schema Text ()
empty' = endpoint @"empty" empty

inspect' :: Promise () PiggyBank2Schema Text ()
inspect' = endpoint @"inspect" inspect

endpoints :: AsContractError e => Contract () PiggyBank2Schema Text e
endpoints = do
    logInfo @String "Waiting for put or empty."
    selectList [put', empty', inspect'] >>  endpoints

-- these functions are used in the simulator
mkSchemaDefinitions ''PiggyBank2Schema
mkKnownCurrencies []
