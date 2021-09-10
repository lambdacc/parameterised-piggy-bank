{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE NoImplicitPrelude             #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE TypeOperators                 #-}
{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Plutus.Contracts.ParameterisedPiggyBank
(inValue,checkAmount,endpoints, ParameterisedPiggyBankSchema, {-MyRedeemer (..)-}) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map hiding (empty)
import           Data.Text            (Text, unpack)
import           Data.Monoid          (Last (..))
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (toBuiltinData)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import           Text.Printf          (printf)
import           Data.Text.Prettyprint.Doc.Extras (PrettyShow (..))
import           Prelude              (Semigroup (..), Show (..))
import           Plutus.Contract       as Contract
import           Plutus.V1.Ledger.Value (Value (..), assetClass, assetClassValueOf)


data PPBParam = PPBParam
    { beneficiary :: PubKeyHash
    } deriving Show

PlutusTx.makeLift ''PPBParam
{-
newtype MyRedeemer = MyRedeemer Bool
    deriving (FromJSON, ToJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''MyRedeemer [('MyRedeemer, 0)]-}

{-# INLINABLE mkValidator #-}
mkValidator :: PPBParam -> () -> () -> ScriptContext -> Bool
mkValidator p () () ctx =
    {-isValid &&-}
    hasSufficientAmount &&
    signedByBeneficiary

    where
      contextTxInfo :: TxInfo
      contextTxInfo = scriptContextTxInfo ctx

      hasSufficientAmount :: Bool
      hasSufficientAmount =
          traceIfFalse "Sorry. Not enough lovelace" $ checkAmount $ inValue contextTxInfo

      signedByBeneficiary :: Bool
      signedByBeneficiary = txSignedBy contextTxInfo $ beneficiary p

{-# INLINABLE inValue #-}
inValue :: TxInfo -> Value
inValue txInfo = valueSpent txInfo

{-# INLINABLE checkAmount #-}
checkAmount :: Value -> Bool
checkAmount val = (assetClassValueOf val $ assetClass Ada.adaSymbol Ada.adaToken) > 1000000

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed    = ()
    type instance RedeemerType Typed = ()

typedValidator :: PPBParam -> Scripts.TypedValidator Typed
typedValidator p = Scripts.mkTypedValidator @Typed
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @()

validator :: PPBParam -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: PPBParam ->  Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

scrAddress :: PPBParam ->  Ledger.Address
scrAddress = scriptAddress . validator

data PutParams = PutParams
    { ppBeneficiary :: !PubKeyHash
    , ppAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
    
type ParameterisedPiggyBankSchema =
            Endpoint "put" PutParams
        .\/ Endpoint "empty" PutParams
        .\/ Endpoint "inspect" PutParams

put :: AsContractError e => PutParams -> Contract w s e ()
put pp =
  do
    let p  = PPBParam { beneficiary = ppBeneficiary pp
                       }
    utxos <- utxoAt $ scrAddress p
    let totalVal = Plutus.foldMap (txOutValue . txOutTxOut) $ snd <$> Map.toList utxos
        numInputs = Map.size utxos
    logInfo @String $ "Putting to piggy bank currently holding "
            ++ show numInputs
            ++ " outputs with a total value of "
            ++ show totalVal
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf $ ppAmount pp
    ledgerTx <- submitTxConstraints (typedValidator p) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "Put %d lovelaces in the piggy bank" <> show $ ppAmount pp

empty :: AsContractError e => PutParams -> Contract w s e ()
empty pp =
  do
    pkh   <- pubKeyHash <$> ownPubKey
    let p  = PPBParam{ beneficiary = pkh
                      }
    utxos <- utxoAt $ scrAddress p
    let totalVal = Plutus.foldMap (txOutValue . txOutTxOut) $ snd <$> Map.toList utxos
        numInputs = Map.size utxos
    logInfo @String
        $ "Attempting to empty piggy bank currently holding "
        <> show numInputs
        <> " outputs with a total value of "
        <> show totalVal
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos <>
                  Constraints.otherScript (validator p)
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ toBuiltinData () | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    handleError (\err -> Contract.logInfo $ "caught error: " ++ unpack err) $ void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "Emptied piggy bank."

inspect :: AsContractError e => PutParams -> Contract w s e ()
inspect pp =
  do
    let p  = PPBParam
                    { beneficiary = ppBeneficiary pp
                    }
    utxos <- utxoAt $ scrAddress p
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

put' :: Promise () ParameterisedPiggyBankSchema Text ()
put' = endpoint @"put" put

empty' :: Promise () ParameterisedPiggyBankSchema Text ()
empty' = endpoint @"empty" empty

inspect' :: Promise () ParameterisedPiggyBankSchema Text ()
inspect' = endpoint @"inspect" inspect

endpoints :: AsContractError e => Contract () ParameterisedPiggyBankSchema Text e
endpoints =
  do
    logInfo @String "Waiting for put or empty."
    selectList [put', empty', inspect'] >>  endpoints


