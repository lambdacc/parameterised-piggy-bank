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

module Plutus.Contracts.ParameterisedPiggyBank where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map hiding (empty)
import           Data.Text            (Text, unpack)
import           Data.Monoid          (Last (..))
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import Plutus.Contract
    ( logInfo,
      awaitTxConfirmed,
      endpoint,
      ownPubKey,
      submitTxConstraints,
      submitTxConstraintsWith,
      utxoAt,
      handleError,
      selectList,
      Endpoint,
      Contract,
      Promise,
      AsContractError,
      type (.\/) )
import           Plutus.Contract.Types
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
import           Prelude              (IO, Semigroup (..), String, Show (..))
import           Text.Printf          (printf)
import           Data.Text.Prettyprint.Doc.Extras (PrettyShow (..))
import           Plutus.Contract       as Contract
import           Plutus.V1.Ledger.Value (Value (..), assetClass, assetClassValueOf)


-- A value which initates 
data PutParams = PutParams
    { ppBeneficiary :: !PubKeyHash
    , ppAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- A beneficiary gets their own script address
newtype BankParam = BankParam
    { beneficiary :: PubKeyHash
    } deriving Show

-- Endpoints of schema
type ParameterisedPiggyBankSchema =
            Endpoint "put" PutParams
        .\/ Endpoint "empty" ()
        .\/ Endpoint "inspect" PutParams

{- 
    Typed validator instance (probably should be PiggyTyped)
    There's no Datum even though you might think there is one from PutParams
-}
data Bank
instance Scripts.ValidatorTypes Bank where
    type instance DatumType Bank    = ()
    type instance RedeemerType Bank = ()

PlutusTx.makeLift ''BankParam

{- 
    here p is to get validate the scriptAddress needs the beneficiary to have signed the current context 
    Remember, no datum or redeemer (since it's checking the person's own signature)
-}
{-# INLINABLE mkValidator #-}
mkValidator :: BankParam -> () -> () -> ScriptContext -> Bool
mkValidator bp () () ctx =
    hasSufficientAmount &&
    signedByBeneficiary

    where
      contextTxInfo :: TxInfo
      contextTxInfo = scriptContextTxInfo ctx

      hasSufficientAmount :: Bool
      hasSufficientAmount =
          traceIfFalse "Sorry. Not enough lovelace" $ checkAmount $ inValue contextTxInfo

      signedByBeneficiary :: Bool
      signedByBeneficiary = txSignedBy contextTxInfo $ beneficiary bp

{-# INLINABLE inValue #-}
inValue :: TxInfo -> Value
inValue = valueSpent

{-# INLINABLE checkAmount #-}
checkAmount :: Value -> Bool
checkAmount val = assetClassValueOf val (assetClass Ada.adaSymbol Ada.adaToken) > 1000000


typedValidator :: BankParam -> Scripts.TypedValidator Bank
typedValidator p = Scripts.mkTypedValidator @Bank
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @()


validator :: BankParam -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: BankParam ->  Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

{-
    scrAddress needs the PPBParam (for the beneficiary!)
-}
scrAddress :: BankParam ->  Ledger.Address
scrAddress = scriptAddress . validator


put :: AsContractError e => PutParams -> Contract w ParameterisedPiggyBankSchema e ()
put pp = do
    -- must create the PPBParam to get the address
    let bp  = BankParam
                    { beneficiary = ppBeneficiary pp
                    }
    
    -- Got the address
    utxos <- utxoAt $ scrAddress bp

    let totalVal = Plutus.foldMap (txOutValue . txOutTxOut) $ snd <$> Map.toList utxos
        numInputs = Map.size utxos
    
    logInfo @String $ "Putting to piggy bank currently holding "
            ++ show numInputs
            ++ " outputs with a total value of "
            ++ show totalVal
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf $ ppAmount pp
    ledgerTx <- submitTxConstraints (typedValidator bp) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "Put %d lovelaces in the piggy bank" <> show $ ppAmount pp

empty :: forall w s e. (AsContractError e)=> () -> Contract w s e () 
empty _ = do
    pkh <- pubKeyHash <$> ownPubKey
    let bp  = BankParam
                        { beneficiary = pkh
                        }
    utxos <- utxoAt $ scrAddress bp
    let totalVal = Plutus.foldMap (txOutValue . txOutTxOut) $ snd <$> Map.toList utxos
        numInputs = Map.size utxos
    logInfo @String $ "Attempting to empty piggy bank currently holding "
        <> show numInputs
        <> " outputs with a total value of "
        <> show totalVal
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos <>
                  Constraints.otherScript (validator bp)
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ toBuiltinData () | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    handleError (\err -> Contract.logInfo $ "caught error: " ++ unpack err) $ void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "Emptied piggy bank."

inspect :: forall w s e. AsContractError e => PutParams -> Contract w s e ()
inspect pp = do
    let bp  = BankParam
                    { beneficiary = ppBeneficiary pp
                    }
    utxos <- utxoAt $ scrAddress bp
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

inspect' :: Promise () ParameterisedPiggyBankSchema Text ()
inspect' = endpoint @"inspect" inspect

empty' :: Promise () ParameterisedPiggyBankSchema Text ()
empty' = endpoint @"empty" empty

endpoints :: AsContractError e => Contract () ParameterisedPiggyBankSchema Text e
endpoints = do
    selectList [put', inspect', empty'] >> endpoints

-- these functions are used in the simulator
mkSchemaDefinitions ''ParameterisedPiggyBankSchema
mkKnownCurrencies []
