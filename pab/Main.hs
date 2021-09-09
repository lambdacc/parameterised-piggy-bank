{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Main(main) where

import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON, defaultOptions, Options(..))
import           Data.Default                        (def)
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Plutus.Contract                     (ContractError)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Plutus.Contracts.ParameterisedPiggyBank          as ParameterisedPiggyBank

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin StarterContracts) "Starting PAB on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    -- Pressing enter results in the balances being printed
    void $ liftIO getLine

    Simulator.logString @(Builtin StarterContracts) "Final balances."
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin StarterContracts) b

    shutdown

data StarterContracts =
    ParameterisedPiggyBankContract
    deriving (Eq, Ord, Show, Generic)

instance ToJSON StarterContracts where
  toJSON = genericToJSON defaultOptions { tagSingleConstructors = True }

instance FromJSON StarterContracts where
  parseJSON = genericParseJSON defaultOptions { tagSingleConstructors = True }

instance Pretty StarterContracts where
    pretty = viaShow

instance Builtin.HasDefinitions StarterContracts where
    getDefinitions = [ParameterisedPiggyBankContract]
    getSchema =  \case
        ParameterisedPiggyBankContract -> Builtin.endpointsToSchemas @ParameterisedPiggyBank.ParameterisedPiggyBankSchema
    getContract = \case
        ParameterisedPiggyBankContract -> SomeBuiltin (ParameterisedPiggyBank.endpoints @ContractError)

handlers :: SimulatorEffectHandlers (Builtin StarterContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)
