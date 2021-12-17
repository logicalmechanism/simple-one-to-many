{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module RoyaltyPayout
  ( royaltyPayoutScript
  , royaltyPayoutScriptShortBs
  , smooshTheBallThenExplode
  , smooshTheBall
  , explodeTheBall
  , smoosh
  , explode
  , Schema
  , contract
  , smoosher
  , sPkh
  , baseQ
  , numDigits
  , payouts
  ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise           ( serialise )
import           Control.Monad             ( void )

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS

import qualified Prelude                   hiding (($))


import           Ledger                    hiding ( singleton )
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
-- import qualified Ledger.Tx                 as Tx
import qualified Ledger.CardanoWallet      as CW
import           Playground.Contract


import qualified PlutusTx
import           PlutusTx.Prelude          as P hiding (Semigroup (..), unless, Applicative (..))
import           PlutusTx.List             as List
import           PlutusTx.Builtins         as Bi

import           Plutus.Contract
import           Plutus.Contract.Test
import qualified Plutus.Trace.Emulator     as Trace

import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Contexts as Contexts
import qualified Plutus.V1.Ledger.Value    as Value

import           Test.Tasty
import           Wallet.Emulator.Wallet    as W
{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0

  cardano-cli 1.32.1 - linux-x86_64 - ghc-8.10
  git rev 4f65fb9a27aa7e3a1873ab4211e412af780a3648

  Ada Balls
-}

-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
    { cdtRoyaltyPKHs    :: ![PubKeyHash]
    -- ^ List of the public key hashes
    , cdtPayouts        :: ![Integer]
    -- ^ List of the payouts in lovelace.
    , cdtProfit         :: Integer
    -- ^ The creator of the ball can define the profit.
  }
    deriving stock (Prelude.Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType

-------------------------------------------------------------------------------
-- | Create the contract parameters data object.
-------------------------------------------------------------------------------
data ContractParams = ContractParams {}
PlutusTx.makeLift ''ContractParams
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = CustomRedeemerType
  {}
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType

-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ datum _ context = oneScriptInput P.&& checkAllPayments (cdtRoyaltyPKHs datum) (cdtPayouts datum)
  where
    info :: TxInfo
    info = Contexts.scriptContextTxInfo context

    -- First signer is the single signer
    txSigner :: PubKeyHash
    txSigner = List.head $ txInfoSignatories info

    -- Value paid to has to be exact ada but this causes issues so use geq which has issues.
    checkValuePaidTo :: PubKeyHash -> Integer -> Bool
    checkValuePaidTo pkh amt = traceIfFalse "pay is incorrect." $ Value.geq (Contexts.valuePaidTo info pkh) (Ada.lovelaceValueOf amt)
    
    checkAllPayments :: [PubKeyHash] -> [Integer] -> Bool
    checkAllPayments []     []     = checkValuePaidTo txSigner (cdtProfit datum)
    checkAllPayments []     _      = checkValuePaidTo txSigner (cdtProfit datum)
    checkAllPayments _      []     = checkValuePaidTo txSigner (cdtProfit datum)
    checkAllPayments (x:xs) (y:ys)
      | checkValuePaidTo x y = checkAllPayments xs ys
      | otherwise            = False

    -- Force a single utxo has a datum hash in the inputs.
    oneScriptInput :: Bool
    oneScriptInput = length (P.mapMaybe txOutDatumHash (P.map txInInfoResolved $ txInfoInputs info)) P.<= 1

        
-------------------------------------------------------------------------------
-- | This determines the data type for Datum and Redeemer.
-------------------------------------------------------------------------------
data Typed
instance Scripts.ValidatorTypes Typed where
  type instance DatumType    Typed = CustomDatumType
  type instance RedeemerType Typed = CustomRedeemerType

-------------------------------------------------------------------------------
-- | Now we need to compile the Typed Validator.
-------------------------------------------------------------------------------
typedValidator :: ContractParams -> Scripts.TypedValidator Typed
typedValidator cp = Scripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cp)
  $$(PlutusTx.compile  [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer

-------------------------------------------------------------------------------
-- | Define The Validator Here
-------------------------------------------------------------------------------
validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator $ ContractParams {})

-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Plutus.Script
script = Plutus.unValidatorScript validator

royaltyPayoutScriptShortBs :: SBS.ShortByteString
royaltyPayoutScriptShortBs = SBS.toShort P.. LBS.toStrict $ serialise script

royaltyPayoutScript :: PlutusScript PlutusScriptV1
royaltyPayoutScript = PlutusScriptSerialised royaltyPayoutScriptShortBs

-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------
-- | The schema of the contract, with two endpoints.
type Schema =
  Endpoint "smoosh"  CustomDatumType .\/ 
  Endpoint "explode" CustomDatumType

minimumAmt :: Integer
minimumAmt = 1234567 -- probably should be the real min utxo


contract :: AsContractError e => Contract () Schema e ()
contract = selectList [smoosh, explode] >> contract

checkMinPayout :: [Integer] -> Bool
checkMinPayout [] = True
checkMinPayout (x:xs)
  | x P.< minimumAmt = False
  | otherwise        = checkMinPayout xs 

-- | Put all the buy functions together here
checkDatumInputs :: [PubKeyHash] -> [Integer] -> Integer -> Bool
checkDatumInputs pkhs amts prof = do
  { let a = traceIfFalse "Found Duplicates."     $ P.length (List.nub pkhs) P.== P.length pkhs    -- no duplicates
  ; let b = traceIfFalse "Lengths Not Equal"     $ P.length pkhs P.== P.length amts               -- equal pkhs + vals
  ; let c = traceIfFalse "A Payout Is Too Small" $ checkMinPayout amts                            -- min utxo required
  ; let d = traceIfFalse "Empty Data"            $ P.length pkhs P./= 0 P.&& P.length amts P./= 0 -- no empty entries
  ; let e = traceIfFalse "Profit Is Too Small."  $ prof P.>= minimumAmt                           -- min utxo
  ; P.all (P.==(True :: Bool)) [a,b,c,d,e]
  }

-- | The buy endpoint.
smoosh :: AsContractError e => Promise () Schema e ()
smoosh =  endpoint @"smoosh" @CustomDatumType $ \(CustomDatumType cdtRoyaltyPKHs cdtPayouts cdtProfit) -> do
  logInfo @Prelude.String "Attempt to smoosh the ball"
  -- Check the datum inputs
  if checkDatumInputs cdtRoyaltyPKHs cdtPayouts cdtProfit
  then do
    logInfo @Prelude.String "add the fee"
    let totalPlusFee = sumList cdtPayouts P.+ cdtProfit
    logInfo @Prelude.String "submit the tx"
    void $ submitTxConstraints (typedValidator $ ContractParams {}) $ Constraints.mustPayToTheScript (CustomDatumType cdtRoyaltyPKHs cdtPayouts cdtProfit) (Ada.lovelaceValueOf totalPlusFee)
  else
    logInfo @Prelude.String "The ball has crumbled due to bad inputs."

-- | The remove endpoint.
explode :: AsContractError e => Promise () Schema e ()
explode =  endpoint @"explode" @CustomDatumType $ \(CustomDatumType cdtRoyaltyPKHs cdtPayouts cdtProfit) -> do
  logInfo @Prelude.String "explode the ada ball"
  scrOutputs        <- utxosAt $ Ledger.scriptAddress validator
  miner             <- Plutus.Contract.ownPubKeyHash
  let mandoPayout   = Ada.lovelaceValueOf cdtProfit
  logInfo @Prelude.String "find the tx"

  -- This may be the path
  -- let saleDatum     = CustomDatumType { cdtRoyaltyPKHs=cdtRoyaltyPKHs, cdtPayouts=cdtPayouts, cdtProfit=cdtProfit }
  -- let flt _ ciTxOut = P.either P.id Ledger.datumHash (Tx._ciTxOutDatum ciTxOut) P.== Ledger.datumHash (Datum (PlutusTx.toBuiltinData saleDatum))
  -- let tx            = collectFromScriptFilter flt scrOutputs (CustomRedeemerType {}) Prelude.<> createTX cdtRoyaltyPKHs cdtPayouts miner mandoPayout
  
  -- this works
  let txOut = collectFromScript scrOutputs (CustomRedeemerType {}) Prelude.<> createTX cdtRoyaltyPKHs cdtPayouts miner mandoPayout
      inst = typedValidator $ ContractParams {}
      lookups = Constraints.typedValidatorLookups inst Prelude.<> Constraints.unspentOutputs scrOutputs
  logInfo @Prelude.String "submit the tx"
  void $ submitTxConstraintsWith lookups txOut
  -- void $ submitTxConstraints (typedValidator $ ContractParams {}) tx -- may be needed
  
-- | sum a list
sumList :: [Integer] -> Integer
sumList = List.foldr (P.+) 0

-- create a list of must pay to pubkeys
createTX :: [PubKeyHash] -> [Integer] -> PubKeyHash -> Value -> Constraints.TxConstraints i o
createTX []     []     pkh val = Constraints.mustPayToPubKey pkh val
createTX []     _      pkh val = Constraints.mustPayToPubKey pkh val
createTX _      []     pkh val = Constraints.mustPayToPubKey pkh val
createTX (x:xs) (y:ys) pkh val = Constraints.mustPayToPubKey x (Ada.lovelaceValueOf y) Prelude.<> createTX xs ys pkh val

-------------------------------------------------------------------------------
-- | TRACES
-------------------------------------------------------------------------------
smoosher :: CW.MockWallet
smoosher = CW.knownWallet 1

exploder :: CW.MockWallet
exploder = CW.knownWallet 2

sPkh :: PubKeyHash
sPkh = CW.pubKeyHash smoosher

ePkh :: PubKeyHash
ePkh = CW.pubKeyHash exploder

payouts :: Integer -> [PubKeyHash]
payouts c = payouts' c []
  where
    payouts' :: Integer -> [PubKeyHash] -> [PubKeyHash]
    payouts' 0 pkhs = pkhs
    payouts' c' pkhs = payouts' (c' P.- 1) (CW.pubKeyHash (CW.knownWallet c') : pkhs)

-- | Convert a base 10 integer into base q as a list of integers then +2 *1,2 ADA
baseQ :: Integer -> Integer -> [Integer]
baseQ number base = P.map (1200000 P.*) $ P.map (2 P.+) $ baseQ' number base []
  where
  baseQ' 0 _base list = list
  baseQ' number' base' list = baseQ' (Bi.divideInteger number' base') base' (Bi.modInteger number' base' : list)

numDigits :: Integer -> Integer -> Integer
numDigits number base = (base Prelude.^ (number P.- 1)) P.+ 20


smooshTheBall :: IO ()
smooshTheBall = Trace.runEmulatorTraceIO smooshTheBall'
  where 
    smooshTheBall' = do
      hdl1 <- Trace.activateContractWallet (W.toMockWallet smoosher) (contract @ContractError)
      let users = 6
      let groupPKHs = payouts users
      let groupPayouts = baseQ (numDigits users users) users
      let datum = CustomDatumType { cdtRoyaltyPKHs = groupPKHs, cdtPayouts = groupPayouts, cdtProfit = 1234567 }
      Trace.callEndpoint @"smoosh" hdl1 datum
      void $ Trace.waitNSlots 1

explodeTheBall :: IO ()
explodeTheBall = Trace.runEmulatorTraceIO explodeTheBall'
  where 
    explodeTheBall' = do
      let users = 10
      let groupPKHs = payouts users
      let groupPayouts = baseQ (numDigits users users) users
      let datum = CustomDatumType { cdtRoyaltyPKHs = groupPKHs, cdtPayouts = groupPayouts, cdtProfit = 1234567 }
      hdl2 <- Trace.activateContractWallet (W.toMockWallet exploder) (contract @ContractError)
      Trace.callEndpoint @"explode" hdl2 datum
      void $ Trace.waitNSlots 1

smooshTheBallThenExplode :: IO ()
smooshTheBallThenExplode = Trace.runEmulatorTraceIO smooshTheBallThenExplode'
  where 
    smooshTheBallThenExplode' = do
      let users = 10
      let groupPKHs = payouts users
      let groupPayouts = baseQ (numDigits users users) users
      let datum = CustomDatumType { cdtRoyaltyPKHs = groupPKHs, cdtPayouts = groupPayouts, cdtProfit = 1234567 }
      hdl1 <- Trace.activateContractWallet (W.toMockWallet smoosher) (contract @ContractError)
      Trace.callEndpoint @"smoosh" hdl1 datum
      _ <-  Trace.waitNSlots 1
      hdl2 <- Trace.activateContractWallet (W.toMockWallet exploder) (contract @ContractError)
      Trace.callEndpoint @"explode" hdl2 datum
      void $ Trace.waitNSlots 1
