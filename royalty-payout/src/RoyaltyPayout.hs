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
  , smooshTheBall
  , testTracing
  , smoosh
  , explode
  , Schema
  , contract
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
import qualified Ledger.Tx                 as Tx

import           Playground.Contract
import qualified Wallet.Emulator.Wallet    as W
import qualified PlutusTx
import           PlutusTx.Prelude          as P hiding (Semigroup (..), unless, Applicative (..))
import           PlutusTx.List             as List

import           Plutus.Contract
import qualified Plutus.Trace.Emulator     as Trace

import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Contexts as Contexts

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
data CustomRedeemerType = CustomRedeemerType {}
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType

-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ datum _ context = checkAllPayments (cdtRoyaltyPKHs datum) (cdtPayouts datum)
  where
    info :: TxInfo
    info = Contexts.scriptContextTxInfo context

    -- First signer is the single signer
    txSigner :: PubKeyHash
    txSigner = List.head $ txInfoSignatories info

    checkValuePaidTo :: PubKeyHash -> Integer -> Bool
    checkValuePaidTo x y = Contexts.valuePaidTo info x P./= Ada.lovelaceValueOf y
    
    checkAllPayments :: [PubKeyHash] -> [Integer] -> Bool
    checkAllPayments [] [] = checkValuePaidTo txSigner (cdtProfit datum)
    checkAllPayments [] _  = checkValuePaidTo txSigner (cdtProfit datum)
    checkAllPayments _  [] = checkValuePaidTo txSigner (cdtProfit datum)
    checkAllPayments (x:xs) (y:ys)
      | checkValuePaidTo x y = False
      | otherwise            = checkAllPayments xs ys
    
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
validator = Scripts.validatorScript (typedValidator ContractParams {})

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
checkDatumInputs pkhs amts pro = do
  { let a = traceIfFalse "Found Duplicates."     $ P.length (List.nub pkhs) P.== P.length pkhs    -- no duplicates
  ; let b = traceIfFalse "Lengths Not Equal"     $ P.length pkhs P.== P.length amts               -- equal pkhs + vals
  ; let c = traceIfFalse "A Payout Is Too Small" $ checkMinPayout amts                            -- min utxo required
  ; let d = traceIfFalse "Empty Data"            $ P.length pkhs P./= 0 P.&& P.length amts P./= 0 -- no empty entries
  ; let e = traceIfFalse "Profit Is Too Small."  $ pro P.== 0 P.|| pro P.>= minimumAmt            -- either no profit or min utxo
  ; P.all (P.==(True :: Bool)) [a,b,c,d,e]
  }

-- | The buy endpoint.
smoosh :: AsContractError e => Promise () Schema e ()
smoosh =  endpoint @"smoosh" @CustomDatumType $ \(CustomDatumType cdtRoyaltyPKHs cdtPayouts cdtProfit) -> do
  logInfo @Prelude.String "smoosh the ada ball"
  -- Check the datum inputs
  if checkDatumInputs cdtRoyaltyPKHs cdtPayouts cdtProfit
  then do
    logInfo @Prelude.String "Add the fee"
    let totalPlusFee = sumList cdtPayouts P.+ cdtProfit
    logInfo @Prelude.String "submit the tx"
    void $ submitTxConstraints (typedValidator $ ContractParams {}) $ Constraints.mustPayToTheScript (CustomDatumType cdtRoyaltyPKHs cdtPayouts cdtProfit) (Ada.lovelaceValueOf totalPlusFee)
  else
    logInfo @Prelude.String "The ball has crumbled"

-- | The remove endpoint.
explode :: AsContractError e => Promise () Schema e ()
explode =  endpoint @"explode" @CustomDatumType $ \(CustomDatumType cdtRoyaltyPKHs cdtPayouts cdtProfit) -> do
  logInfo @Prelude.String "explode the ada ball"
  scrOutputs        <- utxosAt $ Ledger.scriptAddress validator
  miner             <- Plutus.Contract.ownPubKeyHash
  let mandoPayout   = Ada.lovelaceValueOf cdtProfit
  let saleDatum     = CustomDatumType {cdtRoyaltyPKHs=cdtRoyaltyPKHs, cdtPayouts=cdtPayouts, cdtProfit=cdtProfit}
  let flt _ ciTxOut = P.either P.id Ledger.datumHash (Tx._ciTxOutDatum ciTxOut) P.== Ledger.datumHash (Datum (PlutusTx.toBuiltinData saleDatum))
  let tx            = collectFromScriptFilter flt scrOutputs (CustomRedeemerType {}) Prelude.<> createTX cdtRoyaltyPKHs cdtPayouts miner mandoPayout
  void $ submitTxConstraints (typedValidator $ ContractParams {}) tx
  
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
testTracing :: IO ()
testTracing = Trace.runEmulatorTraceIO smooshTheBall

smoosher :: Wallet
smoosher = W.knownWallet 0

sPkh :: PubKeyHash
sPkh = W.walletPubKeyHash smoosher

exploder :: Wallet
exploder = W.knownWallet 1

ePkh :: PubKeyHash
ePkh = W.walletPubKeyHash smoosher

smooshTheBall :: Trace.EmulatorTrace ()
smooshTheBall = do
  hdl <- Trace.activateContractWallet smoosher (contract @ContractError)
  let datum = CustomDatumType { cdtRoyaltyPKHs = [sPkh, ePkh], cdtPayouts = [4567890, 2344570], cdtProfit = 1234567 }
  Trace.callEndpoint @"smoosh" hdl datum
  void $ Trace.waitNSlots 1