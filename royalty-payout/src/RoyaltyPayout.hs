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
  , makeSaleTrace
  , smoosh
  , explode
  , Schema
  , contract
  ) where

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import Codec.Serialise ( serialise )
import           Control.Monad        (void)

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS

import qualified Prelude                   hiding (($))


import           Ledger                    hiding (singleton)
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Ledger.Tx                 as Tx

import           Playground.Contract
import           Plutus.Contract.Trace     as X

import qualified PlutusTx
import           PlutusTx.Prelude          as P hiding (Semigroup (..), unless)
import           PlutusTx.Prelude          hiding (Applicative (..))
import           PlutusTx.List             as List

import           Plutus.Contract
import           Plutus.Trace.Emulator     (EmulatorTrace)
import qualified Plutus.Trace.Emulator     as Trace
import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Contexts as Contexts

{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0

  cardano-cli 1.31.0 - linux-x86_64 - ghc-8.10
  git rev 2cbe363874d0261bc62f52185cf23ed492cf4859

  Assume pure ADA and correct off chain.

  It is the job of off chain code to create correct datum and tx.

  A single pubkey can only be used once so sum the total ada the address
  needs and send it as once entry.
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

    checkValuePaidTo :: PubKeyHash -> Integer -> Bool
    checkValuePaidTo x y = Contexts.valuePaidTo info x P./= Ada.lovelaceValueOf y
    
    checkAllPayments :: [PubKeyHash] -> [Integer] -> Bool
    checkAllPayments [] [] = True
    checkAllPayments [] _  = True
    checkAllPayments _  [] = True
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
minimumAmt = 1234567

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
  { let a = traceIfFalse "Found Duplicates."     $ P.length (List.nub pkhs) P.== P.length pkhs
  ; let b = traceIfFalse "Lengths Not Equal"     $ P.length pkhs P.== P.length amts
  ; let c = traceIfFalse "A Payout Is Too Small" $ checkMinPayout amts
  ; let d = traceIfFalse "Empty Data"            $ P.length pkhs P./= 0 P.&& P.length amts P./= 0
  ; let e = traceIfFalse "Profit Is Too Small."  $ pro P.>= minimumAmt
  ; P.all (P.==(True :: Bool)) [a,b,c,d,e]
  }

-- | The buy endpoint.
smoosh :: AsContractError e => Promise () Schema e ()
smoosh =  endpoint @"smoosh" @CustomDatumType $ \(CustomDatumType cdtRoyaltyPKHs cdtPayouts cdtProfit) -> do
  logInfo @Prelude.String "smoosh the ada ball"
  if checkDatumInputs cdtRoyaltyPKHs cdtPayouts cdtProfit
  then do
    let totalPlusFee = sumList cdtPayouts P.+ cdtProfit
    void $ submitTxConstraints (typedValidator $ ContractParams {}) $ Constraints.mustPayToTheScript (CustomDatumType cdtRoyaltyPKHs cdtPayouts cdtProfit) (Ada.lovelaceValueOf totalPlusFee)
  else
    traceError "Datum Values Are Incorrect."

-- | The remove endpoint.
explode :: AsContractError e => Promise () Schema e ()
explode =  endpoint @"explode" @CustomDatumType $ \(CustomDatumType cdtRoyaltyPKHs cdtPayouts cdtProfit) -> do
  scrOutputs        <- utxosAt $ Ledger.scriptAddress validator
  miner             <- Plutus.Contract.ownPubKeyHash
  let mandoPayout   = Ada.lovelaceValueOf cdtProfit
  let saleDatum     = CustomDatumType {cdtRoyaltyPKHs=cdtRoyaltyPKHs, cdtPayouts=cdtPayouts, cdtProfit=cdtProfit}
  let flt _ ciTxOut = P.either P.id Ledger.datumHash (Tx._ciTxOutDatum ciTxOut) P.== Ledger.datumHash (Datum (PlutusTx.toBuiltinData saleDatum))
  let tx            = collectFromScriptFilter flt scrOutputs (CustomRedeemerType {}) PlutusTx.Prelude.<> createTX cdtRoyaltyPKHs cdtPayouts miner mandoPayout
  void $ submitTxConstraints (typedValidator $ ContractParams {}) tx
  
-- | sum a list
sumList :: [Integer] -> Integer
sumList = List.foldr (P.+) 0

-- create a list of must pay to pubkeys
createTX :: [PubKeyHash] -> [Integer] -> PubKeyHash -> Value -> Constraints.TxConstraints i o
createTX [] [] pkh val = Constraints.mustPayToPubKey pkh val
createTX [] _ pkh val  = Constraints.mustPayToPubKey pkh val
createTX _ [] pkh val  = Constraints.mustPayToPubKey pkh val
createTX (x:xs) (y:ys) _pkh _val  = Constraints.mustPayToPubKey x (Ada.lovelaceValueOf y) PlutusTx.Prelude.<> createTX xs ys _pkh _val

--------- TRACES -------
makeSaleTrace :: EmulatorTrace ()
makeSaleTrace = do
  let _ = X.knownWallet 1
  void $ Trace.waitNSlots 1
    -- Trace.callEndpoint @"sell" hdl (CustomDatumType (Ada.lovelaceValueOf 2000000) (Ada.lovelaceValueOf 10000000) False (walletPubKeyHash w1)  (Ada.lovelaceValueOf 125000000))
  --     w2 = X.knownWallet 2
  --     secret = "secret"

  -- h1 <- Trace.activateContractWallet w1 (lock @ContractError)
  -- void $ Trace.waitNSlots 1
  -- Trace.callEndpoint @"locking" h1 (LockParams secret (Ada.adaValueOf 10))

  -- h2 <- Trace.activateContractWallet w2 (guess @ContractError)
  -- void $ Trace.waitNSlots 1
  -- Trace.callEndpoint @"guessing" h2 (GuessParams secret)
  -- void $ Trace.waitNSlots 1