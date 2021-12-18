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
  , smooshFail
  , explodeTheBall
  , smoosh
  , explode
  , Schema
  , contract
  , CustomDatumType(..)
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
import qualified Ledger.CardanoWallet      as CW
import           Playground.Contract


import qualified PlutusTx
import           PlutusTx.Prelude          as P hiding (Semigroup (..), unless, Applicative (..))
import           PlutusTx.List             as List
import           PlutusTx.Builtins         as Bi

import           Plutus.Contract
import qualified Plutus.Trace.Emulator     as Trace

import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Contexts as Contexts
import qualified Plutus.V1.Ledger.Value    as Value

import           Wallet.Emulator.Wallet    as W
{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0

  cardano-cli 1.32.1 - linux-x86_64 - ghc-8.10
  git rev 4f65fb9a27aa7e3a1873ab4211e412af780a3648

  Smoosh some Ada into a Ball so someone else can explode it later.
-}

-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
    { cdtRoyaltyPKHs    :: ![PubKeyHash]
    -- ^ List of the public key hashes
    , cdtPayouts        :: ![Integer]
    -- ^ List of the payouts in lovelace.
    , cdtProfit         :: !Integer
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
newtype CustomRedeemerType = CustomRedeemerType
  { crtData :: Integer }
    deriving stock (Prelude.Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
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

    -- First signer is the single signer always.
    txSigner :: PubKeyHash
    txSigner = List.head $ txInfoSignatories info

    -- Value paid to has to be exact ada but this causes issues with validation so we use Value.geq which also its own issues.
    checkValuePaidTo :: PubKeyHash -> Integer -> Bool
    checkValuePaidTo pkh amt = traceIfFalse "pay is incorrect." $ Value.geq (Contexts.valuePaidTo info pkh) (Ada.lovelaceValueOf amt)
    
    -- Loop the pkh and amount lists, checking each case.
    checkAllPayments :: [PubKeyHash] -> [Integer] -> Bool
    checkAllPayments []     []     = checkValuePaidTo txSigner (cdtProfit datum)
    checkAllPayments []     _      = checkValuePaidTo txSigner (cdtProfit datum)
    checkAllPayments _      []     = checkValuePaidTo txSigner (cdtProfit datum)
    checkAllPayments (x:xs) (y:ys)
      | checkValuePaidTo x y       = checkAllPayments xs ys
      | otherwise                  = False

    -- Force a single utxo has a datum hash in the inputs by checking the length of the inputs that have a datum hash.
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

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [smoosh, explode] >> contract

minimumAmt :: Integer
minimumAmt = 1234567 -- probably should be the real min utxo

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
smoosh =  endpoint @"smoosh" @CustomDatumType $ \(CustomDatumType {cdtRoyaltyPKHs=cdtRoyaltyPKHs, cdtPayouts=cdtPayouts, cdtProfit=cdtProfit}) -> do
  logInfo @Prelude.String "smoosh the ada ball"
  miner <- Plutus.Contract.ownPubKeyHash
  logInfo @Prelude.String "The Smoosher"
  logInfo @PubKeyHash miner
  logInfo @Prelude.String "The Data"
  logInfo @[PubKeyHash] cdtRoyaltyPKHs
  logInfo @[Integer] cdtPayouts
  logInfo @Integer cdtProfit
  -- Check the datum inputs
  if checkDatumInputs cdtRoyaltyPKHs cdtPayouts cdtProfit
  then do
    logInfo @Prelude.String "adding the profit"
    let totalPlusFee = sumList cdtPayouts P.+ cdtProfit
    logInfo @Prelude.String "submitting the tx"
    let inst  = typedValidator $ ContractParams {}
    let datum = CustomDatumType {cdtRoyaltyPKHs=cdtRoyaltyPKHs, cdtPayouts=cdtPayouts, cdtProfit=cdtProfit}
    let newValue = Ada.lovelaceValueOf totalPlusFee
    _ <- submitTxConstraints inst $ Constraints.mustPayToTheScript datum newValue
    logInfo @Prelude.String "the ball is smooshed"
  else
    logInfo @Prelude.String "the ball has crumbled due to bad inputs."

-- | The remove endpoint.
explode :: AsContractError e => Promise () Schema e ()
explode =  endpoint @"explode" @CustomDatumType $ \(CustomDatumType {cdtRoyaltyPKHs=cdtRoyaltyPKHs, cdtPayouts=cdtPayouts, cdtProfit=cdtProfit}) -> do
  logInfo @Prelude.String "explode the ada ball"
  scrOutputs        <- utxosAt $ Scripts.validatorAddress $ typedValidator $ ContractParams {}
  miner             <- Plutus.Contract.ownPubKeyHash
  let mandoPayout   = Ada.lovelaceValueOf cdtProfit
  logInfo @Prelude.String "The Miner"
  logInfo @PubKeyHash miner
  logInfo @Value mandoPayout
  logInfo @Prelude.String "Find The TX"
  logInfo @Prelude.String "Unique Payouts"
  logInfo @Integer $ length cdtPayouts
  -- collect from the script and build the constraints.
  let txOut = collectFromScript scrOutputs (CustomRedeemerType { crtData = 0 }) Prelude.<> createTX cdtRoyaltyPKHs cdtPayouts miner mandoPayout
      inst = typedValidator $ ContractParams {}
      lookups = Constraints.typedValidatorLookups inst Prelude.<> Constraints.unspentOutputs scrOutputs
  logInfo @Prelude.String "submitting the tx"
  _ <- submitTxConstraintsWith lookups txOut
  logInfo @Prelude.String "the ball has exploded"
  
-- | sum a list
sumList :: [Integer] -> Integer
sumList = List.foldr (P.+) 0

-- create must pay to pubkeys
createTX :: [PubKeyHash] -> [Integer] -> PubKeyHash -> Value -> Constraints.TxConstraints i o
createTX []     []     pkh val = Constraints.mustPayToPubKey pkh val
createTX []     _      pkh val = Constraints.mustPayToPubKey pkh val
createTX _      []     pkh val = Constraints.mustPayToPubKey pkh val
createTX (x:xs) (y:ys) pkh val = Constraints.mustPayToPubKey x (Ada.lovelaceValueOf y) Prelude.<> createTX xs ys pkh val

-------------------------------------------------------------------------------
-- | TRACES
-------------------------------------------------------------------------------
{- | All the wallets will be involved in this but two of them will be selected
as the smoosher and exploder. The smoosher will attempt to smoosh a ball. The
smooshing can fail because the datum is checked to ensure validity. The exploder
will explode the ball, resulting in payments for everyone including themselves.
Each ball has a profit attached to it to incentivize exploding smooshed ada balls.

@See smooshTheBall, smooshFail, explodeTheBall
-}
smoosher :: CW.MockWallet
smoosher = CW.knownWallet 1

exploder :: CW.MockWallet
exploder = CW.knownWallet 2

sPkh :: PubKeyHash
sPkh = CW.pubKeyHash smoosher

ePkh :: PubKeyHash
ePkh = CW.pubKeyHash exploder

groupPkhs :: Integer -> [PubKeyHash]
groupPkhs c = groupPkhs' c []
  where
    groupPkhs' :: Integer -> [PubKeyHash] -> [PubKeyHash]
    groupPkhs' 0 pkhs = pkhs
    groupPkhs' c' pkhs = groupPkhs' (c' P.- 1) (CW.pubKeyHash (CW.knownWallet c') : pkhs)

-- | Convert a base 10 integer into base q as a list of integers then add 2 and multiply by 1.2 ADA.
baseQ :: Integer -> Integer -> [Integer]
baseQ _      0 = [0]
baseQ number 1 = [1200000 P.* number]
baseQ number base = P.map (1200000 P.*) $ P.map (2 P.+) $ baseQ' number base []
  where
  baseQ' 0 _base list = list
  baseQ' number' base' list = baseQ' (Bi.divideInteger number' base') base' (Bi.modInteger number' base' : list)

numDigits :: Integer -> Integer -> Integer
numDigits number base = base Prelude.^ (number P.- 1)

groupPayouts :: Integer -> [Integer]
groupPayouts users = baseQ (numDigits users users) users

-- users
numberOfUsers :: Integer
numberOfUsers = 10

listOfPKHs :: [PubKeyHash]
listOfPKHs    = groupPkhs numberOfUsers

listOfPayouts :: [Integer]
listOfPayouts = groupPayouts numberOfUsers

-- IO calls for the repl
smooshTheBall :: IO ()
smooshTheBall = Trace.runEmulatorTraceIO smooshTheBall'
  where 
    smooshTheBall' = do
      hdl1 <- Trace.activateContractWallet (W.toMockWallet smoosher) (contract @ContractError)
      let datum = CustomDatumType { cdtRoyaltyPKHs = listOfPKHs, cdtPayouts = listOfPayouts, cdtProfit = 1234567 }
      Trace.callEndpoint @"smoosh" hdl1 datum
      void $ Trace.waitNSlots 1

explodeTheBall :: IO ()
explodeTheBall = Trace.runEmulatorTraceIO explodeTheBall'
  where 
    explodeTheBall' = do
      let datum = CustomDatumType { cdtRoyaltyPKHs = listOfPKHs, cdtPayouts = listOfPayouts, cdtProfit = 1234567 }
      hdl2 <- Trace.activateContractWallet (W.toMockWallet exploder) (contract @ContractError)
      Trace.callEndpoint @"explode" hdl2 datum
      void $ Trace.waitNSlots 1

smooshFail :: IO ()
smooshFail = Trace.runEmulatorTraceIO smooshFail'
  where 
    smooshFail' = do
      hdl1 <- Trace.activateContractWallet (W.toMockWallet smoosher) (contract @ContractError)
      let datum = CustomDatumType { cdtRoyaltyPKHs = listOfPKHs, cdtPayouts = listOfPayouts, cdtProfit = 0 } -- no profit
      Trace.callEndpoint @"smoosh" hdl1 datum
      void $ Trace.waitNSlots 1