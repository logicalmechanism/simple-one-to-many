{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}

module GroupPayout
  ( groupPayoutScript
  , groupPayoutScriptShortBs
  , smooshTheBall
  , smooshWithNoProfit
  , explodeTheBall
  , explodeWithNoProfit
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

import           Prelude                   (String, (^))

import           Ledger                    hiding ( singleton )
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Ledger.CardanoWallet      as CW
import           Playground.Contract
import           Wallet.Emulator.Wallet    as W


import qualified PlutusTx
import           PlutusTx.Prelude
import           PlutusTx.Builtins         as Bi
import           Plutus.Contract
import qualified Plutus.Trace.Emulator     as Trace
import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Value    as Value

{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0

  cardano-cli 1.33.0 - linux-x86_64 - ghc-8.10
  git rev 814df2c146f5d56f8c35a681fe75e85b905aed5d

  Smoosh some Ada into a ball so someone else can explode it later. A lightweight
  smart contract solution for pure ADA group payouts. The protocol parameters
  safely allow for a maximum of ~10 users per group. If a group has more than the
  maximum allowed users then the group payment will be split into subgroups.
-}

-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
    { cdtGroupPKHs :: ![PubKeyHash]
    -- ^ List of the public key hashes
    , cdtPayouts   :: ![Integer]
    -- ^ List of the payouts in lovelace.
  }
    deriving stock (Show, Generic)
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
  { crtData :: Integer } -- Dummy Integer
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType

-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ datum _ context = oneScriptInput && checkAllPayments (cdtGroupPKHs datum) (cdtPayouts datum)
  where
    -- Get the Tx Info
    info :: TxInfo
    info = scriptContextTxInfo context

    -- Value paid to has to be exact ada but this causes issues with validation so we use Value.geq which also its own issues.
    checkValuePaidTo :: PubKeyHash -> Integer -> Bool
    checkValuePaidTo pkh amt = Value.geq (valuePaidTo info pkh) (Ada.lovelaceValueOf amt)

    -- Loop the pkh and amount lists, checking each case.
    checkAllPayments :: [PubKeyHash] -> [Integer] -> Bool
    checkAllPayments []     []     = True
    checkAllPayments []     _      = True
    checkAllPayments _      []     = True
    checkAllPayments (x:xs) (y:ys)
      | checkValuePaidTo x y = checkAllPayments xs ys
      | otherwise            = traceIfFalse "A Member Of The Group Is Not Being Paid." $ False

    -- Force a single utxo has a datum hash in the inputs by checking the length of the inputs that have a datum hash.
    oneScriptInput :: Bool
    oneScriptInput = traceIfFalse "More Than One Script Input Is Being Validated." $ loopInputs (txInfoInputs info) 0
      where
        loopInputs :: [TxInInfo] -> Integer -> Bool
        loopInputs []     !counter = counter == 1
        loopInputs (x:xs) !counter = case txOutDatumHash $ txInInfoResolved x of
            Nothing -> do
              if counter > 1
                then loopInputs [] counter
                else loopInputs xs counter
            Just _  -> do
              if counter > 1
                then loopInputs [] counter
                else loopInputs xs (counter + 1)

        
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

groupPayoutScriptShortBs :: SBS.ShortByteString
groupPayoutScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

groupPayoutScript :: PlutusScript PlutusScriptV1
groupPayoutScript = PlutusScriptSerialised groupPayoutScriptShortBs

-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------
{- | This off chain is for the pab and for testing on the mock chain and the repl.
The endpoints are a work in progress. For live testnet testing use the cli.

There are only two things that happen in this contract. Someone is smooshing an ada
ball together for an ada explosion or someone is exploding a smooshed ada ball. A
nice feature of this offchain solution is prohibiting incorrect smoosh datum values.

The minimum required ada @see minimumAmt needs to be set to the real value in production.
-}
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
  | x < minimumAmt = False
  | otherwise        = checkMinPayout xs 

-- | Put all the buy functions together here
checkDatumInputs :: [PubKeyHash] -> [Integer] -> Bool
checkDatumInputs pkhs amts = do
  { let a = traceIfFalse "Found Duplicates."     $ length (nub pkhs) == length pkhs     -- no duplicates
  ; let b = traceIfFalse "Lengths Not Equal"     $ length pkhs == length amts           -- equal pkhs + vals
  ; let c = traceIfFalse "A Payout Is Too Small" $ checkMinPayout amts                  -- min utxo required
  ; let d = traceIfFalse "Empty Data"            $ length pkhs /= 0 && length amts /= 0 -- no empty entries
  ; all (==(True :: Bool)) [a,b,c,d]
  }

-- | The buy endpoint.
smoosh :: AsContractError e => Promise () Schema e ()
smoosh =  endpoint @"smoosh" @CustomDatumType $ \CustomDatumType {cdtGroupPKHs=cdtGroupPKHs, cdtPayouts=cdtPayouts} -> do
  miner <- Plutus.Contract.ownPubKeyHash
  -- log some stuff
  logInfo @String "smoosh the ada ball"
  logInfo @String "The Smoosher"
  logInfo @PubKeyHash miner
  logInfo @String "The Data"
  logInfo @[PubKeyHash] cdtGroupPKHs
  logInfo @[Integer] cdtPayouts
  -- Check the datum inputs
  if checkDatumInputs cdtGroupPKHs cdtPayouts
  then do
    logInfo @String "submitting the tx"
    let totalPlusFee = sumList cdtPayouts
        inst         = typedValidator $ ContractParams {}
        datum        = CustomDatumType {cdtGroupPKHs=cdtGroupPKHs, cdtPayouts=cdtPayouts}
        newValue     = Ada.lovelaceValueOf totalPlusFee
        constraint   = Constraints.mustPayToTheScript datum newValue
    ledgerTx <- submitTxConstraints inst constraint
    _        <- awaitTxConfirmed $ Ledger.getCardanoTxId ledgerTx
    logInfo @String "the ball has been smooshed"
  else
    -- log and do nothing
    logInfo @String "the ball has crumbled due to bad inputs."

-- | The remove endpoint.
explode :: AsContractError e => Promise () Schema e ()
explode =  endpoint @"explode" @CustomDatumType $ \CustomDatumType {cdtGroupPKHs=cdtGroupPKHs, cdtPayouts=cdtPayouts} -> do
  logInfo @String "explode the ada ball"
  scrOutputs        <- utxosAt $ Scripts.validatorAddress $ typedValidator $ ContractParams {}
  miner             <- Plutus.Contract.ownPubKeyHash
  logInfo @String "The Miner"
  logInfo @PubKeyHash miner
  logInfo @String "Find The TX"
  logInfo @String "Unique Payouts"
  logInfo @Integer $ length cdtPayouts
  -- collect from the script and build the constraints.
  let inst = typedValidator $ ContractParams {}
  let tx   = collectFromScript scrOutputs CustomRedeemerType { crtData = 0 } <> createTX cdtGroupPKHs cdtPayouts miner
  _ <- submitTxConstraintsSpending inst scrOutputs tx
  logInfo @String "the ball has exploded"
  
-- | sum a list
sumList :: [Integer] -> Integer
sumList = foldr (+) 0

-- create must pay to pubkeys
createTX :: [PubKeyHash] -> [Integer] -> PubKeyHash -> Constraints.TxConstraints i o
createTX []     []     pkh = Constraints.mustBeSignedBy pkh
createTX []     _      pkh = Constraints.mustBeSignedBy pkh
createTX _      []     pkh = Constraints.mustBeSignedBy pkh
createTX (x:xs) (y:ys) pkh = Constraints.mustPayToPubKey x (Ada.lovelaceValueOf y) <> createTX xs ys pkh


-------------------------------------------------------------------------------
-- | TRACES
-------------------------------------------------------------------------------
{- | All the wallets will be involved in this but two of them will be selected
as the smoosher and exploder. The smoosher will attempt to smoosh a ball. The
smooshing can fail because the datum is checked to ensure validity. The exploder
will explode the ball, resulting in payments for everyone including themselves.
Each ball has a profit attached to it to incentivize exploding smooshed ada balls.

@See smooshTheBall, smooshWithNoProfit, explodeTheBall, explodeWithNoProfit
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
    groupPkhs' c' pkhs = groupPkhs' (c' - 1) (CW.pubKeyHash (CW.knownWallet c') : pkhs)

-- | Convert a base 10 integer into base q as a list of integers then add 2 and multiply by 1.2 ADA.
baseQ :: Integer -> Integer -> [Integer]
baseQ _      0 = [0]
baseQ number 1 = [1200000 * number]
baseQ number base = map (1200000 *) $ map (2 +) $ baseQ' number base []
  where
  baseQ' 0 _base list = list
  baseQ' number' base' list = baseQ' (Bi.divideInteger number' base') base' (Bi.modInteger number' base' : list)

numDigits :: Integer -> Integer -> Integer
numDigits number base = base ^ (number - 1)

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
      let datum = CustomDatumType { cdtGroupPKHs = listOfPKHs, cdtPayouts = listOfPayouts}
      Trace.callEndpoint @"smoosh" hdl1 datum
      void $ Trace.waitNSlots 1

smooshWithNoProfit :: IO ()
smooshWithNoProfit = Trace.runEmulatorTraceIO smooshWithNoProfit'
  where 
    smooshWithNoProfit' = do
      hdl1 <- Trace.activateContractWallet (W.toMockWallet smoosher) (contract @ContractError)
      let datum = CustomDatumType { cdtGroupPKHs = listOfPKHs, cdtPayouts = listOfPayouts } -- no profit
      Trace.callEndpoint @"smoosh" hdl1 datum
      void $ Trace.waitNSlots 1

explodeTheBall :: IO ()
explodeTheBall = Trace.runEmulatorTraceIO explodeTheBall'
  where 
    explodeTheBall' = do
      let datum = CustomDatumType { cdtGroupPKHs = listOfPKHs, cdtPayouts = listOfPayouts}
      hdl2 <- Trace.activateContractWallet (W.toMockWallet exploder) (contract @ContractError)
      Trace.callEndpoint @"explode" hdl2 datum
      void $ Trace.waitNSlots 1

explodeWithNoProfit :: IO ()
explodeWithNoProfit = Trace.runEmulatorTraceIO explodeWithNoProfit'
  where 
    explodeWithNoProfit' = do
      let datum = CustomDatumType { cdtGroupPKHs = listOfPKHs, cdtPayouts = listOfPayouts } -- no profit
      hdl2 <- Trace.activateContractWallet (W.toMockWallet exploder) (contract @ContractError)
      Trace.callEndpoint @"explode" hdl2 datum
      void $ Trace.waitNSlots 1


