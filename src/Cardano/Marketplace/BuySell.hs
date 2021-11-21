{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Marketplace.BuySell
  (
    apiBuySellScript
  , BuySellParams (..)
  , scrAddress
  , valHash
  , serialized 
  , hex 
  ) where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import qualified Data.ByteString          as BS
import           Ledger                   hiding (singleton)
import           Ledger.Ada               as Ada
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified Ledger.Bytes             as PLB
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Prelude                  (Show, String)
import           PlutusPrelude (Generic)
import           Text.Printf
import           Cardano.Api ( ToJSON , FromJSON )

minUtxo :: Integer
minUtxo = 1000000 -- Hard code the min UTxO parameter to calculate fees

ownerFees :: Integer
ownerFees = 25000

data BuySellParams = BuySellParams
  {
    bsOwner         :: !PubKeyHash -- Marketplace, the one receiving the fees
  , bsRoyalties     :: !Integer    -- Royalties for the collection owner
  , bsRoyaltiesAddr :: !PubKeyHash -- The address receiving royalties
  , bsAsset         :: !AssetClass -- Asset to sell
  , bsSeller        :: !PubKeyHash -- Seller
  } deriving Show

PlutusTx.makeIsDataIndexed ''BuySellParams [('BuySellParams, 0)]
PlutusTx.makeLift ''BuySellParams

data BuySellAction =
  -- Actions available
    Buy
  | EditPrice
  | Cancel
  deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''BuySellAction [ ('Buy,       0)
                                           , ('EditPrice, 1)
                                           , ('Cancel,    2)
                                           ]
PlutusTx.makeLift ''BuySellAction

{-# INLINABLE bsDatum #-}
bsDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
bsDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE bsValue #-}
bsValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
bsValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d


{-# INLINABLE mkBuySellValidator #-}
mkBuySellValidator :: BuySellParams -> Integer -> BuySellAction -> ScriptContext -> Bool
mkBuySellValidator params price action ctx =
  case action of
    EditPrice   ->  traceIfFalse "must be signed by seller" (txSignedBy info $ bsSeller params) &&
                    traceIfFalse "token missing from output" outputHasToken                     &&
                    traceIfFalse "wrong output datum" correctOutputDatum

    Cancel      ->  traceIfFalse "must be signed by seller" (txSignedBy info $ bsSeller params) &&
                    traceIfFalse "token must go back to seller" nftToSeller

    Buy         ->  traceIfFalse "incorrect split" correctSplit

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one output"

    signer :: PubKeyHash
    signer = case txInfoSignatories info of
        [pkh] -> pkh
        _     -> traceError "only one signer expected"

    outputDatum :: Integer
    outputDatum = case bsDatum ownOutput (`findDatum` info) of
        Nothing -> traceError "output datum not found"
        Just x  -> x

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (bsAsset params) == 1

    correctOutputDatum :: Bool
    correctOutputDatum = outputDatum > 0 -- price is higher than 0
    -- No real need to include this, but left for future modification

    getsAtLeast :: PubKeyHash -> Integer -> Bool
    getsAtLeast h x = Ada.fromValue (valuePaidTo info h) >= Ada.lovelaceOf x

    thereAreRoyalties :: Bool
    thereAreRoyalties =
        -- don't expect royalties if the creator == seller
        not (bsRoyaltiesAddr params == bsSeller params ||
        bsRoyalties params == 0)

    fees :: Integer -> Integer
    -- x is a ten thousandth of a percentage
    -- Example: fees of 2.5% == fees 25000
    fees x = let
      y = (price * x) `PlutusTx.Prelude.divide` 1000000
      in
        if y == 0 then 0 else
          if y < minUtxo then minUtxo else y

    nftToBuyer :: Bool
    nftToBuyer = assetClassValueOf (valuePaidTo info signer) (bsAsset params) == 1

    nftToSeller :: Bool
    nftToSeller = assetClassValueOf (valuePaidTo info (bsSeller params)) (bsAsset params) == 1

    paysRoyalties :: Bool
    paysRoyalties =
      if
        thereAreRoyalties
      then
        bsRoyaltiesAddr params `getsAtLeast` fees (bsRoyalties params)
      else
        True

    correctSplit :: Bool
    correctSplit =
      let
        x = fees ownerFees
        royalties = if thereAreRoyalties then fees (bsRoyalties params) else 0
        rest = price - (x + royalties)
      in
        bsOwner params `getsAtLeast` x &&
        bsSeller params `getsAtLeast` rest &&
        paysRoyalties &&
        nftToBuyer

data BuySelling
instance Scripts.ValidatorTypes BuySelling where
    type instance DatumType BuySelling = Integer
    type instance RedeemerType BuySelling = BuySellAction

typedBuySellValidator :: BuySellParams -> Scripts.TypedValidator BuySelling
typedBuySellValidator bsp = Scripts.mkTypedValidator @BuySelling
    ($$(PlutusTx.compile [|| mkBuySellValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode bsp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @BuySellAction

-- Validator creator
buySellValidator :: BuySellParams -> Validator
buySellValidator = Scripts.validatorScript . typedBuySellValidator

-- Create a Plutus Script
buySellScript :: BuySellParams -> Plutus.Script
buySellScript = Ledger.unValidatorScript . buySellValidator

validator :: BuySellParams -> Validator
validator = Scripts.validatorScript . typedBuySellValidator 

valHash :: BuySellParams -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedBuySellValidator 

scrAddress :: BuySellParams -> Ledger.Address
scrAddress = scriptHashAddress . valHash

serialized :: BuySellParams -> LB.ByteString 
serialized p = serialise $ scrAddress p

hex :: BuySellParams -> String
hex p = (concatMap (printf "%02x") . LB.unpack) $ serialized p 

buySellScriptAsShortBS :: BuySellParams -> SBS.ShortByteString
buySellScriptAsShortBS = SBS.toShort . LB.toStrict . serialise .buySellScript

apiBuySellScript :: BuySellParams -> PlutusScript PlutusScriptV1
apiBuySellScript = PlutusScriptSerialised . buySellScriptAsShortBS
