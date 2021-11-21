{-# LANGUAGE OverloadedStrings #-}

import Cardano.Api                         hiding (TxId)
import Data.String                         (IsString (..))
import Ledger.Value                        as Value 
import Ledger.Bytes                        (LedgerBytes(LedgerBytes), getLedgerBytes, fromHex)
import Ledger                              (PubKeyHash(..)) 

import Prelude
import System.Environment                  (getArgs)

import Cardano.Marketplace.BuySell


pkhFromStr :: String -> PubKeyHash
pkhFromStr s =
    case fromHex (fromString s) of 
        Right (LedgerBytes bytes) -> PubKeyHash bytes
        Left msg -> error msg

ab :: String -> LedgerBytes
ab s = case fromHex $ fromString s of
    Left _ -> error "No string"
    Right b -> b

main :: IO ()
main = do
    [assetSymbol, assetTokenName, royalties, royaltiesAddr', seller', owner', fileId] <- getArgs

    -- tn :: Value.TokeName
    let tn = Value.TokenName $ getLedgerBytes (ab assetTokenName) 

    let 
        seller = pkhFromStr seller' 
        owner = pkhFromStr owner'
        royaltiesAddr = pkhFromStr royaltiesAddr'
        asset  = Value.AssetClass (fromString assetSymbol, tn)
        bs     = BuySellParams 
            { 
              bsOwner         = owner
            , bsAsset         = asset
            , bsSeller        = seller
            , bsRoyalties     = read royalties :: Integer
            , bsRoyaltiesAddr = royaltiesAddr
            }
        buySellFile = "/cli-generated/scripts/buysell-" ++ fileId ++ ".json"

    buySellResult <- writeFileTextEnvelope buySellFile Nothing $ apiBuySellScript bs
    case buySellResult of
        Left err -> print $ displayError err
        Right () -> 
            (print $ valHash bs) >>
            (print $ scrAddress bs) >>
            (print $ hex bs) 