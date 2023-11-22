{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}

module Contracts.CrowdFundContract2 where

import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils
import Plutus.V2.Ledger.Contexts (ownHash, valueLockedBy)

type CrowdFundDatum = PubKeyHash

data Campaign = Campaign {gcCampaignManager :: PubKeyHash, gcValue :: Integer, gcDeadline :: POSIXTime, gcNumber :: Integer}
  deriving (Generic, ToJSON, FromJSON)

makeLift ''Campaign

crowdFundLambda :: Campaign -> CrowdFundDatum -> () -> ScriptContext -> Bool
crowdFundLambda (Campaign a b c _) d _ ctx@(ScriptContext txInfo _) =
  traceIfFalse "Deadline not yet passed" (from c `contains` txInfoValidRange txInfo)
    && case valueOf (valueLockedBy txInfo (ownHash ctx)) adaSymbol adaToken #>= b of
      True -> traceIfFalse "Wrong signer" (head (txInfoSignatories txInfo) #== a)
      False ->
        traceIfFalse "Wrong receiver of the refund" (pany (\o -> txOutAddress o #== pubKeyHashAddress d) (txInfoOutputs txInfo))
{-# INLINEABLE crowdFundLambda #-}

untypedLambda :: Campaign -> UntypedValidator
untypedLambda campaign = mkUntypedValidator (crowdFundLambda campaign)
{-# INLINEABLE untypedLambda #-}

type CrowdFund2 = ValidatorContract "crowdfund2"

compileScript :: Campaign -> CrowdFund2
compileScript campaign = mkValidatorContract ($$(compile [||untypedLambda||]) `applyCode` liftCode campaign)

exports :: JambExports
exports =
  export
    (defExports compiledScript)
      { dataExports =
          [ ("9683a9e7994b775f746d5a9f91a29536384555aa0563a285290dd971" :: PubKeyHash) `toJSONfile` "crowdFundDatumfunder1",
            ("8874bf12f1b212f42374ece80ab8af13e0cc461c558e131de0157a88" :: PubKeyHash) `toJSONfile` "crowdFundDatumfunder2",
            ("ca988646bd0804bcac14d2d94eea5fee6bf71abce40d35908a9453a5" :: PubKeyHash) `toJSONfile` "crowdFundDatumfunder3",
            ("1347103ea25f872d55033f545ffd73b61bb919c03c5902b41b08a521" :: PubKeyHash) `toJSONfile` "crowdFundDatumfunder4",
            ("a5f8070a0c8e0926d5f67d19d142d20d0e34af8c6530d917c31d5bc7" :: PubKeyHash) `toJSONfile` "crowdFundDatumfunder5",
            (True :: Bool) `toJSONfile` "crowdFundRedeemerTrue",
            (False :: Bool) `toJSONfile` "crowdFundRedeemerFalse"
          ],
        emulatorTest = test
      }
  where
    compiledScript = compileScript (Campaign "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2" 500 1700137066 1)

--due date slot for this contract: 33481002

pkhCampaignManager :: PubKeyHash
--pkhCampaignManager = "7c934f867a8019588a9a2c165a09f5144695908c21025255b7df74d4" --PlatformManager
pkhCampaignManager = "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2" --Emulator Wallet 1

requestedFunds :: Integer
requestedFunds = 500

getValueOut :: (TxOutRef, DecoratedTxOut) -> Integer -> Integer
getValueOut x y = do
  case (getDecoratedTxOutValue (snd (x))) of
    Nothing -> 0
    Just xx -> (valueOf xx adaSymbol adaToken) + y

instance ValidatorEndpoints CrowdFund2 where
  data GiveParam CrowdFund2 = Give {fundLovelace :: Integer}
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam CrowdFund2 = Grab
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveParam CrowdFund2 -> ContractM CrowdFund2 ()
  give (Give fundLovelace) = do
    pkh <- getOwnPKH
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor compiledScript,
          constraints = mustPayScriptWithDatum compiledScript pkh (lovelaceValueOf fundLovelace)
        }
    logStr $
      printf
        "Provided funding of %d lovelace"
        fundLovelace
    where
      compiledScript = compileScript (Campaign pkhCampaignManager 500 1700137066 1)

  grab :: GrabParam CrowdFund2 -> ContractM CrowdFund2 ()
  grab (Grab) = do
    let compiledScript = compileScript (Campaign pkhCampaignManager 500 1700137066 1)
    utxos <- getUtxosAt compiledScript
    now <- getCurrentInterval
    let eutxos = Map.toList utxos
    case eutxos of
      [] -> logStr "No UTxO found"
      _ -> do
        let totalVal = foldr getValueOut 0 eutxos
        let eutxoRef = fst (head eutxos)
        let eutxoDatum = snd (head eutxos)
        let eutxoValue = getDecoratedTxOutValue eutxoDatum
        case eutxoValue of
          Nothing -> logStr "This UTxO has no value"
          Just val -> do
            let utxoValue = valueOf val adaSymbol adaToken
            let maybeDatum = getDatumInDatumFromQuery (snd (_decoratedTxOutScriptDatum eutxoDatum))
            case maybeDatum of
              Nothing -> logStr "No datum found"
              Just datum -> do
                let pkhRefund = unsafeFromBuiltinData (getDatum datum)
                case (totalVal #>= requestedFunds) of
                  True -> do
                    submitAndConfirm
                      Tx
                        { lookups = scriptLookupsFor compiledScript `andUtxos` utxos,
                          constraints =
                            mconcat
                              [ mustValidateInTimeRange (fromPlutusInterval now),
                                mustSign pkhCampaignManager,
                                utxos `mustAllBeSpentWith` ()
                              ]
                        }
                    logStr "Funds send to campaign organizer"
                  False -> do
                    submitAndConfirm
                      Tx
                        { lookups = scriptLookupsFor compiledScript `andUtxos` utxos,
                          constraints =
                            mconcat
                              [ mustValidateInTimeRange (fromPlutusInterval now),
                                mustSign pkhCampaignManager,
                                mustPayPKH pkhRefund val,
                                eutxoRef `mustBeSpentWith` ()
                              ]
                        }
                    logStr "Refund provided"

test :: EmulatorTest
test = do
  initEmulator @CrowdFund2
    7
    [ Give
        { fundLovelace = 90000000
        }
        `fromWallet` 2,
      Give
        { fundLovelace = 90000000
        }
        `fromWallet` 3,
      Give
        { fundLovelace = 90000000
        }
        `fromWallet` 4,
      Give
        { fundLovelace = 90000000
        }
        `fromWallet` 5,
      Give
        { fundLovelace = 90000000
        }
        `fromWallet` 6,
      Give
        { fundLovelace = 90000000
        }
        `fromWallet` 7,
      waitUntil 10,
      Grab `toWallet` 4,
      Grab `toWallet` 1
    ]