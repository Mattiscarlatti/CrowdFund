{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}

module Contracts.CrowdFundContract where

import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils

type CrowdFundDatum = PubKeyHash

type CrowdFundRedeemer = Bool

data Campaign = Campaign {gcNumber :: Integer, gcDeadline :: POSIXTime, gcCampaignManager :: PubKeyHash}
  deriving (Generic, ToJSON, FromJSON)

makeLift ''Campaign

crowdFundLambda :: Campaign -> CrowdFundDatum -> CrowdFundRedeemer -> ScriptContext -> Bool
crowdFundLambda (Campaign _ a b) c d (ScriptContext txInfo _) =
  traceIfFalse "Deadline not yet passed" (from a `contains` txInfoValidRange txInfo)
    && case d of
      True -> traceIfFalse "Wrong signer" (head (txInfoSignatories txInfo) #== b)
      False ->
        traceIfFalse "Wrong signer or wrong receiver of the refund" (head (txInfoSignatories txInfo) #== b)
          && pany (\o -> txOutAddress o #== pubKeyHashAddress c) (txInfoOutputs txInfo)
{-# INLINEABLE crowdFundLambda #-}

untypedLambda :: Campaign -> UntypedValidator
untypedLambda campaign = mkUntypedValidator (crowdFundLambda campaign)
{-# INLINEABLE untypedLambda #-}

type CrowdFund = ValidatorContract "crowdfund"

compileScript :: Campaign -> CrowdFund
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
    compiledScript = compileScript (Campaign 1 1699026428 "7c934f867a8019588a9a2c165a09f5144695908c21025255b7df74d4")

pkhCampaignManager :: PubKeyHash
pkhCampaignManager = "7c934f867a8019588a9a2c165a09f5144695908c21025255b7df74d4" --PlatformManager
--pkhCampaignManager = "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2" --Emulator Wallet 1

instance ValidatorEndpoints CrowdFund where
  data GiveParam CrowdFund = Give {fundLovelace :: Integer}
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam CrowdFund = Grab {epRequestedFundsMet :: Bool}
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveParam CrowdFund -> ContractM CrowdFund ()
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
      compiledScript = compileScript (Campaign 1 1699026428 pkhCampaignManager)

  grab :: GrabParam CrowdFund -> ContractM CrowdFund ()
  grab (Grab epRequestedFundsMet) = do
    let compiledScript = compileScript (Campaign 1 1699026428 pkhCampaignManager)
    utxos <- getUtxosAt compiledScript
    now <- getCurrentInterval
    let eutxos = Map.toList utxos
    case eutxos of
      [] -> logStr "No UTxO found"
      _ -> do
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
                case epRequestedFundsMet of
                  True -> do
                    submitAndConfirm
                      Tx
                        { lookups = scriptLookupsFor compiledScript `andUtxos` utxos,
                          constraints =
                            mconcat
                              [ mustValidateInTimeRange (fromPlutusInterval now),
                                mustSign pkhCampaignManager,
                                mustPayPKH pkhCampaignManager val,
                                eutxoRef `mustBeSpentWith` True
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
                                eutxoRef `mustBeSpentWith` False
                              ]
                        }
                    logStr "Refund provided"

test :: EmulatorTest
test = do
  initEmulator @CrowdFund
    3
    [ Give
        { fundLovelace = 10000000
        }
        `fromWallet` 2,
      Give
        { fundLovelace = 20000000
        }
        `fromWallet` 3,
      waitUntil 10,
      Grab
        { epRequestedFundsMet = True
        }
        `toWallet` 2,
      Grab
        { epRequestedFundsMet = False
        }
        `toWallet` 1,
      Grab
        { epRequestedFundsMet = True
        }
        `toWallet` 1,
      Grab
        { epRequestedFundsMet = True
        }
        `toWallet` 1
    ]