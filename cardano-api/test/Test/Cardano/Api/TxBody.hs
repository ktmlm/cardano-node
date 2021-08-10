{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.TxBody (tests) where

import           Cardano.Prelude

import           Hedgehog (forAll, property, tripping)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Api

import           Gen.Cardano.Api.Typed (genTxBody, genTxBodyContent)


-- * Properties

-- | Check that conversion from 'TxBodyContent' to 'TxBody' and back gives
-- result equivalent to original.
--
-- The original randomly-generated data requires
-- BuildTx/ViewTx type conversion (see View section) and
-- normalization (see Normalization section).
--
-- Roundtrip data requires normalization, too.
test_roundtrip_TxBody_make_get :: [TestTree]
test_roundtrip_TxBody_make_get =
  [ testProperty (show era) $
    property $ do
      content <- forAll $ genTxBodyContent era
      tripping
        (normalizeOriginal $ viewBodyContent content)
        (\_ -> makeTransactionBody content)
        (<&> \(TxBody content') -> normalizeRoundtrip content')
  | AnyCardanoEra era <- [minBound..]
  ]

-- | Check that conversion from 'TxBody' to 'TxBodyContent' and back gives
-- result equivalent to original.
--
-- The original randomly-generated data requires
-- ViewTx/BuildTx type conversion (see Build section).
--
-- No normalization is needed here, because 'TxBody' keeps less information
-- on type and value level than 'TxBodyContent'.
-- For instance, no special /None/ values.
test_roundtrip_TxBody_get_make :: [TestTree]
test_roundtrip_TxBody_get_make =
  [ testProperty (show era) $
    property $ do
      txbody <- forAll $ genTxBody era
      tripping
        txbody
        (\(TxBody content) -> content)
        (makeTransactionBody . buildBodyContent)
  | AnyCardanoEra era <- [minBound..]
  ]


-- * Normalization
--
-- Strip unnecessary details
--
-- In many cases, after roundtrip,
-- @Just mempty@ may become @Nothing@ or vice versa.
-- Input data also may be generated as either @Just 0@ or @Nothing@.
-- Order of some items may also change, they need to be reordered.

-- | Normalizations applied to original data only
normalizeOriginal :: TxBodyContent ViewTx era -> TxBodyContent ViewTx era
normalizeOriginal content =
  content
    { txAuxScripts    = normalizeAuxScripts    $ txAuxScripts    content
    , txCertificates  = normalizeCertificates  $ txCertificates  content
    , txIns           = sortOn fst             $ txIns           content
    , txInsCollateral = normalizeInsCollateral $ txInsCollateral content
    , txMetadata      = normalizeMetadata      $ txMetadata      content
    , txMintValue     = normalizeMintValue     $ txMintValue     content
    , txWithdrawals   = normalizeWithdrawals   $ txWithdrawals   content
    }

-- | Normalizations applied to roundtrip result data only
normalizeRoundtrip :: TxBodyContent ViewTx era -> TxBodyContent ViewTx era
normalizeRoundtrip content@TxBodyContent{txAuxScripts, txIns, txInsCollateral} =
  content
    { txAuxScripts    = normalizeAuxScripts    txAuxScripts
    , txIns           = sortOn fst             txIns
    , txInsCollateral = normalizeInsCollateral txInsCollateral
    }

-- | Original data: Unify empty and None.
--
-- Roundtrip data: Sort transaction input ids.
normalizeInsCollateral :: TxInsCollateral era -> TxInsCollateral era
normalizeInsCollateral = \case
  TxInsCollateralNone -> TxInsCollateralNone
  TxInsCollateral support ins
    -- for original:
    | null ins -> TxInsCollateralNone
    -- for roundtrip:
    | otherwise -> TxInsCollateral support $ sort ins

-- | Unify empty and None.
normalizeMetadata :: TxMetadataInEra era -> TxMetadataInEra era
normalizeMetadata = \case
  TxMetadataInEra _ (TxMetadata m) | null m -> TxMetadataNone
  other                                     -> other

-- | Unify empty and None.
-- Upgrade script versions (see Upgrading scripts section).
-- Stabilize order of scripts sorting them by language.
normalizeAuxScripts :: TxAuxScripts era -> TxAuxScripts era
normalizeAuxScripts = \case
  TxAuxScripts _       []      -> TxAuxScriptsNone
  TxAuxScripts support scripts ->
    -- sorting uses script versions, hence sort after upgrade
    TxAuxScripts support $
    sortOn languageOfScriptInEra $ map upgradeScriptInEra scripts
  other -> other

languageOfScriptInEra :: ScriptInEra era -> AnyScriptLanguage
languageOfScriptInEra (ScriptInEra lang _) =
  AnyScriptLanguage $ languageOfScriptLanguageInEra lang

-- | Unify empty and None.
normalizeWithdrawals :: TxWithdrawals ViewTx era -> TxWithdrawals ViewTx era
normalizeWithdrawals = \case
  TxWithdrawals _ [] -> TxWithdrawalsNone
  other              -> other

-- | Unify empty and None.
normalizeCertificates :: TxCertificates ViewTx era -> TxCertificates ViewTx era
normalizeCertificates = \case
  TxCertificates _ [] _ -> TxCertificatesNone
  other                 -> other

-- | Unify empty and None.
normalizeMintValue :: TxMintValue ViewTx era -> TxMintValue ViewTx era
normalizeMintValue = \case
  TxMintValue _ v _ | v == mempty -> TxMintNone
  other                           -> other


-- * Ugrading scripts
--
-- The instruction set from V1 may be used as V2.
-- We can't determine the script language version from the ledger transaction.
-- Therefore we default to the highest version for the time being.
-- TODO(2021-08-13, cblp) Revisit this

upgradeScriptInEra :: ScriptInEra era -> ScriptInEra era
upgradeScriptInEra = \case
  ScriptInEra SimpleScriptV1InAllegra script ->
    ScriptInEra SimpleScriptV2InAllegra $ upgradeScript script
  ScriptInEra SimpleScriptV1InMary script ->
    ScriptInEra SimpleScriptV2InMary $ upgradeScript script
  ScriptInEra SimpleScriptV1InAlonzo script ->
    ScriptInEra SimpleScriptV2InAlonzo $ upgradeScript script
  other -> other

upgradeScript :: Script SimpleScriptV1 -> Script SimpleScriptV2
upgradeScript (SimpleScript SimpleScriptV1 script) =
  SimpleScript SimpleScriptV2 $ upgradeSimpleScript script

upgradeSimpleScript ::
  SimpleScript SimpleScriptV1 -> SimpleScript SimpleScriptV2
upgradeSimpleScript = \case
  RequireSignature hash -> RequireSignature hash
  RequireAllOf scripts  -> RequireAllOf $ map upgradeSimpleScript scripts
  RequireAnyOf scripts  -> RequireAnyOf $ map upgradeSimpleScript scripts
  RequireMOf n scripts  -> RequireMOf n $ map upgradeSimpleScript scripts


-- * View: Change 'TxBodyContent' “phase” to 'ViewTx'
--
-- Basically, it's just dropping witness information.

viewBodyContent :: TxBodyContent BuildTx era -> TxBodyContent ViewTx era
viewBodyContent body =
  TxBodyContent
    { txAuxScripts      =                    txAuxScripts     body
    , txCertificates    = viewCertificates $ txCertificates   body
    , txExtraKeyWits    =                    txExtraKeyWits   body
    , txExtraScriptData = ViewTx
    , txFee             =                    txFee            body
    , txIns             = map viewTxIn     $ txIns            body
    , txInsCollateral   =                    txInsCollateral  body
    , txMetadata        =                    txMetadata       body
    , txMintValue       = viewMintValue    $ txMintValue      body
    , txOuts            =                    txOuts           body
    , txProtocolParams  = ViewTx
    , txUpdateProposal  =                    txUpdateProposal body
    , txValidityRange   =                    txValidityRange  body
    , txWithdrawals     = viewWithdrawals  $ txWithdrawals    body
    }

viewTxIn ::
  (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era)) ->
  (TxIn, BuildTxWith ViewTx  (Witness WitCtxTxIn era))
viewTxIn = second $ const ViewTx

viewWithdrawals :: TxWithdrawals BuildTx era -> TxWithdrawals ViewTx era
viewWithdrawals = \case
  TxWithdrawalsNone                 -> TxWithdrawalsNone
  TxWithdrawals support withdrawals ->
    TxWithdrawals
      support
      [(address, amount, ViewTx) | (address, amount, _) <- withdrawals]

viewCertificates :: TxCertificates BuildTx era -> TxCertificates ViewTx era
viewCertificates = \case
  TxCertificatesNone                    -> TxCertificatesNone
  TxCertificates support certificates _ ->
    TxCertificates support certificates ViewTx

viewMintValue :: TxMintValue BuildTx era -> TxMintValue ViewTx era
viewMintValue = \case
  TxMintNone                  -> TxMintNone
  TxMintValue support value _ -> TxMintValue support value ViewTx


-- * Build: Change 'TxBodyContent' “phase” to 'BuildTx'
--
-- Here we make up the most trivial witnesses or plug holes with 'panic'
-- to make sure the fields are not touched.

buildBodyContent :: TxBodyContent ViewTx era -> TxBodyContent BuildTx era
buildBodyContent body =
  TxBodyContent
    { txAuxScripts      =                     txAuxScripts     body
    , txCertificates    = buildCertificates $ txCertificates   body
    , txExtraKeyWits    =                     txExtraKeyWits   body
    , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
    , txFee             =                     txFee            body
    , txIns             = map buildTxIn     $ txIns            body
    , txInsCollateral   =                     txInsCollateral  body
    , txMetadata        =                     txMetadata       body
    , txMintValue       = buildMintValue    $ txMintValue      body
    , txOuts            =                     txOuts           body
    , txProtocolParams  = BuildTxWith Nothing
    , txUpdateProposal  =                     txUpdateProposal body
    , txValidityRange   =                     txValidityRange  body
    , txWithdrawals     = buildWithdrawals  $ txWithdrawals    body
    }

buildTxIn ::
  (TxIn, BuildTxWith ViewTx  (Witness WitCtxTxIn era)) ->
  (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
buildTxIn = second $ const $ BuildTxWith $ KeyWitness KeyWitnessForSpending

buildWithdrawals :: TxWithdrawals ViewTx era -> TxWithdrawals BuildTx era
buildWithdrawals = \case
  TxWithdrawalsNone                 -> TxWithdrawalsNone
  TxWithdrawals support withdrawals ->
    TxWithdrawals
      support
      [ ( address
        , amount
        , panic "buildWithdrawals: build field should not be checked"
        )
      | (address, amount, _) <- withdrawals
      ]

buildCertificates :: TxCertificates ViewTx era -> TxCertificates BuildTx era
buildCertificates = \case
  TxCertificatesNone                    -> TxCertificatesNone
  TxCertificates support certificates _ ->
    TxCertificates support certificates (BuildTxWith mempty)

buildMintValue :: TxMintValue ViewTx era -> TxMintValue BuildTx era
buildMintValue = \case
  TxMintNone                  -> TxMintNone
  TxMintValue support value _ -> TxMintValue support value $ BuildTxWith mempty


tests :: TestTree
tests = $testGroupGenerator
