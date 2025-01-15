{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import           Control.Concurrent (threadDelay)
import           Cornelis.Subscripts (decNextDigitSeq, incNextDigitSeq)
import           Cornelis.Types
import           Cornelis.Types.Agda (Rewrite (..))
import           Cornelis.Utils (withBufferStuff, withLocalEnv)
import           Cornelis.Vim
import qualified Data.Text as T
import qualified Data.Vector as V
import           Neovim (liftIO)
import           Neovim.API.Text
import           Neovim.Test
import           Plugin
import           Test.Hspec
import           Utils
import           Lib (cornelisInit)


broken :: String -> SpecWith a -> SpecWith a
broken = before_ . pendingWith

spec :: Spec
spec = focus $ parallel $ do
  let timeout = Seconds 60

  executableSpec "agda"
  executableSpec "nvim"

  it "should load read-only file" $ do
    withVim timeout $ \w b -> do
      env <- cornelisInit
      withLocalEnv env $ do
        vim_command "view test/Readonly.agda"
        liftIO $ threadDelay 1e6
        load

  diffSpec "should refine" timeout "test/Hello.agda"
      [ Swap "unit = ?" "unit = one"] $ \w _ -> do
    goto w 11 8
    refine

  vimSpec "should support helper functions" timeout "test/Hello.agda" $ \w _ -> do
    goto w 11 8
    helperFunc Normalised "help_me"
    liftIO $ threadDelay 5e5
    reg <- getreg "\""
    liftIO $ reg `shouldBe` Just "help_me : Unit"

  diffSpec "should case split (unicode lambda)" timeout "test/Hello.agda"
      [ Add                       "slap = λ { true → {! !}"
      , Swap "slap = λ { x → ? }" "         ; false → {! !} }"
      ] $ \w _ -> do
    goto w 20 16
    caseSplit "x"

  diffSpec "should preserve indents when doing case split" timeout "test/Hello.agda"
      [ Add                       "  testIndent true = {! !}"
      , Swap "  testIndent b = ?" "  testIndent false = {! !}"
      ] $ \w _ -> do
    goto w 24 18
    caseSplit "b"

  diffSpec "should refine with hints" timeout "test/Hello.agda"
      [ Swap "isEven∘ (suc n) = {! isEven∘ !}" "isEven∘ (suc n) = isEven∘ {! !}"] $ \w _ -> do
    goto w 28 24
    refine

  let case_split_test name row col =
        diffSpec ("should case split (" <> T.unpack name <> ")") timeout "test/Hello.agda"
            (fmap (fmap (name <>))
              [ Add           " true = {! !}"
              , Swap " x = ?" " false = {! !}"
              ]
            ) $ \w _ -> do
          goto w row col
          caseSplit "x"

  case_split_test "test" 14 10

  case_split_test "unicodeTest₁" 17 18

  vimSpec "should support why in scope" timeout "test/Hello.agda" $ \_ b -> do
    withBufferStuff b $ \bs -> do
      whyInScope "zero"
      liftIO $ threadDelay 5e5
      res <- buffer_get_lines (iw_buffer $ bs_info_win bs) vimFirstLine vimLastLine False
      liftIO $ V.toList res `shouldContain` ["zero is in scope as"]

  vimSpec "should support goto definition across modules"
          timeout "test/Hello.agda" $ \w _ -> do
    goto w 2 18
    gotoDefinition
    liftIO $ threadDelay 5e5
    b' <- nvim_get_current_buf
    res <- buffer_get_lines b' vimFirstLine vimLastLine False
    liftIO $ V.toList res `shouldContain` ["module Agda.Builtin.Nat where"]

  diffSpec "work with multiple" timeout "test/Hello.agda"
      [ Add                  "copattern true = {! !}"
      , Swap "copattern = ?" "copattern false = {! !}"
      ] $ \w _ -> do
    goto w 31 13
    caseSplit ""
    liftIO $ threadDelay 5e5
    goto w 31 15
    liftIO $ threadDelay 5e5
    caseSplit "x"

  diffSpec "work with ? in names" timeout "test/Hello.agda"
      [ Swap "foo? ?f = {! !}" "foo? ?f x = {! !}"
      ] $ \w _ -> do
    goto w 34 13
    caseSplit ""

  diffSpec "question to meta" timeout "test/Hello.agda"
       [ Swap "unit = ?" "unit = {! !}"
       , Swap "test x = ?" "test x = {! !}"
       , Swap "unicodeTest\8321 x = ?" "unicodeTest\8321 x = {! !}"
       , Swap "slap = \955 { x \8594 ? }" "slap = \955 { x \8594 {! !} }"
       , Swap "  testIndent b = ?" "  testIndent b = {! !}"
       , Swap "copattern = ?" "copattern = {! !}"
       ] $ \_ b -> do
    questionToMeta b

  diffSpec "give" timeout "test/Hello.agda"
        [ Swap "give = {! true !}" "give = true"
        ] $ \w _ -> do
    goto w 37 11
    give

  let elaborate_test rw changes row column =
        let title = maybe "default mode" show rw in
        diffSpec ("elaborate (" <> title <> ")") timeout "test/Hello.agda" changes $
            \w _ -> do
                goto w row column
                withNormalizationMode rw elaborate

  elaborate_test (Just "AsIs")
    [Swap "elaborate = {! 3 !}" "elaborate = 3"]
    40 16

  diffSpec "should dec subscripts" timeout "test/Hello.agda"
      [ Swap "sub₀and-super⁹ : Nat" "sub₋₁and-super⁹ : Nat"] $ \w _ -> do
    goto w 42 1
    decNextDigitSeq

  diffSpec "should inc subscripts" timeout "test/Hello.agda"
      [ Swap "sub₀and-super⁹ : Nat" "sub₁and-super⁹ : Nat"] $ \w _ -> do
    goto w 42 1
    incNextDigitSeq

  diffSpec "should dec superscripts" timeout "test/Hello.agda"
      [ Swap "sub₀and-super⁹ : Nat" "sub₀and-super⁸ : Nat"] $ \w _ -> do
    goto w 42 6
    decNextDigitSeq

  diffSpec "should inc superscripts" timeout "test/Hello.agda"
      [ Swap "sub₀and-super⁹ : Nat" "sub₀and-super¹⁰ : Nat"] $ \w _ -> do
    goto w 42 6
    incNextDigitSeq

  diffSpec "should dec digits" timeout "test/Hello.agda"
      [ Swap "sub₀and-super⁹ = 15" "sub₀and-super⁹ = 14"] $ \w _ -> do
    goto w 43 16
    decNextDigitSeq

  diffSpec "should inc digits" timeout "test/Hello.agda"
      [ Swap "sub₀and-super⁹ = 15" "sub₀and-super⁹ = 16"] $ \w _ -> do
    goto w 43 16
    incNextDigitSeq

  vimSpec "should infer type of local variable" timeout "test/Hello.agda" $ \w b -> do
    withBufferStuff b $ \bs -> do
        goto w 46 14
        inferType AsIs
        liftIO $ threadDelay 5e5
        res <- buffer_get_lines (iw_buffer $ bs_info_win bs) vimFirstLine vimLastLine False
        liftIO $ V.toList res `shouldContain` ["Inferred Type: Bool"]

