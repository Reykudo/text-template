{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Data.Text.IO
import Data.Text.Template (Template (Template), parseTemplate, showTemplate, template)
import Data.Text.Template.Parse (Frag (..), Var (..))
import Test.Hspec
import Test.Hspec.Runner
import Prelude hiding (putStrLn, readFile)

-- specs :: Spec
templateSpecs :: [Expectation]
templateSpecs =
  [ parseTemplate "qwe${aa}bb" `shouldBe` Right (Template [LitF "qwe", VarF Var{var = "aa", modifiers = []}, LitF "bb"])
  , parseTemplate "qwe${aa:mod}bb" `shouldBe` Right (Template [LitF "qwe", VarF Var{var = "aa", modifiers = ["mod"]}, LitF "bb"])
  , parseTemplate "qwe${aa:mod:mod2}bb" `shouldBe` Right (Template [LitF "qwe", VarF Var{var = "aa", modifiers = ["mod", "mod2"]}, LitF "bb"])
  , parseTemplate "qwe$${aa:mod}bb" `shouldBe` Right (Template [LitF "qwe${aa:mod}bb"])
  ]

main :: IO ()
main = do
  hspec $ do
    describe "text-template" $ do
      before (readFile "./test/template.xml") $ do
        it "should show as input" $ \fileContents -> do
          showTemplate (template fileContents) `shouldBe` fileContents

      it "returns the first element of a list" $ do
        sequence_ templateSpecs
