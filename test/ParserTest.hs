{-# LANGUAGE ScopedTypeVariables #-}

module ParserTest where

import RIO

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop, modifyMaxSuccess)
import Test.QuickCheck (PrintableString(..))

import Parser.Inline

parserSpec :: Spec
parserSpec = 
    describe "inline parser" $ modifyMaxSuccess (const 10000) $
        prop "should be able to parse any text without failing or cause infinite loop" $
            \(someText :: PrintableString) -> 
                isRight $ testInlineParser $ getPrintableString someText 