module Main
where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import qualified Parser as P

import Text.Parsec
import Data.Either

import qualified TestRangeList as TRL

main = defaultMain tests

tests =
  [ testGroup "basic parsing"
    [ testCase "sym" test_parse_sym ]
  , testGroup "RangedList" TRL.tests
  ]

-- Check symbol parsing
parse_sym = parse P.sym "fname"

test_parse_sym =
  (show <$> (parse_sym "x") @?= Right "Symbol \"x\"") >>
  (P.symName <$> (parse_sym "x1_") @?= Right "x1_") >>
  (isLeft (parse_sym "3") @?= True) >>
  (isLeft (parse_sym "") @?= True)
