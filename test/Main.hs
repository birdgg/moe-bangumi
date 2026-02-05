module Main (main) where

import Moe.App.Subscription.WashingSpec qualified as WashingSpec
import Moe.Domain.Bangumi.File.NamingSpec qualified as NamingSpec
import Moe.Domain.Bangumi.Parser.BgmtvSpec qualified as BgmtvSpec
import Moe.Domain.Bangumi.Parser.RssTitleSpec qualified as RssTitleSpec
import Moe.Infrastructure.BangumiData.TypesSpec qualified as BangumiDataTypesSpec
import Moe.Prelude
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ NamingSpec.tests,
      BgmtvSpec.tests,
      RssTitleSpec.tests,
      BangumiDataTypesSpec.tests,
      WashingSpec.tests
    ]
