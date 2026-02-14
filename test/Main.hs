module Main (main) where

import Moe.Job.Rename.Strategy.CollectionSpec qualified as CollectionSpec
import Moe.Job.Subscription.WashingSpec qualified as WashingSpec
import Moe.Domain.FileSpec qualified as FileSpec
import Moe.Domain.Parser.OriginalTitleSpec qualified as OriginalTitleSpec
import Moe.Domain.Parser.RssTitleSpec qualified as RssTitleSpec
import Moe.Infra.Metadata.BangumiData.TypesSpec qualified as BangumiDataTypesSpec
import Moe.Infra.Metadata.MikanSpec qualified as MikanSpec
import Moe.Prelude
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ FileSpec.tests,
      OriginalTitleSpec.tests,
      RssTitleSpec.tests,
      BangumiDataTypesSpec.tests,
      MikanSpec.tests,
      WashingSpec.tests,
      CollectionSpec.tests
    ]
