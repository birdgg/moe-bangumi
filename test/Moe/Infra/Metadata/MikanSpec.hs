module Moe.Infra.Metadata.MikanSpec (tests) where

import Moe.Domain.Bangumi (MikanId (..), SeasonNumber (..))
import Moe.Infra.Metadata.Mikan (MikanSearchResult (..), parseSearchResults)
import Moe.Prelude
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Moe.Infra.Metadata.Mikan"
    [ testCase "parses minimal HTML" $ do
        let html = fromStrict $ encodeUtf8 @Text
              "<ul class=\"an-ul\"><li><a href=\"/Home/Bangumi/123\"><div class=\"an-text\" title=\"Test\">Test</div></a></li></ul>"
        parseSearchResults html
          @?= Right [MikanSearchResult {mikanId = MikanId 123, title = "Test", season = Nothing}],
      testCase "parses search results from HTML" $ do
        parseSearchResults sampleHtml
          @?= Right
            [ MikanSearchResult {mikanId = MikanId 3519, title = "\37329\29260\24471\20027", season = Nothing},
              MikanSearchResult {mikanId = MikanId 3822, title = "\37329\29260\24471\20027", season = Just (SeasonNumber 2)}
            ]
    ]

sampleHtml :: LByteString
sampleHtml =
  fromStrict $
    encodeUtf8 @Text
      "<ul class=\"list-inline an-ul\" style=\"margin-top:20px;\">\
      \  <li>\
      \    <a href=\"/Home/Bangumi/3519\" target=\"_blank\">\
      \      <span class=\"b-lazy b-loaded\" style=\"background-image: url(&quot;/images/Bangumi/202501/27eeaf1a.jpg?width=400&amp;height=400&amp;format=webp&quot;);\"></span>\
      \      <div class=\"an-info\">\
      \        <div class=\"an-info-group\">\
      \          <div class=\"an-text\" title=\"\37329\29260\24471\20027\" style=\"white-space:nowrap; width:170px; overflow:hidden; text-overflow:ellipsis;line-height: 40px;\">\37329\29260\24471\20027</div>\
      \        </div>\
      \      </div>\
      \    </a>\
      \  </li>\
      \  <li>\
      \    <a href=\"/Home/Bangumi/3822\" target=\"_blank\">\
      \      <span class=\"b-lazy b-loaded\" style=\"background-image: url(&quot;/images/Bangumi/202601/cbad1678.jpg?width=400&amp;height=400&amp;format=webp&quot;);\"></span>\
      \      <div class=\"an-info\">\
      \        <div class=\"an-info-group\">\
      \          <div class=\"an-text\" title=\"\37329\29260\24471\20027 \31532\20108\23395\" style=\"white-space:nowrap; width:170px; overflow:hidden; text-overflow:ellipsis;line-height: 40px;\">\37329\29260\24471\20027 \31532\20108\23395</div>\
      \        </div>\
      \      </div>\
      \    </a>\
      \  </li>\
      \</ul>"
