module Main (main) where

import           Parse
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib
import           Text.Parsec                     (parse)

main :: IO ()
main = hspec $ do
  it "can parse simple semantic version" $ do
    parse parseSemVer "" "1.3.4" `shouldBe` Right (SemVer 1 3 4 (Fields []) (Fields []))

  it "can parse a semantic version with release" $ do
    parse parseSemVer "" "1.3.4-alpha.1" `shouldBe` Right (SemVer 1 3 4 (Fields [NOSS "alpha", NOSI 1]) (Fields []))

  it "can parse a semantic version with metadata" $ do
    parse parseSemVer "" "1.3.4+linux.1.2" `shouldBe` Right (SemVer 1 3 4 (Fields []) (Fields [NOSS "linux", NOSI 1, NOSI 2]))

  it "can parse a semantic version with release and metadata" $ do
    parse parseSemVer "" "1.3.4-alpha.1+linux.1.2" `shouldBe` Right (SemVer 1 3 4 (Fields [NOSS "alpha", NOSI 1]) (Fields [NOSS "linux", NOSI 1, NOSI 2]))

  it "incorrect semantic versions don't parse" $ do
    parse parseSemVer "" "1.three.4-alpha.1+linux.1.2" `shouldSatisfy` isLeft
    parse parseSemVer "" "1.3.4+alpha.1+linux.1.2" `shouldSatisfy` isLeft
    parse parseSemVer "" "1.3.4-alpha.1-linux.1.2" `shouldSatisfy` isLeft
    parse parseSemVer "" "1.3.4.4" `shouldSatisfy` isLeft
