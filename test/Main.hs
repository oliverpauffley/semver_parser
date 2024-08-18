module Main (main) where

import           Parse
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib
import           Text.Parsec                     (parse)

main :: IO ()
main = hspec $ do
  describe "Parsing semantic versions" $ do
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

  describe "Ordering semantic versions" $ do
    it "can order major minor patch semantic versions" $ do
      compare (p "1.0.3") (p "1.3.2") `shouldBe` LT
      compare (p "1.3.2") (p "1.3.2") `shouldBe` EQ
      compare (p "1.3.4") (p "1.3.2") `shouldBe` GT

    it "can order major minor patch semantic versions with releases" $ do
      compare (p "1.3.2-alpha") (p "1.3.2") `shouldBe` LT
      compare (p "1.3.2-alpha") (p "1.3.2-alpha") `shouldBe` EQ
      compare (p "1.3.2-alpha") (p "1.3.2-beta") `shouldBe` LT
      compare (p "1.3.2-alpha.beta") (p "1.3.2-beta") `shouldBe` LT
      compare (p "1.3.2-beta") (p "1.3.2-beta.2") `shouldBe` LT
      compare (p "1.3.2-beta.2") (p "1.3.2-beta.11") `shouldBe` LT
      compare (p "1.3.2-2") (p "1.3.2-beta") `shouldBe` LT
      compare (p "1.3.2-2.1") (p "1.3.2-2.0") `shouldBe` GT
      compare (p "1.3.2-2.1") (p "1.3.2-2") `shouldBe` GT
      compare (p "1.3.2-2.1") (p "1.3.2-2.1.1") `shouldBe` LT

    it "metadata is ignored for ordering" $ do
      compare (p "1.3.2+alpha") (p "1.3.2") `shouldBe` EQ
      compare (p "1.3.1+alpha") (p "1.3.2") `shouldBe` LT
      compare (p "2.3.1+alpha") (p "1.3.2") `shouldBe` GT

p :: String -> SemVer
p s = unpack parsePlease
  where
    parsePlease = parse parseSemVer "" s
    unpack (Right result) = result
    unpack _              = error "could not unpack"
