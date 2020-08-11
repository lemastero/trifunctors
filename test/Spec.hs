import Test.Hspec

foo :: String
foo = "foo"

main :: IO ()
main = hspec $ do
  describe "How to write a test" $ do
    it "Should be able to run tests" $ do
      foo `shouldBe` "foo"
