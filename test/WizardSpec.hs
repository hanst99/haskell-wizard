module WizardSpec(spec)
where

import Test.Hspec
import Data.Foldable
import System.Random
import Wizard

newtype DetGen = DetGen { detGenList :: [Int] }

mkDetGen :: [Int] -> DetGen
mkDetGen = DetGen . cycle

instance RandomGen DetGen where
  next (DetGen (x:xs)) = (x, DetGen xs)
  split g = (g,g)

count x = length . filter (==x)

spec :: Spec
spec = do
  describe "A shuffled deck" $ do
    let gen = mkDetGen [1..60]
        (d,_) = shuffledDeck gen
    it "should contain 60 cards" $ do
      length d `shouldBe` 60
    it "should contain the same cards as a deck" $ do
      traverse_ (`shouldSatisfy` (`elem` deck)) d
      count Fool d `shouldBe` 4
      count Wizard d `shouldBe` 4
