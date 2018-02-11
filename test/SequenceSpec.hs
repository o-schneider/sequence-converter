module SequenceSpec
  ( main
  , spec
  ) where

import           Control.Exception (evaluate)
import           Sequence
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "rename sequence keeps the meaningfull id and the whole adn" $ do
    it "given id with 6 chars" $ checkRenameSequence "05DELA"
    it "given id with 7 chars" $ checkRenameSequence "08FOUMC"
  describe "contents to sequence" $ do
    it "translate contents into sequence" $ do
      let sequences = contentsToSequences fakeContents
      length sequences `shouldBe` 1
      checkSequence (head sequences)
    it "translate contents into sequences" $ do
      let sequences = contentsToSequences (fakeContents ++ "\n" ++ fakeContents)
      length sequences `shouldBe` 2
      checkSequence (head sequences)
      checkSequence (head (tail sequences))
    it "gives back contents from sequence" $ do
      let res = sequenceToContent (">id", adn)
      res `shouldBe` ">id\n" ++ fakeAdnContent ++ "\n"

checkSequence :: Sequence -> Expectation
checkSequence (id, adn') = (id `shouldBe` ">05DELAclpA14-clpAIF.ab1, 05DELAclpA14-clpAIR.ab1") >>= const (adn' `shouldBe` adn)

checkRenameSequence :: String -> Expectation
checkRenameSequence expectedId =
  let (newId, newAdn) = renameSequence (getSequence expectedId)
  in (newId `shouldBe` ">" ++ expectedId) >>= const (newAdn `shouldBe` adn)

getSequence :: String -> Sequence
getSequence id = (">" ++ id ++ "clpA14-clpAIF.ab1, " ++ id ++ "clpA14-clpAIR.ab1", adn)

adn :: [String]
adn = ["N", "A", "A", "A", "T", "G", "T", "A", "A", "G", "T", "T"]

fakeContents :: String
fakeContents = ">05DELAclpA14-clpAIF.ab1, 05DELAclpA14-clpAIR.ab1\n" ++ fakeAdnContent

fakeAdnContent :: String
fakeAdnContent = "N\n" ++ "A\n" ++ "A\n" ++ "A\n" ++ "T\n" ++ "G\n" ++ "T\n" ++ "A\n" ++ "A\n" ++ "G\n" ++ "T\n" ++ "T\n"
