module MoveSpec where

import           Test.Hspec
import           Systems.Move

spec :: Spec
spec = do
  describe "Find nearest" $ do
    it "returns same object as reference if it is in list" $ do
