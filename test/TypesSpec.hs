module TypesSpec (spec) where

import Lens.Micro ((&), (.~))
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Types

spec :: Spec
spec = describe "Mtlstats.Types" pPointsSpec

pPointsSpec = describe "pPoints" $ mapM_
  (\(goals, assists, points) -> let
    desc = "goals: " ++ show goals ++
      ", assists: " ++ show assists
    stats = newPlayerStats &
      psGoals   .~ goals &
      psAssists .~ assists
    in context desc $
      it ("should be " ++ show points) $
        pPoints stats `shouldBe` points)
  --  goals, assists, points
  [ ( 0,     0,       0      )
  , ( 1,     0,       1      )
  , ( 0,     1,       1      )
  , ( 2,     3,       5      )
  ]
