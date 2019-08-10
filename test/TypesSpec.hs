{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module TypesSpec (spec) where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Lens.Micro ((&), (.~))
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Text.RawString.QQ (r)
import Mtlstats.Types

spec :: Spec
spec = describe "Mtlstats.Types" $ do
  pPointsSpec
  playerSpec

pPointsSpec :: Spec
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

playerSpec :: Spec
playerSpec = describe "Player" $ do

  describe "decode" $
    it "should decode" $
      decode playerJSON `shouldBe` Just player

  describe "encode" $
    it "should encode" $
      decode (encode player) `shouldBe` Just player

player :: Player
player = newPlayer 1 "Joe" "centre"
  & pYtd . psGoals        .~ 2
  & pYtd . psAssists      .~ 3
  & pYtd . psPMin         .~ 4
  & pLifetime . psGoals   .~ 5
  & pLifetime . psAssists .~ 6
  & pLifetime . psPMin    .~ 7

playerJSON :: ByteString
playerJSON = [r|
  { "number": 1
  , "name": "Joe"
  , "position": "centre"
  , "ytd":
    { "goals": 2
    , "assists": 3
    , "penalty_mins": 4
    }
  , "lifetime":
    { "goals": 5
    , "assists": 6
    , "penalty_mins": 7
    }
  }|]
