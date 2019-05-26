{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Monad.State.Lazy as S
import           Data.Map.Strict          ((!))
import qualified Data.Map.Strict          as M'
import qualified Data.Set                 as Set
import           Lib
import           Parser
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           TestData
import           Text.Megaparsec          hiding (State)
import           Types

g :: Group Int
g =
  Group
    { _unitCount = 10
    , _hitPointsPerUnit = 10
    , _initiative = 10
    , _damage = 5
    , _damageType = Cold
    , _attributes = [WeakTo Fire, ImmuneTo Radiation]
    , _team = TeamName "A"
    }

g2 :: Group Int
g2 =
  Group
    { _unitCount = 10
    , _hitPointsPerUnit = 10
    , _initiative = 10
    , _damage = 5
    , _damageType = Fire
    , _attributes = [WeakTo Cold, ImmuneTo Radiation]
    , _team = TeamName "A"
    }

g3 :: Group Int
g3 =
  Group
    { _unitCount = 10
    , _hitPointsPerUnit = 10
    , _initiative = 10
    , _damage = 15
    , _damageType = Slashing
    , _attributes = [WeakTo Bludgeoning, ImmuneTo Fire]
    , _team = TeamName "B"
    }

gameState :: GameState
gameState = M'.fromList [(GroupID "1", g), (GroupID "2", g2), (GroupID "3", g3)]

main :: IO ()
main =
  hspec $ do
    describe "getEP" $
      it "returns the product of unit count and damage" $ getEP g `shouldBe` 50
    describe "getDamage" $ do
      it "returns effective power if no attributes apply" $
        getDamage g g3 `shouldBe` 50
      it "returns effective power * 2 if target is WeakTo" $
        getDamage g g2 `shouldBe` 100
      it "returns 0 if target is ImmuneTo" $ getDamage g2 g3 `shouldBe` 0
    describe "dealDamage" $ do
      it "reduces unit count if enough damage was dealt" $
        let result = S.execState (dealDamage (GroupID "1") 10) gameState
            target = result ! GroupID "1"
         in target `shouldBe`
            Group
              { _unitCount = 9
              , _hitPointsPerUnit = 10
              , _initiative = 10
              , _damage = 5
              , _damageType = Cold
              , _attributes = [WeakTo Fire, ImmuneTo Radiation]
              , _team = TeamName "A"
              }
      it
        "does not reduce unit count if not enough damage was dealt to kill a unit" $
        let result = S.execState (dealDamage (GroupID "1") 9) gameState
            target = result ! GroupID "1"
         in target `shouldBe` g
      it "removes group from state if all units were killed" $
        let result = S.execState (dealDamage (GroupID "1") 100) gameState
            target = M'.lookup (GroupID "1") result
         in target `shouldBe` Nothing
    describe "getSelectOrder" $ do
      it "sorts by effective power, descending" $
        getSelectOrder gameState `shouldBe`
        [(GroupID "3", g3), (GroupID "1", g), (GroupID "2", g2)]
      it "sorts by initiative if effective power is a tie, descending" $
        let g4 =
              Group
                { _unitCount = 10
                , _hitPointsPerUnit = 10
                -- | One higher than for 3
                , _initiative = 11
                , _damage = 15
                , _damageType = Slashing
                , _attributes = [WeakTo Bludgeoning, ImmuneTo Fire]
                , _team = TeamName "B"
                }
            state = M'.fromList [(GroupID "3", g3), (GroupID "4", g4)]
         in getSelectOrder state `shouldBe`
            [(GroupID "4", g4), (GroupID "3", g3)]
    describe "findBestTarget" $ do
      it "finds target receiving most damage" $
        let input =
              [ ((GroupID "1", g), 10)
              , ((GroupID "2", g2), 10)
              , ((GroupID "3", g3), 11)
              ]
         in findBestTarget input `shouldBe` ((GroupID "3", g3), 11)
      it "finds target with highest EP if received damage is a tie" $
        let input =
              [ ((GroupID "1", g), 10)
              , ((GroupID "2", g2), 10)
              , ((GroupID "3", g3), 10)
              ]
         in findBestTarget input `shouldBe` ((GroupID "3", g3), 10)
      it
        "finds target with highest initiative if received damage and highes EP are a tie" $
        let g4 =
              Group
                { _unitCount = 10
                , _hitPointsPerUnit = 10
                -- | One higher than for 3
                , _initiative = 11
                , _damage = 15
                , _damageType = Slashing
                , _attributes = [WeakTo Bludgeoning, ImmuneTo Fire]
                , _team = TeamName "B"
                }
            input =
              [ ((GroupID "1", g), 10)
              , ((GroupID "2", g2), 10)
              , ((GroupID "3", g3), 10)
              , ((GroupID "4", g4), 10)
              ]
         in findBestTarget input `shouldBe` ((GroupID "4", g4), 10)
    describe "getTargetsWithDamageReceived" $ do
      it "only includes targets that take damage" $
        let defenders = Set.fromList [(GroupID "1", g), (GroupID "2", g2)]
            attacker =
              Group
                { _unitCount = 10
                , _hitPointsPerUnit = 10
                , _initiative = 10
                , _damage = 15
                , _damageType = Radiation
                , _attributes = [WeakTo Bludgeoning, ImmuneTo Fire]
                , _team = TeamName "B"
                }
         in getTargetsWithDamageReceived attacker defenders `shouldBe` []
      it "only includes targets from opposite team" $
        let defenders =
              Set.fromList
                [(GroupID "1", g), (GroupID "2", g2), (GroupID "3", g3)]
            attacker =
              Group
                { _unitCount = 10
                , _hitPointsPerUnit = 10
                , _initiative = 10
                , _damage = 15
                , _damageType = Radiation
                , _attributes = [WeakTo Bludgeoning, ImmuneTo Fire]
                , _team = TeamName "B"
                }
         in getTargetsWithDamageReceived attacker defenders `shouldBe` []
      it "calculates correct damage" $
        let defenders = Set.fromList [(GroupID "1", g), (GroupID "2", g2)]
            attacker = g3
         in getTargetsWithDamageReceived attacker defenders `shouldBe`
            [((GroupID "1", g), 150), ((GroupID "2", g2), 150)]
    describe "attributesParser'" $ do
      it "should parse several attributes of same kind" $
        parse attributesParser' "" "immune to fire, radiation, slashing" `shouldParse`
        [ImmuneTo Fire, ImmuneTo Radiation, ImmuneTo Slashing]
      it "should parse several attributes of different kinds" $
        parse
          attributesParser'
          ""
          "weak to cold; immune to fire, radiation, slashing" `shouldParse`
        [WeakTo Cold, ImmuneTo Fire, ImmuneTo Radiation, ImmuneTo Slashing]
    describe "assignTargets" $ do
      it "correctly assigns targets" $
        S.evalState assignTargets (makeState [is3, is4] [in3]) `shouldBe`
        M'.fromList
          [ (GroupID "1-Immune System", GroupID "1-Infection")
          , (GroupID "1-Infection", GroupID "2-Immune System")
          ]
      -- it "correctly assigns targets" $
      --   S.evalState assignTargets state `shouldBe`
      --   M'.fromList
      --     [ (GroupID "7-Immune System", GroupID "1-Infection")
      --     , (GroupID "4-Immune System", GroupID "2-Infection")
      --     , (GroupID "9-Immune System", GroupID "3-Infection")
      --     , (GroupID "8-Immune System", GroupID "4-Infection")
      --     , (GroupID "10-Immune System", GroupID "5-Infection")
      --     , (GroupID "2-Immune System", GroupID "6-Infection")
      --     , (GroupID "3-Immune System", GroupID "7-Infection")
      --     , (GroupID "5-Immune System", GroupID "8-Infection")
      --     , (GroupID "1-Immune System", GroupID "9-Infection")
      --     , (GroupID "6-Immune System", GroupID "10-Infection")
      --     , (GroupID "7-Infection", GroupID "1-Immune System")
      --     , (GroupID "8-Infection", GroupID "2-Immune System")
      --     , (GroupID "9-Infection", GroupID "3-Immune System")
      --     , (GroupID "3-Infection", GroupID "4-Immune System")
      --     , (GroupID "2-Infection", GroupID "5-Immune System")
      --     , (GroupID "10-Infection", GroupID "6-Immune System")
      --     , (GroupID "5-Infection", GroupID "7-Immune System")
      --     , (GroupID "6-Infection", GroupID "8-Immune System")
      --     , (GroupID "4-Infection", GroupID "9-Immune System")
      --     , (GroupID "1-Infection", GroupID "10-Immune System")
      --     ]
