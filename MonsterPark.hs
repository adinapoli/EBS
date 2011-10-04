--Hardcoded monster definition, just for test
module EBS.MonsterPark(
  piros) where

import EBS.Elemental
import EBS.Target

piros = Unit {name = "Piros",
              level = 1,
              hp = 300,
              maxHp = 300,
              mp = 50,
              maxMp = 50,
              strength = 10,
              dexterity = 10,
              vitality = 10,
              magic = 10,
              spirit = 10,
              luck = 10,
              elemType = Just Fire,
              status = []}
