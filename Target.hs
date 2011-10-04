module EBS.Target(
  HitPoints,
  ManaPoints,
  StatType,
  TargetableUnit(..),
  weakTo,
  strongAgainst,
  isWeakTo,
  isStrongAgainst) where


import EBS.Elemental
import EBS.Status


type HitPoints = Integer
type ManaPoints = Integer
type StatType = Int


data TargetableUnit = Unit{name :: String,
                           level :: Int,
                           hp :: HitPoints,
                           maxHp :: HitPoints,
                           mp :: ManaPoints,
                           maxMp :: ManaPoints,
                           strength :: StatType,
                           dexterity :: StatType,
                           vitality :: StatType,
                           magic :: StatType,
                           spirit :: StatType,
                           luck :: StatType,
                           elemType :: Maybe Element,
                           status :: [Status]} deriving (Eq, Read, Show)


checkProperty :: TargetableUnit -> (Element -> elem) -> Maybe elem
checkProperty m f = elemType m >>= (\e -> Just . f $ e)


weakTo :: TargetableUnit -> Maybe Element
weakTo m = checkProperty m succ


strongAgainst :: TargetableUnit -> Maybe Element
strongAgainst m = checkProperty m pred


--If the monster hasn't got any element, the result will be Nothing.
isWeakTo :: TargetableUnit -> Maybe Element -> Maybe Bool
m `isWeakTo` elem = elem >>= (\e -> checkProperty m ((==e) . succ))


isStrongAgainst :: TargetableUnit -> Maybe Element -> Maybe Bool
m `isStrongAgainst` elem = elem >>= (\e -> checkProperty m ((==e) . pred))
