module EBS.Target(
  HitPoints,
  ManaPoints,
  TargetableUnit(..),
  weakTo,
  strongAgainst,
  isWeakTo,
  isStrongAgainst) where


import EBS.Elemental
import EBS.Status


type HitPoints = Integer
type ManaPoints = Integer


data TargetableUnit = Unit{name :: String,
                           level :: Int,
                           hp :: HitPoints,
                           maxHp :: HitPoints,
                           mp :: ManaPoints,
                           maxMp :: ManaPoints,
                           elemType :: Maybe Element,
                           status :: Maybe [Status]} deriving (Eq, Read, Show)


checkProperty :: TargetableUnit -> (Element -> elem) -> Maybe elem
checkProperty m f = case elemType m of
                    Just e -> Just . f $ e
                    _ -> Nothing


weakTo :: TargetableUnit -> Maybe Element
weakTo m = checkProperty m succ


strongAgainst :: TargetableUnit -> Maybe Element
strongAgainst m = checkProperty m pred


--If the monster hasn't got any element, the result will be Nothing.
isWeakTo :: TargetableUnit -> Maybe Element -> Maybe Bool
m `isWeakTo` elem = case elem of
                         Just e -> checkProperty m ((==e) . succ)
                         _ -> Nothing


isStrongAgainst :: TargetableUnit -> Maybe Element -> Maybe Bool
m `isStrongAgainst` elem = case elem of
                                Just e -> checkProperty m ((==e) . pred)
                                _ -> Nothing
