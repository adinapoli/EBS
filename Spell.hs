module EBS.Spell(
  SpellEffect(..),
  Spell(..),
  cast,
  getDmgMult,
  fire, fira, firaga,
  earth,
  bio,
  frogSong,
  laser) where

import Data.List (nub)
import EBS.Elemental
import EBS.Status
import EBS.Target
import Control.Applicative


--Essentially the result of a spell cast
data SpellEffect = Damage HitPoints ManaPoints
                 | Inflict [Status]
                 | Custom (TargetableUnit -> TargetableUnit)


--Essentially a magic
data Spell = Spell{spellName :: String,
                   spellDesc :: String,
                   spellCost :: Integer,
                   spellElem :: Maybe Element,
                   spellEffect :: SpellEffect}

instance Show Spell where
    show x = spellDesc x


--cast function
cast :: Spell -> TargetableUnit -> TargetableUnit
cast s t =
    let coeff = getDmgMult t (spellElem s)
        in case spellEffect s of
            Damage hit mana -> t {hp = hp t - floor (fromIntegral hit * coeff),
                                  mp = mp t - floor (fromIntegral mana * coeff)}
            Inflict statList -> case (status t) of
                                     (Just sList) -> t {status = Just $ nub (sList ++ statList)}
                                     Nothing -> t {status = Just statList}
            Custom f -> f $ t


--the damage multiplier function
--Ugly, can I do better?
getDmgMult :: TargetableUnit -> Maybe Element -> Double
getDmgMult t e = case t `isWeakTo` e of
                      Just True -> 2.0
                      Just False -> case t `isStrongAgainst` e of
                                         Just True -> 0.5
                                         Just False -> 1.0
                                         _ -> 1.0
                      _ -> 1.0


--SPELL DEFINITIONS
fire   = Spell "Fire"   "Low Fire damage to one opponent" 20 (Just Fire) (Damage 100 0)
fira   = Spell "Fira"   "Good Fire damage to one opponent" 40 (Just Fire) (Damage 200 0)
firaga = Spell "Firaga" "Great Fire damage to one opponent" 80 (Just Fire) (Damage 300 0)

earth = Spell "Earth" "Earth damage to one opponent" 30 (Just Earth) (Damage 150 0)


bio = Spell "Bio" "Inflict Poison status" 20 Nothing (Inflict [Poison])
frogSong = Spell "Frog Song" "Inflict Frog and Sleep status" 30 Nothing (Inflict [Frog, Sleep])

--Enemy skill section
laserBehaviour :: TargetableUnit -> TargetableUnit
laserBehaviour t = t {hp = floor $ (fromIntegral $ maxHp t) / 2.0}
laser = Spell "Laser" "Cut by half target's health" 20 Nothing (Custom laserBehaviour)
