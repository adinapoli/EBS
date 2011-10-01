module EBS.Spell(
  SpellEffect(..),
  Spell(..),
  cast,
  getDmgMult,
  fire, fira, firaga,
  earth,
  bio,
  frogSong) where

import Data.List (nub)
import EBS.Elemental
import EBS.Status
import EBS.Target
import Control.Applicative


--Essentially the result of a spell cast
data SpellEffect = Damage HitPoints ManaPoints
                 | Inflict [Status] deriving (Show)


--Essentially a magic
data Spell = Spell{spellName :: String,
                   spellCost :: Integer,
                   spellElem :: Maybe Element,
                   spellEffect :: SpellEffect} deriving (Show)


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
fire   = Spell "Fire"   20 (Just Fire) (Damage 100 0)
fira   = Spell "Fira"   40 (Just Fire) (Damage 200 0)
firaga = Spell "Firaga" 80 (Just Fire) (Damage 300 0)

earth = Spell "Earth" 30 (Just Earth) (Damage 150 0)


bio = Spell "Bio" 20 Nothing (Inflict [Poison])
frogSong = Spell "Frog Song" 30 Nothing (Inflict [Frog, Sleep])
