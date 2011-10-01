module EBS.Item(
  Item(..),
  ItemEffect(..),
  use,
  potion,
  ether,
  antidote)	where
	
import EBS.Target
import EBS.Status
import Control.Applicative


data Item = Item{itemName :: String,
				         itemDesc :: String,
                 itemEffect :: ItemEffect} deriving (Show)


data ItemEffect = Restore HitPoints ManaPoints
                | Cure [Status] deriving (Show)

--Use the item i on the target t
use :: Item -> TargetableUnit -> TargetableUnit
use i t = case (itemEffect i) of
               (Restore hp' mp')  -> t {hp = hp t + hp', mp = mp t + mp'}
               (Cure rList) -> let newStatus = filter (\s -> s `notElem` rList) <$> (status t)
                                      in case newStatus of
                                              (Just []) -> t {status = Nothing}
                                              _ -> t {status = newStatus}

--Some Items
potion = Item "Potion" "Restores 100 HP" (Restore 100 0)
ether = Item "Ether" "Restores 100 MP" (Restore 0 100)
antidote = Item "Antidote" "Cures Poison status" (Cure [Poison])


