module EBS.Item(
  Item(..),
  ItemEffect(..),
  use,
  potion,
  ether,
  antidote,
  remedy,
  elixir)	where
	
import EBS.Target
import EBS.Status
import Control.Applicative


data Item = Item{itemName :: String,
				         itemDesc :: String,
                 itemEffect :: ItemEffect}

instance Show Item where
    show x = itemDesc x


data ItemEffect = Restore HitPoints ManaPoints
                | Cure [Status]
                | Custom (TargetableUnit -> TargetableUnit)


--Use the item i on the target t
use :: Item -> TargetableUnit -> TargetableUnit
use i t = case (itemEffect i) of
               (Restore hp' mp')  -> t {hp = hp t + hp', mp = mp t + mp'}
               (Cure rList) -> let newStatus = filter (\s -> s `notElem` rList) <$> (status t)
                                      in case newStatus of
                                              (Just []) -> t {status = Nothing}
                                              _ -> t {status = newStatus}
               (Custom f) -> f $ t

--Some Items
potion = Item "Potion" "Restores 100 HP" (Restore 100 0)
ether = Item "Ether" "Restores 100 MP" (Restore 0 100)
antidote = Item "Antidote" "Cures Poison status" (Cure [Poison])
remedy = Item "Remedy" "Cures Sleep status" (Cure [Sleep])

elixirBehaviour :: TargetableUnit -> TargetableUnit
elixirBehaviour t = t {hp = maxHp t, mp = maxMp t}
elixir = Item "Elixir" "Fully restores HP and MP" (Custom elixirBehaviour)
