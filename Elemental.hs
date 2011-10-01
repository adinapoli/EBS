module EBS.Elemental(Element(..)) where

--We want a cyrcular enumeration for our elems
--so we can't rely on the Bounded type
data Element = Fire | Earth | Wind | Water | Lightning
  deriving (Eq, Ord, Show, Read)

instance Enum Element where
    toEnum 0 = Fire
    toEnum 1 = Earth
    toEnum 2 = Wind
    toEnum 3 = Water
    toEnum 4 = Lightning
    toEnum i = toEnum $ i `mod` 5

    fromEnum Fire = 0
    fromEnum Earth = 1
    fromEnum Wind = 2
    fromEnum Water = 3
    fromEnum Lightning = 4

    enumFromTo x y = map toEnum [a .. b']
      where a = fromEnum x
            b = fromEnum y
            b' = if a <= b then b else b + 5

    enumFromThen x1 x2 = error "enumFromThen not supported for Element"
    enumFromThenTo x1 x2 y = error "enumFromThenTo not supported for Element"
