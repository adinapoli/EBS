module EBS.Status(
  Status(..)) where


data Status = Poison | Sleep | Frog | Slow
  deriving (Eq, Show, Read)
