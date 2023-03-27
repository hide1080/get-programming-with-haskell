data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  | Colorless
  deriving (Show, Eq)

instance Monoid Color where
  mempty = Colorless

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b  | a == b = a
            | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
            | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
            | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
            | otherwise = Brown
