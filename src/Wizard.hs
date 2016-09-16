module Wizard
where

import Control.Applicative

data Suit = Air | Earth | Fire | Water
  deriving (Show, Eq)

data Card = Fool | Wizard | Card Suit Int
  deriving (Show, Eq)

deck :: [Card]
deck = replicate 4 Fool ++
  replicate 4 Wizard ++
  (Card <$> [Air,Earth,Fire,Water] <*> [1..13])

canPlay :: Maybe Suit -> Card -> Bool
canPlay Nothing _ = True
canPlay (Just s) c = suitMatches s c

canDiscard :: Maybe Suit -> [Card] -> Bool
canDiscard s = all (not . canPlay s)

-- a card is playable either because
getPlayables :: Maybe Suit -> [Card] -> [Card]
getPlayables s h | canDiscard s h = h
                 | otherwise = filter (canPlay s) h

suitMatches :: Suit -> Card -> Bool
suitMatches _ Fool = True
suitMatches _ Wizard = True
suitMatches s (Card s' _) = s == s'
