module Wizard (deck,Suit(..), Card(..), getPlayables,shuffledDeck)
where

import Control.Applicative
import System.Random
import Control.Arrow
import Data.List

data Suit = Air | Earth | Fire | Water
  deriving (Show, Eq)

data Card = Fool | Wizard | Card Suit Int
  deriving (Show, Eq)

shuffle :: RandomGen g => g -> [a] -> ([a],g)
shuffle g = first (map snd . sortBy (\x y -> compare (fst x) (fst y))) . foldr randomMark ([],g) where
  randomMark x (xms, gen) =
    let (m,gen') = next gen in ((m,x):xms,gen')

deck :: [Card]
deck = replicate 4 Fool ++
  replicate 4 Wizard ++
  (Card <$> [Air,Earth,Fire,Water] <*> [1..13])

shuffledDeck :: RandomGen g => g -> ([Card],g)
shuffledDeck g = shuffle g deck

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
