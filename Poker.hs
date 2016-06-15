-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where
import Data.List
import Data.Ord
-- This strang looking comment adds code only needed when running the
-- doctest tests embedded in the comments
-- $setup
--

-- | Test 1
-- >>> Ace > King
-- True
-- >>> compareGroup [King] [King]
-- EQ
-- >>> compareGroup [King] [Queen]
-- GT
-- >>> compareGroup [King] [Queen,Queen]
-- LT
-- >>> cardsBestGroupFirst [King]
-- [King]
-- >>> cardsBestGroupFirst [King,Queen]
-- [King,Queen]
-- >>> cardsBestGroupFirst [King,Queen,Queen]
-- [Queen,King]
-- >>> cardsBestGroupFirst [Jack,King,Queen,Jack,Queen,Jack]
-- [Jack,Queen,King]
-- >>> handType [Ace,King,Queen,Jack]
-- BadHand
-- >>> handType [Ace,King,Queen,Jack,Nine]
-- HighCard
-- >>> handType [Ace,Ace,Queen,Jack,Ten]
-- Pair
-- >>> handType [Ace,Queen,Queen,Ten,Ten]
-- TwoPair
-- >>> handType [Ten,King,Ten,Jack,Ten]
-- ThreeOfAKind
-- >>> handType [Ace,King,Ace,King,Ace]
-- FullHouse
-- >>> handType [Ace,King,Ace,Ace,Ace]
-- FourOfAKind
-- >>> handType [King,Ace,Queen,Jack,Ten]
-- Straight
-- >>> beats [Jack] [King]
-- False
-- >>>beats [Ace] [King]
-- True
-- >>>beats [Ace, King] [Ace,Queen]
-- True
-- >>>beats [Queen, King] [Ace,Queen]
-- False
-- >>>beats [Ace, Queen] [Ace,King]
-- False
-- >>>beats [King,Ace] [Ace,Queen]
-- True
-- >>>beats [King, King,Jack,Ace,Ten] [Ace,Queen,Jack,Nine,Ten]
-- True
-- >>>beats [King, King,Queen,Ace,Ten] [Jack,Jack,Queen,Ace,Ten]
-- True
-- >>>beats [King,King,Queen,Jack,Ace,Ten] [Jack,Jack,Queen,Jack,Ace,Ten]
-- True
-- >>>beats [King,King,King,Queen,Ten] [Ace,Jack,Ace,Queen,Ten]
-- True
-- >>>beats [Jack,Jack,Jack,Jack,Queen] [Ace,Ace,Ace,King,Queen]
-- True
-- >>>beats [Jack,Jack,Queen,Queen,Ten] [Jack,Jack,Queen,Ace,Ten]
-- True
-- >>>beats [Jack,Jack,Queen,Queen,King,Nine] [Jack,Jack,Queen,Queen,Ten,Nine]
-- True
-- >>>beats [Jack,Jack,Queen,Queen,King] [Jack,Jack,Jack,Queen,Ten]
-- False
data Card = Nine | Ten| Jack | Queen | King | Ace  deriving (Eq,Ord,Show, Enum)
data HandType = BadHand | HighCard | Pair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush
                   deriving (Eq,Ord,Show, Enum)
type Hand = [Card]
-- compareGroup :: [Card] -> [Card] -> Ord Card
thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ x = x
thenCmp x _ = x
compareGroup :: [Card] -> [Card] -> Ordering
compareGroup g1 g2 = comparing length g1 g2  `thenCmp` comparing head g1 g2
revCompareGroup g1 g2 = compareGroup g2 g1
cardsBestGroupFirst :: Hand -> Hand
cardsBestGroupFirst h= map head (sortBy (flip compareGroup) (group (sort h)))
cardsGroupings :: Hand -> [Int]
cardsGroupings h= sortBy (flip compare) $ map length (group (sort h))
isStraight :: Hand -> Bool
isStraight h = length h == 5 &&
            (foldl (&&) True $ map (\(x,y) -> (fromEnum y - fromEnum x)==1) (zip s (tail s)))
        where s = sort h
is h = map (\(x,y) -> (fromEnum y - fromEnum x)) (zip s (tail s))
        where s = sort h
handType :: Hand -> HandType
handType h
        | isStraight h = Straight
        | g == [4,1] = FourOfAKind
        | g == [3,2] = FullHouse
        | g == [2,2,1] = TwoPair
        | g == [3,1,1] = ThreeOfAKind
        | g == [2,1,1,1] = Pair
        | length h == 5 = HighCard
        | otherwise = BadHand
        where g = cardsGroupings h
beats :: Hand -> Hand -> Bool
beats h1 h2
          = (compare (handType h1) (handType h2) `thenCmp`
            compare (cardsBestGroupFirst h1) (cardsBestGroupFirst h2)) == GT
hello :: String -> String
hello s = "Hello " ++ s

main :: IO ()
main = putStrLn (hello "World")

