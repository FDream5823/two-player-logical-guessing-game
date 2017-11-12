--  File     : Card.hs
--  Author   : Peter Schachte
--  Purpose  : An implementation of a playing card type

-- | This code implements a playing card type, including types for
--   ranks and suits.  All three types are in the Eq, Ord, Bounded,
--   Enum, Show, and Read classes.  Note that we use a compact
--   format for showing and reading ranks and suits:  one character
--   each for rank and suit.

module Card (Suit(..), Rank(..), Card(..)) where

import Data.List
import qualified Data.IntMap as IntMap

-- | A playing card suit.  Suits are ordered alphabetically, in the 
--   standard order used for Bridge.

data Suit = Club | Diamond | Heart | Spade
          deriving (Eq, Ord, Bounded, Enum)

suitchars = "CDHS"

-- | A playing card rank.  Ranks are ordered 2 - 10, followed by 
--   Jack, Queen, King and Ace.

data Rank =
        R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 |
        Jack | Queen | King | Ace
        deriving (Eq, Ord, Bounded, Enum)

rankchars = "23456789TJQKA"

-- | A standard western playing card.  Jokers are not supported.  
--   Cards are not in the Ord class, but for convenience they are in 
--   Bounded and Enum, to make it convenient to enumerate all the 
--   cards in a deck.  Cards are enumerated with all clubs first, 
--   then diamonds, hearts, and spades, and each suit is enumerated 
--   from lowest to highest rank.

data Card = Card {suit::Suit, rank::Rank}
          deriving (Eq, Bounded)

instance Ord Card where
    compare (Card s1 r1) (Card s2 r2) =
        let suitorder = compare s1 s2
        in  if suitorder == EQ then compare r1 r2 else suitorder

instance Enum Card where
    fromEnum (Card s r) = (fromEnum s)*13 + (fromEnum r)
    toEnum n = (Card s r)
      where s = toEnum (n `div` 13)
            r = toEnum (n `mod` 13)

instance Show Rank where
    show r = [rankchars !! fromEnum r]

instance Show Suit where
    show r = [suitchars !! fromEnum r]

instance Show Card where
    show (Card s r) = show r ++ show s

instance Read Rank where
    readsPrec _ = readSingleCharEnum rankchars

instance Read Suit where
    readsPrec _ = readSingleCharEnum suitchars

readSingleCharEnum :: Enum e => String -> String -> [(e,String)]
readSingleCharEnum str (c:cs) =
    case elemIndex c str of
        Nothing -> []
        Just i -> [(toEnum i, cs)]

instance Read Card where
    readsPrec _ string = 
        [(Card s r,rest) | (r,rest0) <- reads string,
                           (s,rest)  <- reads rest0]

						   
--correctMatch :: [Card] -> [Card] -> [a]
--correctMatch ans gue = intersect ans gue

--take a target or a guess and output the rank list of the cards
getRanks :: [Card] -> [Rank]
getRanks cardList = map rank cardList

getSuits :: [Card] -> [Suit]
getSuits cardList = map suit cardList

--take a target and a guess to output the number of cards in answer ranks lower than the lowest ranks in the guess
lowerRanks :: [Card] -> [Card] -> Int
lowerRanks ans gue
  = length (filter (< (minimum (getRanks gue))) (getRanks ans))

--take a target and a guess to output the number of cards in answer ranks higher than the highest ranks in the guess
higherRanks :: [Card] -> [Card] -> Int
higherRanks ans gue
  = length (filter (> (maximum (getRanks gue))) (getRanks ans))

--idetify how many cards in the answer and guess have the same ranks
sameRank ::[Card] -> [Card] -> Int
sameRank ans gue
  = (length gue) - (length (deleteFirstsBy (==)(getRanks ans) (getRanks gue)))

sameSuit :: [Card] -> [Card] -> Int
sameSuit ans gue
  = (length gue) - (length (deleteFirstsBy (==)(getSuits ans) (getSuits gue)))
  
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback answer guess = (
  length (intersect answer guess), 
  lowerRanks answer guess, 
  sameRank answer guess, 
  higherRanks answer guess, 
  sameSuit answer guess)

  {-where
    correctMatch = length (intersect answer guess)
	rankLower = lowerRanks answer guess
	rankSame = sameRank answer guess
	rankHigher = higherRanks answer guess
	suitSame = sameSuit answer guess-}

    
allPossiblefb :: [Card] -> [[Card]] -> [(Int, Int, Int, Int, Int)]
allPossiblefb answer (c:cs) = (feedback answer c) : (allPossiblefb answer cs)


--getCards :: Int -> Card
--getCards a = toEnum a

--list of all 52 cards in order
originalCardList = [minBound .. maxBound] :: [Card]

--get the required mulit-card list which contains all possible combinations
multiCardsList :: Int -> [a] -> [[a]]
multiCardsList 0 _ = [[]]
multiCardsList _ [] = []
multiCardsList n (c:cs) 
  = map (c:) (multiCardsList (n-1) cs) ++ multiCardsList n cs

data GameState = GameState {space :: [[Card]]}
  deriving (Eq, Ord, Show)

initialTwo = GameState (multiCardsList 2 originalCardList)
initialThree = GameState (multiCardsList 3 originalCardList)
initialFour = GameState (multiCardsList 4 originalCardList)

initialGuess :: Int -> ([Card], GameState)
initialGuess cardNumbers
  |cardNumbers == 2 = ([Card Club R6, Card Heart R10],initialTwo)
  |cardNumbers == 3 = ([Card Club R5, Card Diamond R9, Card Heart Queen],initialThree)
  |cardNumbers == 4 = ([Card Club R4, Card Diamond R7, Card Heart R10, Card Spade King],initialFour)


nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess (preGuess, preState) fb = (nextguess, nextState)
  where
    nextState = GameState currentSolutionSpace
    currentSolutionSpace=getSolutions preGuess fb (space preState)
    nextguess = selectMinPartitionGuess currentSolutionSpace currentSolutionSpace
    

getSolutions :: [Card] -> (Int, Int, Int, Int, Int) -> [[Card]] -> [[Card]]
getSolutions guess fb original = filter verify original
  where verify as =isEqualClass fb guess as
  

isEqualClass :: (Int, Int, Int, Int, Int) -> [Card] -> [Card] -> Bool
isEqualClass fb guess target
  |feedback target guess == fb = True
  |otherwise = False

compose :: (b -> c) -> (a -> a-> b) -> a -> a -> c
compose f g a b= f (g a b)  

selectMinGuess lst = snd $ IntMap.findMin $ IntMap.fromList lst
  
selectMinPartitionGuess::[[Card]]->[[Card]]->[Card]
selectMinPartitionGuess = compose selectMinGuess returnListOfMaxPartitions
  
returnListOfMaxPartitions :: [[Card]] -> [[Card]] -> [(Int,[Card])]
returnListOfMaxPartitions [] _ = []
returnListOfMaxPartitions (c:cs) lst
  =insert (length $ (returnMaxPartition $ partitions c (c:cs) lst (allPossiblefb c (c:cs))),c) (returnListOfMaxPartitions cs lst)

returnMaxPartition :: [[[Card]]] -> [[Card]]
returnMaxPartition mp = maximumBy(\x y -> length x `compare` length y) mp

partitions::[Card]->[[Card]]->[[Card]]->[(Int,Int,Int,Int,Int)]->[[[Card]]]
partitions guess _ _ []=[]
partitions guess gs mp (a:as)
               | (not (null (getSolutions guess a mp))) =insert (getSolutions guess a mp)
                                                        (partitions guess gs mp as)
               | otherwise =insert (getSolutions guess a mp) (partitions guess gs mp as)

  
  
  
  
  