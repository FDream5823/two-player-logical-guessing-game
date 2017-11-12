{-
  Title： Project 1 
  For two-player logical guessing game
  
  Name: Hao Ma
-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Data.List
import Card


--Take a target or a guess and output the rank list of the cards.
getRanks :: [Card] -> [Rank]
getRanks cardList = map rank cardList

--Take a target or a guess and output the suit list of the cards.
getSuits :: [Card] -> [Suit]
getSuits cardList = map suit cardList


--Take the minimum rank in guess and use "filter"
--to filter those ranks in answer which ranks lower
--than the minimum guess rank, then return the number
--of them in the filtered list.
lowerRanks :: [Card] -> [Card] -> Int
lowerRanks ans gue = 
    length (filter (< (minimum (getRanks gue))) (getRanks ans))

--Almost the same as the function above,
--here we take the maximum rank in the guess
--and identify how many ranks in the answer ranks
--higher than the maximum one
higherRanks :: [Card] -> [Card] -> Int
higherRanks ans gue = 
    length (filter (> (maximum (getRanks gue))) (getRanks ans))

--Idetify how many cards in the answer and guess have the same ranks.
--"deleteFirstsBy" take the first occurance of the element in the answer list
--and compare with the guess list whether the rank is equal or not, if equal
--then delete the element and move on to the next one.
--The second length gets the number of different ranks,
--"total length - length of differnet ranks = the number of the same rank".
sameRank ::[Card] -> [Card] -> Int
sameRank ans gue = 
    (length gue) - (length (deleteFirstsBy (==)(getRanks ans) (getRanks gue)))

--idetify how many cards in the answer and guess have the same suits
--same as the function above, only here is calculating same suits.
sameSuit :: [Card] -> [Card] -> Int
sameSuit ans gue = 
    (length gue) - (length (deleteFirstsBy (==)(getSuits ans) (getSuits gue)))

--This function takes an answer and a guess and returns 5 feedback numbers.
--"intersect" will compare the elements of two lists and return elements 
--which are exactly the same.
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback answer guess = (
    length (intersect answer guess), 
    lowerRanks answer guess, 
    sameRank answer guess, 
    higherRanks answer guess, 
    sameSuit answer guess)


--list of all 52 cards in order
originalCardList = [minBound .. maxBound] :: [Card]

--get the required mulit-card list which contains all possible combinations
multiCardsList :: Int -> [a] -> [[a]]
multiCardsList 0 _ = [[]]
multiCardsList _ [] = []
multiCardsList n (c:cs) = 
    --Get the first element of the list and combine it 
    --with the rest of elements one by one, then put all 
    --the combination lists into a list.
    map (c:) (multiCardsList (n-1) cs) ++ multiCardsList n cs

--put the card lists of list into the game state
data GameState = GameState [[Card]]
    deriving (Eq, Ord, Show)

--the inital cards combination list for 2-4 cards
initialTwo = GameState (multiCardsList 2 originalCardList)
initialThree = GameState (multiCardsList 3 originalCardList)
initialFour = GameState (multiCardsList 4 originalCardList)

-- the intial guess for 2, 3 and 4 cards
initialGuessTwo = [Card Diamond R6, Card Heart R10]
initialGuessThree = [Card Club R5, Card Diamond R9, Card Heart Queen]
initialGuessFour = 
    [Card Diamond R5, Card Heart R6, Card Diamond R10, Card Heart Jack]

-- get the number of cards as input to give a inital guess
initialGuess :: Int -> ([Card], GameState)
initialGuess cardNumbers
    |cardNumbers == 2 = (initialGuessTwo, initialTwo)
    |cardNumbers == 3 = (initialGuessThree, initialThree)
    |cardNumbers == 4 = (initialGuessFour, initialFour)

--take the previous guess and feedback to calculate the next guess
nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess (preGue, (GameState preState)) fb = (nextGue, nextState)
    where
        --updated game state
        nextState = GameState currentSolutions
        
        --use another function to filter the game state list
        currentSolutions=getSolutions preGue fb preState
        
        --take the first cards list in the game state as next guess
        nextGue = head currentSolutions
    
--filter out the unmatched feedbacks,
--and keep the same feedback of card combinations together as the updated game state
getSolutions :: [Card] -> (Int, Int, Int, Int, Int) -> [[Card]] -> [[Card]]
getSolutions guess fb original = filter verify original
    where verify as = sameFeedback fb guess as
  
--take the feedback, guess and target 
--to identify if the calculated feedback is equal to the given feedback
sameFeedback :: (Int, Int, Int, Int, Int) -> [Card] -> [Card] -> Bool
sameFeedback fb guess target
    |feedback target guess == fb = True
    |otherwise = False
