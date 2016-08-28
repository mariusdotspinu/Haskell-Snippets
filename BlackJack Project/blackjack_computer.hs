module Blackjack_computer
where
import System.Random
import Control.Monad
import Blackjack

pureNumber x = x

getProbabityToPick = do
  i <- System.Random.randomRIO (1,2) :: IO Int
  return i

pick x | x `mod` 2 == 0 = False
       | otherwise = True

score = 0

bank player_score score deck = do
  putStrLn "Bank is playing..."
  bankIsPlaying player_score score deck
  putStrLn "Game Over."

bankIsPlaying player_score score deck = do
  number <- getProbabityToPick
  if score > 21 then
    print ("Bank lost With score :" ++ show score) >> whoWins player_score score
  else if pick number == True && score < 20 || score <=15
    then
    print ("The bank has drawn : " ++ show (getCard deck)) >>
    print ("Current score : " ++ show (score + getCostOfCardOfDeck deck)) >>
    bankIsPlaying player_score (score + (getCostOfCardOfDeck deck)) (tail deck)
  else
    print ("Bank forfeited . With score : " ++ show score) >>
    whoWins player_score score
