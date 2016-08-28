import Blackjack
import System.Random
import Blackjack_computer
import Control.Monad

shuffle x = if length x < 2 then return x else do
 i <- System.Random.randomRIO (0, length x - 1)
 r <- shuffle (take i x ++ drop (i+1) x)
 return (x!!i : r)

pureList x = x

player_score = 0

main =  do
    putStrLn "Welcome to Blackjack !\n"
    putStrLn "Shuffling deck...\n"
    randomDeck <- shuffle deck
    let pureDeck = pureList randomDeck
    play player_score pureDeck

play player_score deck = do
 if player_score > 21 then
  print ("The player has lost ") >> bank player_score 0 deck
 else do
  putStrLn "Your curent score is : \n"
  print player_score
  putStrLn "1.Hit"
  putStrLn "2.Forfeit\n"
  cho <- getLine
  let choice = read cho
  if choice == 1 then
    if score == 21 then
      bank player_score 0 deck
    else
      print ("You have drawn : " ++ show (getCard deck)) >>
      play (player_score + (getCostOfCardOfDeck deck)) (tail deck)
  else
    if player_score == 0 then
      print ("The Bank wins by default")
    else
      print ("You have forfeited with score : " ++ show player_score) >>
      bank player_score 0 deck
