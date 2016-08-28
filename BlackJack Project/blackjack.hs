module Blackjack
where

data Figure = None
             |Two
             |Three
             |Four
             |Five
             |Six
             |Seven
             |Eight
             |Nine
             |Ten
             |Ace
             |Jack
             |Queen
             |King deriving (Show, Enum,Eq)

data Color = Nan
            |Hearts
            |Spades
            |Clubs
            |Diamonds deriving (Show,Enum,Eq)


type Card = (Figure,Color)
type Deck = [Card]


deck :: Deck
deck = [(fig,col) |  fig <- [Two .. King] ,  col <- [Hearts .. Diamonds]]

--deckIndexes = [0..51]

getCost :: Card -> Int
getCost a = case (fst) a of
  Two -> 2
  Three -> 3
  Four -> 4
  Five -> 5
  Six -> 6
  Seven -> 7
  Eight -> 8
  Nine -> 9
  Ten -> 10
  Ace -> 11
  Jack -> 10
  Queen -> 10
  King -> 10


getCard [] = (None,Nan)
getCard x = head x

getCostOfCardOfDeck x = getCost (getCard x)

whoWins playerScore bankScore
 | playerScore > bankScore && playerScore <= 21 && bankScore <=21 || (playerScore < bankScore && playerScore<=21 && bankScore>21)
            = print ("The player wins with : " ++ show playerScore ++ " score")
 | playerScore > bankScore && playerScore > 21 && bankScore <=21 || (playerScore < bankScore && playerScore <=21 && bankScore <=21)
            = print ("The bank wins with :" ++ show bankScore ++ " score")
 | otherwise = print ("Draw")
