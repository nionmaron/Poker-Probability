data.frame(CARTA_PLAYER01_01="", CARTA_PLAYER01_02="", ROUND_ONE_01="", ROUND_ONE_02="", ROUND_ONE_03="", ROUND_TWO="", ROUND_THREE="")

dotTransformToRank(board) # Determine the rank of a card. 2 = deuce ...11 = jack 12 = queen 13 = king 14 = ace
dotTransformToSuit(board) # 1 = spade 2 = club 3 = heart 4 = diamond
dotScorer(c(2,1,3,2,5,3,6,4,7,1,13,2,14,2)) # Determine the ranking of one hand / 9 = Straight Flush / 8 = Four of a Kind /..

dotScorer(c(2,1,2,2,2,3,2,4))

# função de exemplo pacote Poker

hand_nion <- function (players, board, ncol) 
{
  nPlayers <- nrow(players)
  cards <- matrix(0, nrow = nPlayers, ncol = 14)
  for (i in 1:nPlayers) {
    j <- 1:2
    cards[i, 2 * j - 1] <- dotTransformToRank(players[i, 
    ])
    cards[i, 2 * j] <- dotTransformToSuit(players[i, ])
    j <- 1:5
    cards[i, 4 + 2 * j - 1] <- dotTransformToRank(board)
    cards[i, 4 + 2 * j] <- dotTransformToSuit(board)
  }
  cards
}


testRoundOfPoker <- function(nPlayers) {
  nPlayers<-1
  alias <- strsplit(replicate(1, paste0("Player", sep="", 1:nPlayers, collapse=" "))[1]," ")[[1]]
  alias
  #nPlayers <- length(alias)
  position <- nPlayers
  y <- deal(nPlayers, position) # 9 jogadores 18 cartas + 5 cartas mesa total 23 cartas (aleatorio)
  y 
  players <- assignToPlayers(nPlayers, position, y) # carta dos jogadores distribui??o 
  players
  board <- assignToBoard(y) # carta sobre a mesa
  board
  cards <- hand(players, board)  # board tem sempre 5 cartas
  cards
  score <- showdown(cards)
  score
  winner <- tiebreaker(nPlayers,cards,score)
  winner
  round <- 1
  cgiPlayers(round, alias, position, cards)
  round <- 2
  cgiPlayers(round, alias, position, cards)
  round <- 3
  cgiPlayers(round, alias, position, cards)
  round <- 4
  cgiPlayers(round, alias, position, cards)
  cat("Please look at the graphics window to see the current hand.\n\nThe score of each player is one of the following:\n\t9 = Straight Flush\n\t8 = Four of a Kind\n\t7 = Full House\n\t6 = Flush\n\t5 = Straight\n\t4 = Three of a Kind\n\t3 = Two Pair\n\t2 = Pair\n\t1 = Highcard\nThe scores for this hand are:\n\tPlayer 1: ",
      score[1],"\n\tPlayer 2: ",score[2],"\n\tPlayer 3: ",score[3],"\n\tPlayer 4: ",score[4],"\n\tPlayer 5: ",score[5],"\n\tPlayer 6: ",score[6],"\n\tPlayer 7: ",score[7],"\n\tPlayer 8: ",score[8],"\n\tPlayer 9: ",score[9],".\nThe winners of this hand are Players",winner,".\n\nThank you for playing.")
}

