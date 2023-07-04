



library(poker)

# BARALHO (DECK OF CARDS)   NAIPE  (SUIT) COPAS  (HEARTS) PAUS (CLUBS) OUROS  (DIAMONDS) ESPADAS (SPADES)
cardDeck <- c(outer(c(2:10,"J","Q","K","A"),
                    c("H","S","C","D"),
                    ## could use
                    ## "\u2661","\u2662","\u2667", "\u2664"
                    paste0))

n_cards<-1:length(cardDeck)
Data_frame_cards<-data.frame(n_cards,cardDeck)
cat(cardDeck)

Data_frame_cards[Data_frame_cards$n_cards==12,2]
Data_frame_cards
length(y)
for (c in 1:length(board)) {
  print(Data_frame_cards[Data_frame_cards$n_cards==board[c],2])
}


testRoundOfPoker <- function() {
  alias <- c("Player 1", "Player 2","Player 3", "Player 4", "Player 5", "Player 6", "Player 7", "Player 8", "Player 9")
  nPlayers <- length(alias)
  position <- nPlayers
  y <- deal(nPlayers, position) # 9 jogadores 18 cartas + 5 cartas mesa total 23 cartas (aleatorio)
  players <- assignToPlayers(nPlayers, position, y) # carta dos jogadores distribuição 
  board <- assignToBoard(y) # carta sobre a mesa
  cards <- hand(players, board)
  score <- showdown(cards)
  score
  winner <- tiebreaker(nPlayers,cards,score)
  
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


NION_POKER <- function() {
  alias <- c("Player 1", "Player 2","Player 3", "Player 4", "Player 5", "Player 6", "Player 7", "Player 8", "Player 9")
  nPlayers <- length(alias)
  position <- nPlayers
  y <- deal(nPlayers, position) # 9 jogadores 18 cartas + 5 cartas mesa total 23 cartas (aleatorio)
  
  
  players <- assignToPlayers(nPlayers, position, y) # carta dos jogadores distribuição 
  board <- assignToBoard(y) # carta sobre a mesa
  cards <- hand(players, board)
  score <- showdown(cards)
  winner <- tiebreaker(nPlayers,cards,score)
  return(winner)
  }

testRoundOfPoker()


########################################################################################
#IMPUTS START
JOGADORES<-9
CARTA_PLAYER01_01<-"AD"
CARTA_PLAYER01_02<-"CD"


ROUND_ONE<-c()
# CALCULAR 

ROUND_TWO<-""
# CALCULAR

ROUND_THRE<-""
# CALCULAR

{
TIME00<-Sys.time()
for (ii in 1:1500) {
ID<-ii
VENCEDOR<-NION_POKER()
TABLE00<-data.table::data.table(ID,VENCEDOR)
ifelse(ii==1,TABLE_ALL<-TABLE00, TABLE_ALL<-rbind(TABLE_ALL,TABLE00))
}
TIME01<-Sys.time()
cat(paste(" tempo de processamento",TIME01-TIME00))
}

{
TABELA_FREQ<-data.frame(table(TABLE_ALL$VENCEDOR))
TABELA_FREQ$PROB<- round(TABELA_FREQ$Freq/length(TABLE_ALL$VENCEDOR)*100,2)
TABELA_FREQ<-TABELA_FREQ[order(TABELA_FREQ$Var1),]
}

