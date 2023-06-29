
# SIMULAÇÃO POKER

library(poker)
source("Function_Poker.R")


# BARALHO (DECK OF CARDS)   NAIPE  (SUIT) COPAS  (HEARTS) PAUS (CLUBS) OUROS  (DIAMONDS) ESPADAS (SPADES)
cardDeck <- c(outer(c(2:10,"J","Q","K","A"),
                    c("H","S","C","D"),
                    ## could use
                    ## "\u2661","\u2662","\u2667", "\u2664"
                    paste0))


n_cards<-1:length(cardDeck)
Data_frame_cards<-data.frame(n_cards,cardDeck)

SEQ_CARDS<-seq(52)
cardDeck

# testar funções

testRoundOfPoker(6)
NION_POKER(9)

#############################################################



#######################################################################################################################################
#IMPUTS START
#H - COPAS  (HEARTS) 
#P - PAUS (CLUBS) 
#D - OUROS  (DIAMONDS) 
#s - ESPADAS (SPADES)

N_SIMULATION<-300
PLAYERS<-4

{
CARD01<-"QH"
CARD02<-"10H"
#
CARD03<-"2S"
CARD04<-"7C"
CARD05<-"AD"
#
CARD06<-"JS"
CARD07<-""


  nPlayers<-PLAYERS
  CARTA_PLAYER01_01<-CARD01
  CARTA_PLAYER01_02<-CARD02
  ROUND_ONE_01<-CARD03
  ROUND_ONE_02<-CARD04
  ROUND_ONE_03<-CARD05
  ROUND_TWO<-CARD06
  ROUND_THREE<-CARD07
  
  SIMULATION_POKER_NION(Data_frame_cards=Data_frame_cards,
                        N_SIMULATION,
                        nPlayers=nPlayers,
                        CARTA_PLAYER01_01=CARTA_PLAYER01_01,
                        CARTA_PLAYER01_02,
                        ROUND_ONE_01,
                        ROUND_ONE_02,
                        ROUND_ONE_03,
                        ROUND_TWO,
                        ROUND_THREE)
}



