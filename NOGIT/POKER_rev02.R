
# SIMULAÇÃO POKER
{
  library(stringr)
  library(tidyverse)
  library(tidytext)
  library(dplyr) # for pipes and the data_frame function
  library(poker)
  library(stringr) # to deal with strings and to clean up our data
  library(xlsx)
  library(readxl)
}


# BARALHO (DECK OF CARDS)   NAIPE  (SUIT) COPAS  (HEARTS) PAUS (CLUBS) OUROS  (DIAMONDS) ESPADAS (SPADES)
cardDeck <- c(outer(c(2:10,"J","Q","K","A"),
                    c("H","S","C","D"),
                    ## could use
                    ## "\u2661","\u2662","\u2667", "\u2664"
                    paste0))

n_cards<-1:length(cardDeck)
Data_frame_cards<-data.frame(n_cards,cardDeck)

testRoundOfPoker <- function(nPlayers) {
  alias <- strsplit(replicate(1, paste0("Player", sep="", 1:nPlayers, collapse=" "))[1]," ")[[1]]
  #nPlayers <- length(alias)
  position <- nPlayers
  y <- deal(nPlayers, position) # 9 jogadores 18 cartas + 5 cartas mesa total 23 cartas (aleatorio)
  players <- assignToPlayers(nPlayers, position, y) # carta dos jogadores distribui??o 
  board <- assignToBoard(y) # carta sobre a mesa
  cards <- hand(players, board)
  score <- showdown(cards)
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

testRoundOfPoker(6)

NION_POKER <- function(nPlayers,y) {
  alias <- strsplit(replicate(1, paste0("Player", sep="", 1:nPlayers, collapse=" "))[1]," ")[[1]]
  nPlayers <- length(alias)
  position <- nPlayers
  #y <- deal(nPlayers, position) # 9 jogadores 18 cartas + 5 cartas mesa total 23 cartas (aleatorio)
  players <- assignToPlayers(nPlayers, position, y) # carta dos jogadores distribui??o 
  board <- assignToBoard(y) # carta sobre a mesa
  cards <- hand(players, board)
  score <- showdown(cards)
  winner <- tiebreaker(nPlayers,cards,score)
  return(winner)
  }

NION_POKER(9)

#############################################################
SEQ_CARDS<-seq(52)
cardDeck

#############################################################


###################################################################################################################################
# PRIMEIRA CARTA


SIMULATION_POKER_NION<-function( Data_frame_cards,
                                 N_SIMULATION,
                                 nPlayers,
                                 CARTA_PLAYER01_01,
                                 CARTA_PLAYER01_02,
                                 ROUND_ONE_01,
                                 ROUND_ONE_02,
                                 ROUND_ONE_03,
                                 ROUND_TWO,
                                 ROUND_THREE){
    
    JOGADORES_NAMES<-strsplit(replicate(1, paste0("Player", sep="", 1:nPlayers, collapse=" "))[1]," ")
    PROB_MEAN<-round(1/nPlayers*100,2)
  
    if(CARTA_PLAYER01_01!=CARTA_PLAYER01_02 & ROUND_ONE_01==""){
        FIRST_CARD<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==CARTA_PLAYER01_01,][1])
        SECOND_CARD<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==CARTA_PLAYER01_02,][1])
        SEQ_CARDS00<-SEQ_CARDS[SEQ_CARDS!=FIRST_CARD & SEQ_CARDS!=SECOND_CARD]
        
          TIME00<-Sys.time()
          for (ii in 1:N_SIMULATION) {
            ID<-ii
            SAMPLE_ROUND01<-sample(SEQ_CARDS00,nPlayers*2+5-2)
            SEQ_CARDS_TABLE<-seq(nPlayers*2+5)
            kk<-1
            for (ff in seq(nPlayers*2+5)){
              if(ff==1){SEQ_CARDS_TABLE[ff]<-FIRST_CARD}
              if(ff==nPlayers+1){SEQ_CARDS_TABLE[ff]<-SECOND_CARD}
              if(ff!=(nPlayers+1) & ff!=1){
                SEQ_CARDS_TABLE[ff]<-SAMPLE_ROUND01[kk]
                kk<-kk+1
              }
            }
            y<-SEQ_CARDS_TABLE
            VENCEDOR<-NION_POKER(nPlayers,y)
            TABLE00<-data.table::data.table(ID,VENCEDOR)
            ifelse(ii==1,TABLE_ALL<-TABLE00, TABLE_ALL<-rbind(TABLE_ALL,TABLE00))
          }
          TIME01<-Sys.time()
          cat(paste0("\nRESULTADOS\nTempo de processamento: ",round(TIME01-TIME00,2),"\n"))
        
        
        {
          TABELA_FREQ<-data.frame(table(TABLE_ALL$VENCEDOR))
          TABELA_FREQ$PROB<- round(TABELA_FREQ$Freq/length(TABLE_ALL$VENCEDOR)*100,2)
          TABELA_FREQ<-TABELA_FREQ[order(TABELA_FREQ$Var1),]
          
          cat((paste0("ETAPA 01 - CARTAS NA MÃO --------------------- \n")))
          cat((paste0("PROBABILIDADE DE VENCER: ",TABELA_FREQ[TABELA_FREQ$Var1==1,][3],"%\n")))
          if(TABELA_FREQ[TABELA_FREQ$Var1==1,][3]>PROB_MEAN){cat((paste0("PROBABILIDADE ACIMA DA MÉDIA: ",PROB_MEAN,"%\n")))}
          if(TABELA_FREQ[TABELA_FREQ$Var1==1,][3]<=PROB_MEAN){cat((paste0("PROBABILIDADE ABAIXO DA MÉDIA: ",PROB_MEAN,"% !!!!!!!!!!\n")))}
         
          TXT_PROB<-(paste("PROBABILIDADE DE VENCER DE:",TABELA_FREQ[TABELA_FREQ$Var1==1,][3],"%"))
          DADOS_POKER_SIMULATION<-c(TXT_PROB)
          #return(DADOS_POKER_SIMULATION)
          }
      }

###################################################################################################################################
# PRIMEIRA CARTAS NA MESA

      if(CARTA_PLAYER01_01!=CARTA_PLAYER01_02 & ROUND_ONE_01!="" & ROUND_TWO ==""){
    
      FIRST_CARD<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==CARTA_PLAYER01_01,][1])
      SECOND_CARD<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==CARTA_PLAYER01_02,][1])
      BOARD_01<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_01,][1])
      BOARD_02<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_02,][1])
      BOARD_03<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_03,][1])
      SEQ_CARDS00<-SEQ_CARDS[SEQ_CARDS!=FIRST_CARD & SEQ_CARDS!=SECOND_CARD & SEQ_CARDS!=BOARD_01 & SEQ_CARDS!=BOARD_02 & SEQ_CARDS!=BOARD_03]
      SEQ_CARDS00
      
      TIME00<-Sys.time()
      for (ii in 1:N_SIMULATION) {
        ID<-ii
        SAMPLE_ROUND01<-sample(SEQ_CARDS00,nPlayers*2+5-2)
        SAMPLE_ROUND01
        SEQ_CARDS_TABLE<-seq(nPlayers*2+5)
        kk<-1
        for (ff in seq(nPlayers*2+5)){
          if(ff==1){SEQ_CARDS_TABLE[ff]<-FIRST_CARD}
          if(ff==(nPlayers+1)){SEQ_CARDS_TABLE[ff]<-SECOND_CARD}
          if(ff==(nPlayers*2+1)){SEQ_CARDS_TABLE[ff]<-BOARD_01}
          if(ff==(nPlayers*2+2)){SEQ_CARDS_TABLE[ff]<-BOARD_02}
          if(ff==(nPlayers*2+3)){SEQ_CARDS_TABLE[ff]<-BOARD_03}
          
          if(ff!=(nPlayers+1) & ff!=1 & ff!=(nPlayers*2+1)& ff!=(nPlayers*2+2) & ff!=(nPlayers*2+3)){
            SEQ_CARDS_TABLE[ff]<-SAMPLE_ROUND01[kk]
            kk<-kk+1
          }
        }
        
        y<-SEQ_CARDS_TABLE
        VENCEDOR<-NION_POKER(nPlayers,y)
        TABLE00<-data.table::data.table(ID,VENCEDOR)
        ifelse(ii==1,TABLE_ALL<-TABLE00, TABLE_ALL<-rbind(TABLE_ALL,TABLE00))
        
      }
      TIME01<-Sys.time()
      cat(paste0("\nRESULTADOS\nTempo de processamento: ",round(TIME01-TIME00,2),"\n"))
      
      
    {
      TABELA_FREQ<-data.frame(table(TABLE_ALL$VENCEDOR))
      TABELA_FREQ$PROB<- round(TABELA_FREQ$Freq/length(TABLE_ALL$VENCEDOR)*100,2)
      TABELA_FREQ<-TABELA_FREQ[order(TABELA_FREQ$Var1),]
      
      cat((paste0("ETAPA 02 - CARTAS NA MÃO E NA MESA 3 CARDS --------------------- \n")))
      cat((paste0("PROBABILIDADE DE VENCER: ",TABELA_FREQ[TABELA_FREQ$Var1==1,][3],"%\n")))
      if(TABELA_FREQ[TABELA_FREQ$Var1==1,][3]>PROB_MEAN){cat((paste0("PROBABILIDADE ACIMA DA MÉDIA: ",PROB_MEAN,"%\n")))}
      if(TABELA_FREQ[TABELA_FREQ$Var1==1,][3]<=PROB_MEAN){cat((paste0("PROBABILIDADE ABAIXO DA MÉDIA: ",PROB_MEAN,"% !!!!!!!!!!\n")))}
      
      TXT_PROB<-(paste("PROBABILIDADE DE VENCER DE:",TABELA_FREQ[TABELA_FREQ$Var1==1,][3],"%"))
      DADOS_POKER_SIMULATION<-c(TXT_PROB)
      #return(DADOS_POKER_SIMULATION)
     }
    
  }

###################################################################################################################################
# ROUND TWO

      if(CARTA_PLAYER01_01!=CARTA_PLAYER01_02 & ROUND_ONE_01!="" & ROUND_TWO !="" &   ROUND_THREE ==""){
      
      FIRST_CARD<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==CARTA_PLAYER01_01,][1])
      SECOND_CARD<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==CARTA_PLAYER01_02,][1])
      BOARD_01<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_01,][1])
      BOARD_02<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_02,][1])
      BOARD_03<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_03,][1])
      BOARD_04<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_TWO,][1])
      
      SEQ_CARDS00<-SEQ_CARDS[SEQ_CARDS!=FIRST_CARD & SEQ_CARDS!=SECOND_CARD & 
                               SEQ_CARDS!=BOARD_01 & SEQ_CARDS!=BOARD_02 & 
                               SEQ_CARDS!=BOARD_03 &
                               SEQ_CARDS!=BOARD_04]
      TIME00<-Sys.time()
      for (ii in 1:N_SIMULATION) {
        ID<-ii
        SAMPLE_ROUND01<-sample(SEQ_CARDS00,nPlayers*2+5-2)
        SEQ_CARDS_TABLE<-seq(nPlayers*2+5)
        kk<-1
        for (ff in seq(nPlayers*2+5)){
          if(ff==1){SEQ_CARDS_TABLE[ff]<-FIRST_CARD}
          if(ff==(nPlayers+1)){SEQ_CARDS_TABLE[ff]<-SECOND_CARD}
          if(ff==(nPlayers*2+1)){SEQ_CARDS_TABLE[ff]<-BOARD_01}
          if(ff==(nPlayers*2+2)){SEQ_CARDS_TABLE[ff]<-BOARD_02}
          if(ff==(nPlayers*2+3)){SEQ_CARDS_TABLE[ff]<-BOARD_03}
          if(ff==(nPlayers*2+4)){SEQ_CARDS_TABLE[ff]<-BOARD_04}
          
          if(ff!=(nPlayers+1) & ff!=1 & ff!=(nPlayers*2+1) & 
             ff!=(nPlayers*2+2) & 
             ff!=(nPlayers*2+3) &
             ff!=(nPlayers*2+4)){
            SEQ_CARDS_TABLE[ff]<-SAMPLE_ROUND01[kk]
            kk<-kk+1
          }
        }
        y<-SEQ_CARDS_TABLE
        VENCEDOR<-NION_POKER(nPlayers,y)
        
        TABLE00<-data.table::data.table(ID,VENCEDOR)
        ifelse(ii==1,TABLE_ALL<-TABLE00, TABLE_ALL<-rbind(TABLE_ALL,TABLE00))
      }
      TIME01<-Sys.time()
      cat(paste0("\nRESULTADOS\nTempo de processamento: ",round(TIME01-TIME00,2),"\n"))
    
    {
      TABELA_FREQ<-data.frame(table(TABLE_ALL$VENCEDOR))
      TABELA_FREQ$PROB<- round(TABELA_FREQ$Freq/length(TABLE_ALL$VENCEDOR)*100,2)
      TABELA_FREQ<-TABELA_FREQ[order(TABELA_FREQ$Var1),]
      
      cat((paste0("ETAPA 03 - CARTAS NA MÃO E NA MESA 4 CARDS --------------------- \n")))
      cat((paste0("PROBABILIDADE DE VENCER: ",TABELA_FREQ[TABELA_FREQ$Var1==1,][3],"%\n")))
      if(TABELA_FREQ[TABELA_FREQ$Var1==1,][3]>PROB_MEAN){cat((paste0("PROBABILIDADE ACIMA DA MÉDIA: ",PROB_MEAN,"%\n")))}
      if(TABELA_FREQ[TABELA_FREQ$Var1==1,][3]<=PROB_MEAN){cat((paste0("PROBABILIDADE ABAIXO DA MÉDIA: ",PROB_MEAN,"% !!!!!!!!!!\n")))}
      
      TXT_PROB<-(paste("PROBABILIDADE DE VENCER DE:",TABELA_FREQ[TABELA_FREQ$Var1==1,][3],"%"))
      DADOS_POKER_SIMULATION<-c(TXT_PROB)
      #return(DADOS_POKER_SIMULATION)
    }

}
###################################################################################################################################
# ROUND THREE

  
    if(CARTA_PLAYER01_01!=CARTA_PLAYER01_02 & ROUND_ONE_01!="" & ROUND_TWO !="" &   ROUND_THREE !=""){
    
        FIRST_CARD<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==CARTA_PLAYER01_01,][1])
        SECOND_CARD<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==CARTA_PLAYER01_02,][1])
        BOARD_01<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_01,][1])
        BOARD_02<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_02,][1])
        BOARD_03<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_03,][1])
        BOARD_04<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_TWO,][1])
        BOARD_05<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_THREE,][1])
        
        SEQ_CARDS00<-SEQ_CARDS[SEQ_CARDS!=FIRST_CARD & SEQ_CARDS!=SECOND_CARD & 
                                 SEQ_CARDS!=BOARD_01 & SEQ_CARDS!=BOARD_02 & 
                                 SEQ_CARDS!=BOARD_03 &
                                 SEQ_CARDS!=BOARD_04 &
                                 SEQ_CARDS!=BOARD_05]
        
        TIME00<-Sys.time()
        for (ii in 1:N_SIMULATION) {
          ID<-ii
          SAMPLE_ROUND01<-sample(SEQ_CARDS00,nPlayers*2+5-2)
          SEQ_CARDS_TABLE<-seq(nPlayers*2+5)
          kk<-1
          for (ff in seq(nPlayers*2+5)){
            if(ff==1){SEQ_CARDS_TABLE[ff]<-FIRST_CARD}
            if(ff==(nPlayers+1)){SEQ_CARDS_TABLE[ff]<-SECOND_CARD}
            if(ff==(nPlayers*2+1)){SEQ_CARDS_TABLE[ff]<-BOARD_01}
            if(ff==(nPlayers*2+2)){SEQ_CARDS_TABLE[ff]<-BOARD_02}
            if(ff==(nPlayers*2+3)){SEQ_CARDS_TABLE[ff]<-BOARD_03}
            if(ff==(nPlayers*2+4)){SEQ_CARDS_TABLE[ff]<-BOARD_04}
            if(ff==(nPlayers*2+5)){SEQ_CARDS_TABLE[ff]<-BOARD_05}
            
            if(ff!=(nPlayers+1) & ff!=1 & ff!=(nPlayers*2+1) & 
               ff!=(nPlayers*2+2) & 
               ff!=(nPlayers*2+3) &
               ff!=(nPlayers*2+4) &
               ff!=(nPlayers*2+5)){
              SEQ_CARDS_TABLE[ff]<-SAMPLE_ROUND01[kk]
              kk<-kk+1
            }
          }
          y<-SEQ_CARDS_TABLE
          VENCEDOR<-NION_POKER(nPlayers,y)
          
          TABLE00<-data.table::data.table(ID,VENCEDOR)
          ifelse(ii==1,TABLE_ALL<-TABLE00, TABLE_ALL<-rbind(TABLE_ALL,TABLE00))
        }
        TIME01<-Sys.time()
        cat(paste0("\nRESULTADOS\nTempo de processamento: ",round(TIME01-TIME00,2),"\n"))
      {
        TABELA_FREQ<-data.frame(table(TABLE_ALL$VENCEDOR))
        TABELA_FREQ$PROB<- round(TABELA_FREQ$Freq/length(TABLE_ALL$VENCEDOR)*100,2)
        TABELA_FREQ<-TABELA_FREQ[order(TABELA_FREQ$Var1),]
        
        cat((paste0("ETAPA 04 FINAL - CARTAS NA MÃO E NA MESA 5 CARDS --------------------- \n")))
        cat((paste0("PROBABILIDADE DE VENCER: ",TABELA_FREQ[TABELA_FREQ$Var1==1,][3],"%\n")))
        if(TABELA_FREQ[TABELA_FREQ$Var1==1,][3]>PROB_MEAN){cat((paste0("PROBABILIDADE ACIMA DA MÉDIA: ",PROB_MEAN,"%\n")))}
        if(TABELA_FREQ[TABELA_FREQ$Var1==1,][3]<=PROB_MEAN){cat((paste0("PROBABILIDADE ABAIXO DA MÉDIA: ",PROB_MEAN,"% !!!!!!!!!!\n")))}
        
        TXT_PROB<-(paste("PROBABILIDADE DE VENCER DE:",TABELA_FREQ[TABELA_FREQ$Var1==1,][3],"%"))
        DADOS_POKER_SIMULATION<-c(TXT_PROB)
        #return(DADOS_POKER_SIMULATION)
      }

   }
}


#######################################################################################################################################
#IMPUTS START
#H - COPAS  (HEARTS) 
#P - PAUS (CLUBS) 
#D - OUROS  (DIAMONDS) 
#s - ESPADAS (SPADES)

N_SIMULATION<-300
PLAYERS<-2

{
CARD01<-"QH"
CARD02<-"10H"
#
CARD03<-"2S"
CARD04<-"7C"
CARD05<-"AD"
#
CARD06<-"JS"
CARD07<-"QD"


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


######################################################################################
# RECONHECR IMAGE
{
  library(KeyboardSimulator)
  library(stringr)
  library(dplyr)
  library(tidyverse)
  library(tidytext)
  library(tesseract)
  library(magick)
  library(png)
  library(xlsx)
  library(imager)
  library(png)
  tesseract_info()
}

exist_file<-0
while (exist_file==0) {
  List_ScreenShot<-list.files("C:/Users/nionm/Pictures/Screenshots",pattern = "Captura de Tela")
  if(length(List_ScreenShot>0)){
    Last_List_ScreenShot<-List_ScreenShot[length(List_ScreenShot)]
    print(Last_List_ScreenShot)
    exist_file<-1
  }
}

link_file<-paste0("C:/Users/nionm/Pictures/Screenshots/",Last_List_ScreenShot)
SHOTSCREEN02 <- image_read(link_file)
BASE_IMAGE<-image_info(SHOTSCREEN02)[[2]]
ALTURA_IMAGE<-image_info(SHOTSCREEN02)[[3]]

CROP_AREA<-paste0(BASE_IMAGE/2,"x",0,"+",BASE_IMAGE/2,"+",0)
#image_crop(image, "100x150+50"): crop out width:100px and height:150px starting +50px from the left
SHOTSCREEN02_CROP<-image_crop(SHOTSCREEN02, CROP_AREA)


TABLE_WORDS<-image_ocr_data(SHOTSCREEN02_CROP,language = "eng")

# POTE: 885,798,951,820

CROP_AREA02<-paste0(650,"x",80,"+",630,"+",838)
print(paste(CROP_AREA02))
IMAGE_CROP02<-image_crop(SHOTSCREEN02_CROP,  CROP_AREA02)
IMAGE_CROP03<-image_negate(IMAGE_CROP02)
IMAGE_CROP03

CARDS_LETTERS <- tesseract(options = list(tessedit_char_whitelist = "A0123456789JQK"))
ocr(IMAGE_CROP03, engine = CARDS_LETTERS)
CARDS_TABLE<-unlist(strsplit(unlist(strsplit(ocr(IMAGE_CROP02, engine = CARDS_LETTERS),"\n"))[1]," "))
CARDS_TABLE
N_CARDS_TABLE<-nchar(CARDS_TABLE)

if(N_CARDS_TABLE==3){}
if(N_CARDS_TABLE==4){}
if(N_CARDS_TABLE==5){}



