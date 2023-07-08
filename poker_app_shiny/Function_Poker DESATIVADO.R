

####################################################################################
# Retorna a pontuação no poker
get_poker_score <- function(score) {
  score_dict <- c("High Card", "One Pair", "Two Pair", "Three of a Kind", 
                  "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush")
  if(score < 1 || score > 9) {
    return("Invalid score. Please input a number between 1 and 9.")
  } else {
    return(score_dict[score])
  }
}

####################################################################################
# função para gerar unicode dos nipes

convert_to_unicode <- function(cards) {
  for(i in seq_along(cards)){
    # Obter o último caractere (naipe)
    suit <- substr(cards[i], nchar(cards[i]), nchar(cards[i]))
    
    # Substituir a última letra pelo símbolo Unicode correspondente
    if(suit == "H"){
      cards[i] <- paste0(substr(cards[i], 1, nchar(cards[i])-1), "\u2665") # Copas
    } else if(suit == "S"){
      cards[i] <- paste0(substr(cards[i], 1, nchar(cards[i])-1), "\u2660") # Espadas
    } else if(suit == "C"){
      cards[i] <- paste0(substr(cards[i], 1, nchar(cards[i])-1), "\u2663") # Paus
    } else if(suit == "D"){
      cards[i] <- paste0(substr(cards[i], 1, nchar(cards[i])-1), "\u2666") # Ouros
    }
  }
  return(cards)
}

####################################################################################
# função para determinar o vencedor

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

#######################################################################################
# Função para determinar meu score


#######################################################################################
# função para gerar simulação e tabela de probabilidade de ganhar

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
  
  cardDeck <- c(outer(c(2:10,"J","Q","K","A"),
                      c("H","S","C","D"),
                      paste0))
  
  n_cards <- 1:length(cardDeck)
  Data_frame_cards <- data.frame(n_cards, cardDeck)
  
  # DETERMINAR ERRO E CONDIÇÕES
  
  
  # INICIAR SIMULAÇÃO
  
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
      
      cat((paste0("MINHAS  CARTAS: ",convert_to_unicode(CARTA_PLAYER01_01)," | ",convert_to_unicode(CARTA_PLAYER01_02),"\n")))
      cat((paste0("MESA  CARTAS: ",convert_to_unicode(BOARD_01)," | ",convert_to_unicode(BOARD_02),"\n")))
      
      
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


