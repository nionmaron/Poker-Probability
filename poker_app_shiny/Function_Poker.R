

####################################################################################
# 

Game_Guidance <- function(lang, selected_cards) {
  
  phrases_pt <- list(
    player01 = "Jogador 01: Selecione duas cartas para sua mão.",
    flop = "Flop: Selecione três cartas para a mesa.",
    turn = "Turn: Selecione uma quarta carta para a mesa.",
    select = "Selecione uma quinta carta para a mesa.",
    all_selected = "Todas as cartas foram selecionadas. Boa sorte!"
  )
  
  phrases_en <- list(
    player01 = "Player 01: Select two cards for your hand.",
    flop = "Flop: Select three cards for the table.",
    turn = "Turn: Select a fourth card for the table.",
    select = "Select a fifth card for the table.",
    all_selected = "All cards have been selected. Good luck!"
  )
  
  if(lang == "pt") {
    phrases <- phrases_pt
  } else if(lang == "en") {
    phrases <- phrases_en
  } else {
    stop("Language not supported.")
  }
  
  if(length(selected_cards$hand) < 2) {
    resultado <- phrases$player01
  } else if(length(selected_cards$table) < 3) {
    resultado <- phrases$flop
  } else if(length(selected_cards$table) < 4) {
    resultado <- phrases$turn
  } else if(length(selected_cards$table) < 5) {
    resultado <- phrases$select
  } else {
    resultado <- phrases$all_selected
  }
  
  texto <- paste("", resultado)
  
  return(texto)
}


####################################################################################
# função data frame de resultados
get_poker_score_df <- function(language = "en", new_column = NULL) {
  Score <- 1:9
  my_score<-c("","","","","","","","","")
  score_dict_en <- c("High Card", "One Pair", "Two Pair", "Three of a Kind", 
                     "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush")
  score_dict_pt <- c("Carta Alta", "Um Par", "Dois Pares", "Trinca", 
                     "Sequência", "Flush", "Full House", "Quadra", "Sequência de Mesmo Naipe")
  Prob_Simulation <- c(50.1177,42.2569,4.7539,2.1128,0.3925,0.1965,0.1441,0.0240,0.00139)
  
  if (language == "en") {
    score_dict <- score_dict_en
  } else if (language == "pt") {
    score_dict <- score_dict_pt
  } else {
    stop("Invalid language. Please input 'en' for English or 'pt' for Portuguese.")
  }
  
  df <- data.frame(Score = Score,my_score=my_score, Hand = score_dict, Probability = Prob_Simulation)
  
  if (!is.null(new_column)) {
    if (length(new_column) != length(Score)) {
      stop("Invalid new_column. It must have the same length as Score.")
    }
    df$Prob_Simulation <- new_column
  }
  
  return(df)
}


####################################################################################
# função da linguagem
lang_app<- function(lang="English"){
  if(lang=="English"){return("en")}
  else if (lang=="Português"){return("pt")}
  else{
    return("en")}
}

####################################################################################
#library(gtools)
# gerar data.frame de todas combinações
gerar_combinacoes <- function() {
  # Criando as 52 cartas do baralho
  naipes <- c("H", "D", "S", "C")  # Heart, Diamond, Spade, Club
  valores <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")  # 2-10, Jack, Queen, King, Ace
  baralho <- paste0(rep(valores, each=4), naipes)  # Criando todas as 52 cartas
  
  # Gerando todas as combinações de 7 cartas
  combinacoes <- combinations(n = length(baralho), r = 7, v = baralho)
  
  # Convertendo as combinações em um data.frame
  df <- data.frame(combinacoes)
  return(df)
}


####################################################################################
# função de decisão conforme estágio do jogo conforme probabilidade

get_decision <- function(n_players=2, prob_Win) {
  
  # Verifica se a probabilidade está no intervalo de 0 a 100
  if (prob_Win < 0 || prob_Win > 100) {
    stop("Erro: A probabilidade deve estar no intervalo de 0 a 100.")
  }
  
  # Verifica se a probabilidade está no intervalo de 0 a 100
  if (n_players < 2 || n_players > 9) {
    stop("Erro: Número de jogadores deve ser entre 2 a 9.")
  }
  poker_decision <- data.frame(
    "Probabilidade_Min" = c(0, 40*(1/n_players)/50, 0.7, 0.8, 0.9, 0.98),
    "Probabilidade_Max" = c(40*(1/n_players)/50, 0.7, 0.8, 0.9, 0.98, 1.01),
    "Decision" = c("Fold", "Call", "Raise", "Raise+", "Raise++","All In")
  )
  
  #*(1/n_players)*5
  
  #entradsa
  # 1/4 = 50%
  # 30  = y
  
  #tabela
  # 1/4 = 50%
  # x = 100
 #return(poker_decision)
  
  prob<-prob_Win/100
  #return(prob)
  
  for (i in 1:nrow(poker_decision)) {
    if (prob >= poker_decision$Probabilidade_Min[i] && prob <= poker_decision$Probabilidade_Max[i]) {
      return(poker_decision[["Decision"]][i])
    }
  }
}

get_decision(3,29)

# Testa a função
#get_decision(4,15, "Pre_Flop") # Deve retornar "Fold"
#get_decision(45, "Pre_Flop") # Deve retornar "Raise (Moderado)"


####################################################################################
# Retorna o nome da pontuação no poker
get_poker_score <- function(score, language = "en") {
  score_dict_en <- c("High Card", "One Pair", "Two Pair", "Three of a Kind", 
                     "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush")
  score_dict_pt <- c("Carta Alta", "Um Par", "Dois Pares", "Trinca", 
                     "Sequência", "Flush", "Full House", "Quadra", "Sequência de Mesmo Naipe")
  
  if(score < 1 || score > 9) {
    if (language == "en") {
      return("Invalid score. Please input a number between 1 and 9.")
    } else if (language == "pt") {
      return("Pontuação inválida. Por favor, insira um número entre 1 e 9.")
    } else {
      stop("Invalid language. Please input 'en' for English or 'pt' for Portuguese.")
    }
  } else {
    if (language == "en") {
      return(score_dict_en[score])
    } else if (language == "pt") {
      return(score_dict_pt[score])
    } else {
      stop("Invalid language. Please input 'en' for English or 'pt' for Portuguese.")
    }
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
  my_score  <- showdown(cards[1:nPlayers,1:cc])[1]
  win_score <- max(score)
  winner <- tiebreaker(nPlayers,cards,score)
  df<-data.frame("my_score"=my_score,"win_score"=win_score,"winner"=winner)
  return(df)
}

#######################################################################################
# Função para determinar meu score

MY_SCORE <- function(nPlayers,y,cc=4) {
  alias <- strsplit(replicate(1, paste0("Player", sep="", 1:nPlayers, collapse=" "))[1]," ")[[1]]
  nPlayers <- length(alias)
  position <- nPlayers
  #y <- deal(nPlayers, position) # 9 jogadores 18 cartas + 5 cartas mesa total 23 cartas (aleatorio)
  players <- assignToPlayers(nPlayers, position, y) # carta dos jogadores distribui??o 
  board <- assignToBoard(y) # carta sobre a mesa
  cards <- hand(players, board)
  score <- showdown(cards)
  my_score  <- showdown(cards[1:nPlayers,1:cc])[1]
  return(my_score)
}


#######################################################################################
# Função para determinar meu score

NION_POKER <- function(nPlayers,y) {
  alias <- strsplit(replicate(1, paste0("Player", sep="", 1:nPlayers, collapse=" "))[1]," ")[[1]]
  nPlayers <- length(alias)
  position <- nPlayers
  #y <- deal(nPlayers, position) # 9 jogadores 18 cartas + 5 cartas mesa total 23 cartas (aleatorio)
  players <- assignToPlayers(nPlayers, position, y) # carta dos jogadores distribui??o 
  board <- assignToBoard(y) # carta sobre a mesa
  cards <- hand(players, board)
  score <- showdown(cards)
  win_score <- max(score)
  winner <- tiebreaker(nPlayers,cards,score)
  df<-data.frame("win_score"=win_score,"winner"=winner)
  return(df)
}



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
      DF_VENCEDOR<-NION_POKER(nPlayers,y)  # data frame winner
      VENCEDOR<-DF_VENCEDOR$winner[1]
      WIN_SCORE<-DF_VENCEDOR$win_score
      
      TABLE00<-data.table::data.table(ID,VENCEDOR, WIN_SCORE)
      ifelse(ii==1,TABLE_ALL<-TABLE00, TABLE_ALL<-rbind(TABLE_ALL,TABLE00))
    }
    
    SCORE_FREQ<-data.frame(table(TABLE_ALL$WIN_SCORE))
    TABELA_FREQ<-data.frame(table(TABLE_ALL$VENCEDOR))
    TABELA_FREQ$PROB<- round(TABELA_FREQ$Freq/length(TABLE_ALL$VENCEDOR)*100,2)
    TABELA_FREQ<-TABELA_FREQ[order(TABELA_FREQ$Var1),]
    

    Total_simulation<-sum(SCORE_FREQ$Freq)
    
    # data frame return
    Stage         <- "Flop"
    Players       <- nPlayers
    Norm_Prob     <- round(1/nPlayers,2)
    Exat_prob_P01 <- 0
    Score_P01     <- MY_SCORE(nPlayers,y,cc=4)
    N_simulation  <- N_SIMULATION
    Prob_Win_S    <- round(TABELA_FREQ[TABELA_FREQ$Var1==1,][3],2)
    Decision      <- get_decision(Players, Prob_Win_S)
    Prob_score_01 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==1,]$Freq)/Total_simulation)*100,2)
    Prob_score_02 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==2,]$Freq)/Total_simulation)*100,2)
    Prob_score_03 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==3,]$Freq)/Total_simulation)*100,2)
    Prob_score_04 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==4,]$Freq)/Total_simulation)*100,2)
    Prob_score_05 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==5,]$Freq)/Total_simulation)*100,2)
    Prob_score_06 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==6,]$Freq)/Total_simulation)*100,2)
    Prob_score_07 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==7,]$Freq)/Total_simulation)*100,2)
    Prob_score_08 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==8,]$Freq)/Total_simulation)*100,2)
    Prob_score_09 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==9,]$Freq)/Total_simulation)*100,2)
    
    # prob jus community
    
    TIME01<-Sys.time()
    Time_Calc     <- round(TIME01-TIME00,2)
    df_f<-data.frame(Stage,Players,Norm_Prob,Exat_prob_P01,Score_P01,N_simulation,Prob_Win_S,Decision,
                     Prob_score_01,Prob_score_02,Prob_score_03,Prob_score_04,Prob_score_05,Prob_score_06,Prob_score_07,
                     Prob_score_08,Prob_score_09,
                     Time_Calc)
    
    df_f
    return(df_f)
  }
  
  ###################################################################################################################################
  # PRIMEIRA CARTAS NA MESA
  
  if(CARTA_PLAYER01_01!=CARTA_PLAYER01_02 & ROUND_ONE_01!="" & ROUND_TWO ==""){
    TIME00<-Sys.time()
    FIRST_CARD<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==CARTA_PLAYER01_01,][1])
    SECOND_CARD<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==CARTA_PLAYER01_02,][1])
    BOARD_01<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_01,][1])
    BOARD_02<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_02,][1])
    BOARD_03<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_03,][1])
    SEQ_CARDS00<-SEQ_CARDS[SEQ_CARDS!=FIRST_CARD & SEQ_CARDS!=SECOND_CARD & SEQ_CARDS!=BOARD_01 & SEQ_CARDS!=BOARD_02 & SEQ_CARDS!=BOARD_03]
    SEQ_CARDS00
  
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
        
        if(ff!=(nPlayers+1) & ff!=1 & ff!=(nPlayers*2+1)& ff!=(nPlayers*2+2) & ff!=(nPlayers*2+3)){
          SEQ_CARDS_TABLE[ff]<-SAMPLE_ROUND01[kk]
          kk<-kk+1
        }
      }
      
      y<-SEQ_CARDS_TABLE
      DF_VENCEDOR<-NION_POKER(nPlayers,y)  # data frame winner
      VENCEDOR<-DF_VENCEDOR$winner[1]
      WIN_SCORE<-DF_VENCEDOR$win_score
      
      TABLE00<-data.table::data.table(ID,VENCEDOR, WIN_SCORE)
      ifelse(ii==1,TABLE_ALL<-TABLE00, TABLE_ALL<-rbind(TABLE_ALL,TABLE00))
    }
  
      SCORE_FREQ<-data.frame(table(TABLE_ALL$WIN_SCORE))
      TABELA_FREQ<-data.frame(table(TABLE_ALL$VENCEDOR))
      TABELA_FREQ$PROB<- round(TABELA_FREQ$Freq/length(TABLE_ALL$VENCEDOR)*100,2)
      TABELA_FREQ<-TABELA_FREQ[order(TABELA_FREQ$Var1),]
  
    
    Total_simulation<-sum(SCORE_FREQ$Freq)
    
    # data frame return
    Stage         <- "Flop"
    Players       <- nPlayers
    Norm_Prob     <- round(1/nPlayers,2)
    Exat_prob_P01 <- 0
    Score_P01     <- MY_SCORE(nPlayers,y,cc=10)
    N_simulation  <- N_SIMULATION
    Prob_Win_S    <- round(TABELA_FREQ[TABELA_FREQ$Var1==1,][3],2)
    Decision      <- get_decision(Players, Prob_Win_S)
    Prob_score_01 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==1,]$Freq)/Total_simulation)*100,2)
    Prob_score_02 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==2,]$Freq)/Total_simulation)*100,2)
    Prob_score_03 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==3,]$Freq)/Total_simulation)*100,2)
    Prob_score_04 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==4,]$Freq)/Total_simulation)*100,2)
    Prob_score_05 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==5,]$Freq)/Total_simulation)*100,2)
    Prob_score_06 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==6,]$Freq)/Total_simulation)*100,2)
    Prob_score_07 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==7,]$Freq)/Total_simulation)*100,2)
    Prob_score_08 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==8,]$Freq)/Total_simulation)*100,2)
    Prob_score_09 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==9,]$Freq)/Total_simulation)*100,2)
    
    # prob jus community
    
    TIME01<-Sys.time()
    Time_Calc     <- round(TIME01-TIME00,2)
    df_f<-data.frame(Stage,Players,Norm_Prob,Exat_prob_P01,Score_P01,N_simulation,Prob_Win_S,Decision,
               Prob_score_01,Prob_score_02,Prob_score_03,Prob_score_04,Prob_score_05,Prob_score_06,Prob_score_07,
               Prob_score_08,Prob_score_09,
               Time_Calc)
    
    return(df_f)
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
      DF_VENCEDOR<-NION_POKER(nPlayers,y)  # data frame winner
      VENCEDOR<-DF_VENCEDOR$winner[1]
      WIN_SCORE<-DF_VENCEDOR$win_score
      
      TABLE00<-data.table::data.table(ID,VENCEDOR, WIN_SCORE)
      ifelse(ii==1,TABLE_ALL<-TABLE00, TABLE_ALL<-rbind(TABLE_ALL,TABLE00))
    }
    
    SCORE_FREQ<-data.frame(table(TABLE_ALL$WIN_SCORE))
    TABELA_FREQ<-data.frame(table(TABLE_ALL$VENCEDOR))
    TABELA_FREQ$PROB<- round(TABELA_FREQ$Freq/length(TABLE_ALL$VENCEDOR)*100,2)
    TABELA_FREQ<-TABELA_FREQ[order(TABELA_FREQ$Var1),]
    
    
    Total_simulation<-sum(SCORE_FREQ$Freq)
    
    # data frame return
    Stage         <- "Turn"
    Players       <- nPlayers
    Norm_Prob     <- round(1/nPlayers,2)
    Exat_prob_P01 <- 0
    Score_P01     <- MY_SCORE(nPlayers,y,cc=12)
    N_simulation  <- N_SIMULATION
    Prob_Win_S    <- round(TABELA_FREQ[TABELA_FREQ$Var1==1,][3],2)
    Decision      <- get_decision(Players, Prob_Win_S)
    Prob_score_01 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==1,]$Freq)/Total_simulation)*100,2)
    Prob_score_02 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==2,]$Freq)/Total_simulation)*100,2)
    Prob_score_03 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==3,]$Freq)/Total_simulation)*100,2)
    Prob_score_04 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==4,]$Freq)/Total_simulation)*100,2)
    Prob_score_05 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==5,]$Freq)/Total_simulation)*100,2)
    Prob_score_06 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==6,]$Freq)/Total_simulation)*100,2)
    Prob_score_07 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==7,]$Freq)/Total_simulation)*100,2)
    Prob_score_08 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==8,]$Freq)/Total_simulation)*100,2)
    Prob_score_09 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==9,]$Freq)/Total_simulation)*100,2)
    
    # prob jus community
    
    TIME01<-Sys.time()
    Time_Calc     <- round(TIME01-TIME00,2)
    df_f<-data.frame(Stage,Players,Norm_Prob,Exat_prob_P01,Score_P01,N_simulation,Prob_Win_S,Decision,
                     Prob_score_01,Prob_score_02,Prob_score_03,Prob_score_04,Prob_score_05,Prob_score_06,Prob_score_07,
                     Prob_score_08,Prob_score_09,
                     Time_Calc)
    
    return(df_f)
    
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
      DF_VENCEDOR<-NION_POKER(nPlayers,y)  # data frame winner
      VENCEDOR<-DF_VENCEDOR$winner[1]
      WIN_SCORE<-DF_VENCEDOR$win_score
      
      TABLE00<-data.table::data.table(ID,VENCEDOR, WIN_SCORE)
      ifelse(ii==1,TABLE_ALL<-TABLE00, TABLE_ALL<-rbind(TABLE_ALL,TABLE00))
    }
    SCORE_FREQ<-data.frame(table(TABLE_ALL$WIN_SCORE))
    TABELA_FREQ<-data.frame(table(TABLE_ALL$VENCEDOR))
    #CALCULO %
    TABELA_FREQ$PROB<- round(TABELA_FREQ$Freq/length(TABLE_ALL$VENCEDOR)*100,2)
    TABELA_FREQ<-TABELA_FREQ[order(TABELA_FREQ$Var1),]
    
    
    Total_simulation<-sum(SCORE_FREQ$Freq)
    
    # data frame return
    Stage         <- "River"
    Players       <- nPlayers
    Norm_Prob     <- round(1/nPlayers,2)
    Exat_prob_P01 <- 0
    Score_P01     <- MY_SCORE(nPlayers,y,cc=14)
    N_simulation  <- N_SIMULATION
    Prob_Win_S    <- round(TABELA_FREQ[TABELA_FREQ$Var1==1,][3],2)
    Decision      <- get_decision(Players, Prob_Win_S)
    Prob_score_01 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==1,]$Freq)/Total_simulation)*100,2)
    Prob_score_02 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==2,]$Freq)/Total_simulation)*100,2)
    Prob_score_03 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==3,]$Freq)/Total_simulation)*100,2)
    Prob_score_04 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==4,]$Freq)/Total_simulation)*100,2)
    Prob_score_05 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==5,]$Freq)/Total_simulation)*100,2)
    Prob_score_06 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==6,]$Freq)/Total_simulation)*100,2)
    Prob_score_07 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==7,]$Freq)/Total_simulation)*100,2)
    Prob_score_08 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==8,]$Freq)/Total_simulation)*100,2)
    Prob_score_09 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==9,]$Freq)/Total_simulation)*100,2)
    
    # prob jus community
    TIME01<-Sys.time()
    Time_Calc     <- round(TIME01-TIME00,2)
    df_f<-data.frame(Stage,Players,Norm_Prob,Exat_prob_P01,Score_P01,N_simulation,Prob_Win_S,Decision,
                     Prob_score_01,Prob_score_02,Prob_score_03,Prob_score_04,Prob_score_05,Prob_score_06,Prob_score_07,
                     Prob_score_08,Prob_score_09,
                     Time_Calc)
    
    return(df_f)
    
  }
}







#######################################################################################
# probabilidade da mesa
PROB_COMMUNITY<-function( Data_frame_cards,
                                 N_SIMULATION,
                                 nPlayers,
                                 ROUND_ONE_01,
                                 ROUND_ONE_02,
                                 ROUND_ONE_03,
                                 ROUND_TWO,
                                 ROUND_THREE){
  
  CARTA_PLAYER01_01<-""
  CARTA_PLAYER01_02<-""
  
  cardDeck <- c(outer(c(2:10,"J","Q","K","A"),
                      c("H","S","C","D"),
                      paste0))
  
  n_cards <- 1:length(cardDeck)
  Data_frame_cards <- data.frame(n_cards, cardDeck)
  
  # DETERMINAR ERRO E CONDIÇÕES
  
  
  # INICIAR SIMULAÇÃO
  
  JOGADORES_NAMES<-strsplit(replicate(1, paste0("Player", sep="", 1:nPlayers, collapse=" "))[1]," ")
  PROB_MEAN<-round(1/nPlayers*100,2)
  

  
  ###################################################################################################################################
  # PRIMEIRA CARTAS NA MESA
  
  if(CARTA_PLAYER01_01==CARTA_PLAYER01_02 & ROUND_ONE_01!="" & ROUND_TWO ==""){
    TIME00<-Sys.time()
    FIRST_CARD<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==CARTA_PLAYER01_01,][1])
    SECOND_CARD<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==CARTA_PLAYER01_02,][1])
    BOARD_01<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_01,][1])
    BOARD_02<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_02,][1])
    BOARD_03<-as.numeric(Data_frame_cards[Data_frame_cards$cardDeck==ROUND_ONE_03,][1])
    SEQ_CARDS00<-SEQ_CARDS[SEQ_CARDS!=FIRST_CARD & SEQ_CARDS!=SECOND_CARD & SEQ_CARDS!=BOARD_01 & SEQ_CARDS!=BOARD_02 & SEQ_CARDS!=BOARD_03]
    SEQ_CARDS00
    
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
        
        if(ff!=(nPlayers+1) & ff!=1 & ff!=(nPlayers*2+1)& ff!=(nPlayers*2+2) & ff!=(nPlayers*2+3)){
          SEQ_CARDS_TABLE[ff]<-SAMPLE_ROUND01[kk]
          kk<-kk+1
        }
      }
      
      y<-SEQ_CARDS_TABLE
      DF_VENCEDOR<-NION_POKER(nPlayers,y)  # data frame winner
      VENCEDOR<-DF_VENCEDOR$winner[1]
      WIN_SCORE<-DF_VENCEDOR$win_score
      
      TABLE00<-data.table::data.table(ID,VENCEDOR, WIN_SCORE)
      ifelse(ii==1,TABLE_ALL<-TABLE00, TABLE_ALL<-rbind(TABLE_ALL,TABLE00))
    }
    
    SCORE_FREQ<-data.frame(table(TABLE_ALL$WIN_SCORE))
    TABELA_FREQ<-data.frame(table(TABLE_ALL$VENCEDOR))
    TABELA_FREQ$PROB<- round(TABELA_FREQ$Freq/length(TABLE_ALL$VENCEDOR)*100,2)
    TABELA_FREQ<-TABELA_FREQ[order(TABELA_FREQ$Var1),]
    
    
    Total_simulation<-sum(SCORE_FREQ$Freq)
    
    # data frame return
    Stage         <- "Flop"
    Players       <- nPlayers
    Norm_Prob     <- round(1/nPlayers,2)
    Exat_prob_P01 <- 0
    Score_P01     <- MY_SCORE(nPlayers,y,cc=10)
    N_simulation  <- N_SIMULATION
    Prob_Win_S    <- round(TABELA_FREQ[TABELA_FREQ$Var1==1,][3],2)
    Decision      <- get_decision(Players, Prob_Win_S)
    Prob_score_01 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==1,]$Freq)/Total_simulation)*100,2)
    Prob_score_02 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==2,]$Freq)/Total_simulation)*100,2)
    Prob_score_03 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==3,]$Freq)/Total_simulation)*100,2)
    Prob_score_04 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==4,]$Freq)/Total_simulation)*100,2)
    Prob_score_05 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==5,]$Freq)/Total_simulation)*100,2)
    Prob_score_06 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==6,]$Freq)/Total_simulation)*100,2)
    Prob_score_07 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==7,]$Freq)/Total_simulation)*100,2)
    Prob_score_08 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==8,]$Freq)/Total_simulation)*100,2)
    Prob_score_09 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==9,]$Freq)/Total_simulation)*100,2)
    
    # prob jus community
    
    TIME01<-Sys.time()
    Time_Calc     <- round(TIME01-TIME00,2)
    df_f<-data.frame(Stage,Players,Norm_Prob,Exat_prob_P01,Score_P01,N_simulation,Prob_Win_S,Decision,
                     Prob_score_01,Prob_score_02,Prob_score_03,Prob_score_04,Prob_score_05,Prob_score_06,Prob_score_07,
                     Prob_score_08,Prob_score_09,
                     Time_Calc)
    
    return(df_f)
  }
  
  ###################################################################################################################################
  # ROUND TWO
  
  if(CARTA_PLAYER01_01==CARTA_PLAYER01_02 & ROUND_ONE_01!="" & ROUND_TWO !="" &   ROUND_THREE ==""){
    
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
      DF_VENCEDOR<-NION_POKER(nPlayers,y)  # data frame winner
      VENCEDOR<-DF_VENCEDOR$winner[1]
      WIN_SCORE<-DF_VENCEDOR$win_score
      
      TABLE00<-data.table::data.table(ID,VENCEDOR, WIN_SCORE)
      ifelse(ii==1,TABLE_ALL<-TABLE00, TABLE_ALL<-rbind(TABLE_ALL,TABLE00))
    }
    
    SCORE_FREQ<-data.frame(table(TABLE_ALL$WIN_SCORE))
    TABELA_FREQ<-data.frame(table(TABLE_ALL$VENCEDOR))
    TABELA_FREQ$PROB<- round(TABELA_FREQ$Freq/length(TABLE_ALL$VENCEDOR)*100,2)
    TABELA_FREQ<-TABELA_FREQ[order(TABELA_FREQ$Var1),]
    
    
    Total_simulation<-sum(SCORE_FREQ$Freq)
    
    # data frame return
    Stage         <- "Turn"
    Players       <- nPlayers
    Norm_Prob     <- round(1/nPlayers,2)
    Exat_prob_P01 <- 0
    Score_P01     <- MY_SCORE(nPlayers,y,cc=12)
    N_simulation  <- N_SIMULATION
    Prob_Win_S    <- round(TABELA_FREQ[TABELA_FREQ$Var1==1,][3],2)
    Decision      <- get_decision(Players, Prob_Win_S)
    Prob_score_01 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==1,]$Freq)/Total_simulation)*100,2)
    Prob_score_02 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==2,]$Freq)/Total_simulation)*100,2)
    Prob_score_03 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==3,]$Freq)/Total_simulation)*100,2)
    Prob_score_04 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==4,]$Freq)/Total_simulation)*100,2)
    Prob_score_05 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==5,]$Freq)/Total_simulation)*100,2)
    Prob_score_06 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==6,]$Freq)/Total_simulation)*100,2)
    Prob_score_07 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==7,]$Freq)/Total_simulation)*100,2)
    Prob_score_08 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==8,]$Freq)/Total_simulation)*100,2)
    Prob_score_09 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==9,]$Freq)/Total_simulation)*100,2)
    
    # prob jus community
    
    TIME01<-Sys.time()
    Time_Calc     <- round(TIME01-TIME00,2)
    df_f<-data.frame(Stage,Players,Norm_Prob,Exat_prob_P01,Score_P01,N_simulation,Prob_Win_S,Decision,
                     Prob_score_01,Prob_score_02,Prob_score_03,Prob_score_04,Prob_score_05,Prob_score_06,Prob_score_07,
                     Prob_score_08,Prob_score_09,
                     Time_Calc)
    
    return(df_f)
    
  }
  ###################################################################################################################################
  # ROUND THREE
  
  if(CARTA_PLAYER01_01==CARTA_PLAYER01_02 & ROUND_ONE_01!="" & ROUND_TWO !="" &   ROUND_THREE !=""){
    
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
      DF_VENCEDOR<-NION_POKER(nPlayers,y)  # data frame winner
      VENCEDOR<-DF_VENCEDOR$winner[1]
      WIN_SCORE<-DF_VENCEDOR$win_score
      
      TABLE00<-data.table::data.table(ID,VENCEDOR, WIN_SCORE)
      ifelse(ii==1,TABLE_ALL<-TABLE00, TABLE_ALL<-rbind(TABLE_ALL,TABLE00))
    }
    SCORE_FREQ<-data.frame(table(TABLE_ALL$WIN_SCORE))
    TABELA_FREQ<-data.frame(table(TABLE_ALL$VENCEDOR))
    #CALCULO %
    TABELA_FREQ$PROB<- round(TABELA_FREQ$Freq/length(TABLE_ALL$VENCEDOR)*100,2)
    TABELA_FREQ<-TABELA_FREQ[order(TABELA_FREQ$Var1),]
    
    
    Total_simulation<-sum(SCORE_FREQ$Freq)
    
    # data frame return
    Stage         <- "River"
    Players       <- nPlayers
    Norm_Prob     <- round(1/nPlayers,2)
    Exat_prob_P01 <- 0
    Score_P01     <- MY_SCORE(nPlayers,y,cc=14)
    N_simulation  <- N_SIMULATION
    Prob_Win_S    <- round(TABELA_FREQ[TABELA_FREQ$Var1==1,][3],2)
    Decision      <- get_decision(Players, Prob_Win_S)
    Prob_score_01 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==1,]$Freq)/Total_simulation)*100,2)
    Prob_score_02 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==2,]$Freq)/Total_simulation)*100,2)
    Prob_score_03 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==3,]$Freq)/Total_simulation)*100,2)
    Prob_score_04 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==4,]$Freq)/Total_simulation)*100,2)
    Prob_score_05 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==5,]$Freq)/Total_simulation)*100,2)
    Prob_score_06 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==6,]$Freq)/Total_simulation)*100,2)
    Prob_score_07 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==7,]$Freq)/Total_simulation)*100,2)
    Prob_score_08 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==8,]$Freq)/Total_simulation)*100,2)
    Prob_score_09 <- round((sum(SCORE_FREQ[SCORE_FREQ$Var1==9,]$Freq)/Total_simulation)*100,2)
    
    # prob jus community
    TIME01<-Sys.time()
    Time_Calc     <- round(TIME01-TIME00,2)
    df_f<-data.frame(Stage,Players,Norm_Prob,Exat_prob_P01,Score_P01,N_simulation,Prob_Win_S,Decision,
                     Prob_score_01,Prob_score_02,Prob_score_03,Prob_score_04,Prob_score_05,Prob_score_06,Prob_score_07,
                     Prob_score_08,Prob_score_09,
                     Time_Calc)
    
    return(df_f)
    
  }
}


