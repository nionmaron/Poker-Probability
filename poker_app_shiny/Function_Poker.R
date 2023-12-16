

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


####################
#'testRoundOfPoker
#'
#'Run a test round of poker.
#'
#'@return Outputs a plot window showing the cards dealt as well as prints to the console the summary text, i.e., each hand's score and the winner.
#'@seealso \code{\link{deal}}, \code{\link{assignToPlayers}}, \code{\link{assignToBoard}}, \code{\link{hand}}, \code{\link{showdown}}, \code{\link{tiebreaker}}, and \code{\link{cgiPlayers}}
#'@examples 
#'testRoundOfPoker()
#'@export
testRoundOfPoker <- function() {
  alias <- c("Player 1", "Player 2","Player 3", "Player 4", "Player 5", "Player 6", "Player 7", "Player 8", "Player 9")
  nPlayers <- length(alias)
  position <- nPlayers
  
  y <- deal(nPlayers, position)
  players <- assignToPlayers(nPlayers, position, y)
  board <- assignToBoard(y)
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
  
  cat("Please look at the graphics window to see the current hand.\n\nThe score of each player is one of the following:\n\t9 = Straight Flush\n\t8 = Four of a Kind\n\t7 = Full House\n\t6 = Flush\n\t5 = Straight\n\t4 = Three of a Kind\n\t3 = Two Pair\n\t2 = Pair\n\t1 = Highcard\nThe scores for this hand are:\n\tPlayer 1: ",score[1],"\n\tPlayer 2: ",score[2],"\n\tPlayer 3: ",score[3],"\n\tPlayer 4: ",score[4],"\n\tPlayer 5: ",score[5],"\n\tPlayer 6: ",score[6],"\n\tPlayer 7: ",score[7],"\n\tPlayer 8: ",score[8],"\n\tPlayer 9: ",score[9],".\nThe winners of this hand are Players",winner,".\n\nThank you for playing.")
}


#'deal
#'
#'Generate Player+Community cards = 2x(nPlayers)+5 cards.
#'
#'@param		nPlayers number of hands to deal as 
#'												integer in \{2, ... , 9\}
#'@param		position dealer position 		as integer in \{2, ..., nPlayers\}
#'@return		y : cards dealt in hole			as vector[nCards] in \{1, 2, ..., 52\}
#'@examples
#'deal(9,9)
#'deal(9,1)
#'@export
deal <- function(nPlayers, position) {
  nCards <- 2*nPlayers+5
  y <- numeric(nCards)
  #create a vector for the cards
  y <- sample(1:52, nCards, replace=FALSE, prob=rep(1/52,52))
  #deal numbers in \{1, 2, ... , 52\}
  y
}


#'dotTestDealer
#'
#'Assume player 1 already has cards.  For remaining players, generate Player+Community cards = 2x(nPlayers-1)+5 cards. \cr
#'
#'@param nPlayers number of hands to deal as 
#'												integer in \{2, ... , 9\}
#'@param position dealer position 	as integer in \{2, ..., nPlayers\}
#'@param holeCards the hand of player 1 as vector[2] in \{1, 2, ..., 52\}
#'@return y : cards dealt in hole as		vector[nCards] in \{1, 2, ..., 52\}
#'@examples
#'dotTestDealer(9,9,c(1,52))
#'dotTestDealer(9,5,c(1,52))
#'dotTestDealer(5,2,c(3,42))
#'@export
dotTestDealer <- function(nPlayers, position,holeCards) {
  nCards <- 2*nPlayers+5
  y <- numeric(nCards)
  #create a vector for the cards
  y[c(1,nPlayers+1)] <- holeCards
  #the cards for player 1
  y[-c(1,nPlayers+1)] <- sample(subset(1:52, 1:52 != holeCards), nCards-2)
  #deal numbers in \{1, 2, ... , 52\}
  y
}



#'assignToPlayers
#'
#'A standard deal situation beginnng the deal at the left of the dealer.
#'
#'@param nPlayers number of hands to deal as
#'												integer in \{2, ... , 9\}
#'@param position dealer position as	integer in \{2, ..., nPlayers\}
#'@param y cards dealt as					vector[2*nPlayers+5] in \{1, 2, ..., 52\}
#'@seealso \code{\link{dotTransformToAbsolute}}
#'@return \tabular{ll}{players :	the hole cards in absolute position \tab
#'												as matrix[nPlayers, 4] in \{1, 2, ..., 52\} \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2}
#'@examples
#'assignToPlayers(9,9,1:23)
#'assignToPlayers(9,1,1:23)
#'assignToPlayers(9,1,c(1:17,24,48:52))
#'@export
assignToPlayers <- function(nPlayers, position, y) {
  players <- matrix(0, nPlayers, 2) 
  #create a vector for the hole cards
  for (i in 1:2) {
    for (k in 1:nPlayers) {	
      players[dotTransformToAbsolute(nPlayers,position,k), i] <- y[(i-1)*nPlayers+k]
      #(i-1)*nPlayers+k : number of cards dealt at this time
      #integer in {1, 2, ..., 2*nPlayers}
    }
  }
  players
}



#'assignToBoard
#'
#'Deal 3 community cards.
#'
#'@param y cards dealt as					vector[2*nPlayers+3] in \{1, 2, ..., 52\}
#'@return board :	the board cards	as		vector[5] in \{1, 2, ..., 52\}
#'@examples
#'assignToBoard(1:23)
#'assignToBoard(c(1:17,24,48:52))
#'@export
assignToBoard <- function(y) {
  board <- numeric(5)
  #create a vector for the board cards
  board <- y[(length(y)-4):length(y)]
  #assign the board cards
  board
}


#'hand
#'
#'Assemble the 7 card hands.
#'
#'@param players : \tabular{ll}{the hole cards as			matrix[nPlayers, 4] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2}
#'@param board the board cards	as		vector[5] in \{1, 2, ..., 52\}
#'@return \tabular{ll}{cards : the 7 card hand as
#'												matrix[nPlayers, 14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab 
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@seealso \code{\link{dotTransformToRank}} and \code{\link{dotTransformToSuit}}
#'@examples
#'hand(matrix(1:18,9,2,byrow=TRUE),19:23)
#'hand(matrix(c(1:9,14:22),9,2),48:52)
#'@export
hand <- function(players, board) {
  nPlayers <- nrow(players)
  #create a variable for the number of players
  cards <- matrix(0, nrow=nPlayers, ncol = 14)
  #create a variable for the cards
  for (i in 1:nPlayers) {
    j <- 1:2
    cards[i, 2*j-1] <- dotTransformToRank(players[i,])
    #2*j-1 : column for rank	#integer in {1, 3}
    cards[i, 2*j] <- dotTransformToSuit(players[i,])
    #2*j : column for rank	#integer in {2, 4}
    j <- 1:5
    cards[i, 4+2*j-1] <- dotTransformToRank(board)
    #4+2*j-1 : column for rank	#integer in {5, 7, 9, 11, 13}
    cards[i, 4+2*j] <- dotTransformToSuit(board)
    #4+2*j :column for suit		#integer in {6, 8, 10, 12, 14}
  }
  cards
}


#'dotTransformToRank
#'
#'Determine the rank of a card.
#'
#'@param y number corresponding to card as
#'												integer in \{1, 2, ... , 52\} \cr
#'@return \tabular{ll}{rank: rank of card y as  
#'												integer in \{2, ... , 14\} \tab \cr \tab
#'												 2 = deuce \cr \tab
#'												 . \cr \tab
#'												 . \cr \tab
#'												 . \cr \tab
#'												 11 = jack \cr \tab
#'												 12 = queen \cr \tab
#'												 13 = king \cr \tab
#'												 14 = ace}
#'@examples
#'dotTransformToRank(1)
#'dotTransformToRank(13)
#'dotTransformToRank(14)
#'dotTransformToRank(26)
#'@export
dotTransformToRank <- function(y) {
  rank <- (y-1) %% 13+2
  #(y-1) %% 13+2: rank of this card
  rank
}


#'dotTransformToSuit
#'
#'Determine the suit of a card.
#'
#'@param y number corresponding to card as
#'												integer in \{1, 2, ... , 52\}
#'@return \tabular{ll}{suit: suit of card y as
#'												integer in \{1, 2, 3, 4\} \tab \cr \tab
#'												 1 = spade \cr \tab
#'												 2 = club \cr \tab
#'												 3 = heart \cr \tab
#'												 4 = diamond}
#'@examples
#'dotTransformToSuit(1)
#'dotTransformToSuit(13)
#'dotTransformToSuit(14)
#'dotTransformToSuit(26)
#'@export
dotTransformToSuit <- function(y) {
  suit <-  (y-1) %/% 13+1
  #(y-1) %/% 13+1: suit of this card
  suit
}


#'dotTransformToNumber
#'
#'Determine the card from a rank and suit.
#'
#'@param rank : \tabular{ll}{rank of card y as  
#'												integer in \{2, ... , 14\} \tab \cr \tab
#'												 2 = deuce \cr \tab
#'												 . \cr \tab
#'												 . \cr \tab
#'												 . \cr \tab
#'												 11 = jack \cr \tab
#'												 12 = queen \cr \tab
#'												 13 = king \cr \tab
#'												 14 = ace}
#'@param suit : \tabular{ll}{suit of card y as
#'												integer in \{1, 2, 3, 4\} \tab \cr \tab
#'												 1 = spade \cr \tab
#'												 2 = club \cr \tab
#'												 3 = heart \cr \tab
#'												 4 = diamond}
#'@return y: number corresponding to card as
#'												integer in \{1, 2, ... , 52\}
#'@examples
#'dotTransformToNumber(2,1)
#'dotTransformToNumber(14,1)
#'dotTransformToNumber(2,2)
#'dotTransformToNumber(14,2)
#'@export
dotTransformToNumber <- function(rank, suit) {
  y <- 13*(suit-1)+(rank-2)+1
  #the number of this card
  y
}


#'transformToRelative
#'
#'Transforms an absolute position (i.e., seat at the table) into a relative position (i.e., seats behind the dealer)
#'
#'@param nPlayers number of hands to deal as
#'												integer in \{2, ... , 9\}
#'@param position dealer position as		integer in \{2, ..., nPlayers\}
#'@param j absolute position of a player as
#'												integer in \{1, 2, ... , nPlayers\}
#'@return k : relative position of a player as
#'												integer in \{1, 2, ... , nPlayers\}
#'@examples
#'transformToRelative(9,9,9)
#'transformToRelative(9,9,8)
#'transformToRelative(9,1,9)
#'transformToRelative(9,5,2)
#'@export
transformToRelative <- function(nPlayers, position, j) {
  if (j-position == 0) k <- 0
  else if (j-position > 0) k <- j-position		
  else k <- j-position+nPlayers
  k
}


#'dotTransformToAbsolute
#'
#'Transform a relative position (i.e., seats behind the dealer) into an absolute position (i.e., seat at the table).
#'
#'@param nPlayers number of hands to deal as
#'												integer in \{2, ... , 9\}
#'@param position dealer position as		integer in \{2, ..., nPlayers\}
#'@param k relative position of a player as
#'												integer in \{1, 2, ... , nPlayers\}
#'@return j : absolute position of a player as
#'												integer in \{1, 2, ... , nPlayers\}
#'@examples
#'dotTransformToAbsolute(9,9,0)
#'dotTransformToAbsolute(9,9,8)
#'dotTransformToAbsolute(9,1,8)
#'dotTransformToAbsolute(9,5,6)

#'@export
dotTransformToAbsolute <- function(nPlayers, position, k) {
  j <- (k+position-1)%%nPlayers+1
  j
}


#'dotScorer
#'
#'Determine the ranking of one hand.
#'
#'@param cardsRow : \tabular{ll}{one 7 card hand as
#'												vector[14] \tab  \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@return \tabular{ll}{ranking : the rank of the hand as
#'												integer in \{2, ... , 9\} \tab \cr \tab
#'												 9 = Straight Flush \cr \tab
#'												 8 = Four of a Kind \cr \tab
#'												 7 = Full House \cr \tab
#'												 6 = Flush \cr \tab
#'												 5 = Straight \cr \tab
#'												 4 = Three of a Kind \cr \tab
#'												 3 = Two Pair \cr \tab
#'												 2 = One Pair \cr \tab
#'												 1 = High Card}
#'@seealso \code{\link{dotTransformToNumber}}, \code{\link{dotTransformToRank}}
#'@examples
#'dotScorer(c(2,1,3,2,5,3,6,4,7,1,13,2,14,2))
#'dotScorer(c(2,1,2,2,5,3,6,4,7,1,13,2,14,2))
#'dotScorer(c(2,1,2,2,5,3,5,4,7,1,13,2,14,2))
#'dotScorer(c(2,1,2,2,2,3,5,4,7,1,13,2,14,2))
#'dotScorer(c(2,1,3,2,4,3,5,4,6,1,13,2,14,2))
#'dotScorer(c(2,1,3,1,5,1,6,1,7,1,13,2,14,2))
#'dotScorer(c(2,1,2,2,2,8,13,8,7,1,13,2,14,2))
#'dotScorer(c(2,1,2,2,2,3,2,4,7,1,13,2,14,2))
#'dotScorer(c(2,1,3,1,4,1,5,1,6,1,7,1,14,2))
#'@export
dotScorer <- function(cardsRow) {
  ranks <- seq(from = 1, to = 13, by = 2)
  #the columns of rank
  suits <- seq(from = 2, to = 14, by = 2)
  #the columns of suit
  sortedSuits <- sort(cardsRow[suits])
  #sort the suits from low to high
  sortedRanks <- sort(cardsRow[ranks])
  #sort the ranks
  suitValues <- rle(sortedSuits)$values
  #the unique suits
  suitLengths <- rle(sortedSuits)$lengths
  #the length of the unique suits
  rankValues <- rle(sortedRanks)$values
  #the unique ranks
  rankLengths <- rle(sortedRanks)$lengths
  #the length of the unique ranks
  k <- length(rankValues)
  #create a variable for the length of the length of unique ranks
  kgt4 <- k - 4
  #create a variable for the number of sequences to check in a straight
  straight <- FALSE
  #create an indicator variable for the presence of a straight
  straightFlush <- FALSE
  #create an indicator variable for the presence of a straight flush
  #determine the ranking of the hand
  #high card
  if (sum(rankLengths > 1) == 0) ranking <- 1
  #one pair
  if (sum(rankLengths == 2) == 1) ranking <- 2
  #two pair 
  if (sum(rankLengths == 2) >= 2) ranking <- 3
  #there can be two or three pairs
  #three of a kind
  if (sum(rankLengths == 3) >= 1) ranking <- 4
  #there can be one or two sets
  #straight 
  if (k >= 5) {
    #if there are 5 unique ranks
    if (sum(rankValues[c(k,1,2,3,4)] == c(14,2,3,4,5)) == 5) straight <- TRUE
    #check for wrap around
    #aka a low straight or wheel
    for (i in 1:kgt4) {
      #check kgt4 number of sequences
      if (rankValues[i+4] == (rankValues[i]+4)) straight <- TRUE
      #if the (i+4)th rank is the ith rank plus four 
      #the high card is the (i+4)th rank
    }
    if (straight == TRUE) ranking <- 5
  }
  #flush
  if (sum(suitLengths >= 5) == 1 ) ranking <- 6
  #full house
  if (sum(rankLengths == 3) == 2 | (sum(rankLengths == 3) == 1 & sum(rankLengths == 2) >= 1)) {
    #if there are 
    #two sets
    #or
    #a set and one/two pairs
    ranking <- 7	
  }
  #four of a kind
  if (sum(rankLengths == 4) == 1) ranking <- 8
  #straight flush
  if (straight == TRUE & sum(suitLengths >= 5) == 1) {
    #if there is
    #a straight
    #and 
    #a flush
    yTemp <- dotTransformToNumber(cardsRow[ranks],cardsRow[suits])
    #transform cards to number
    yTemp <- sort(yTemp)
    #sort the numbers	
    for (i in 0:3){
      #check each suit for a flush and a wheel
      if (sum(yTemp %in% c(13+13*i,1+13*i,2+13*i,3+13*i,4+13*i)) == 5) straightFlush <- TRUE
      #check each suit for a flush and a straight
      if (sum(yTemp %in% c(1+13*i,2+13*i,3+13*i,4+13*i,5+13*i)) == 5) straightFlush <- TRUE
      if (sum(yTemp %in% c(2+13*i,3+13*i,4+13*i,5+13*i,6+13*i)) == 5) straightFlush <- TRUE
      if (sum(yTemp %in% c(3+13*i,4+13*i,5+13*i,6+13*i,7+13*i)) == 5) straightFlush <- TRUE
      if (sum(yTemp %in% c(4+13*i,5+13*i,6+13*i,7+13*i,8+13*i)) == 5) straightFlush <- TRUE
      if (sum(yTemp %in% c(5+13*i,6+13*i,7+13*i,8+13*i,9+13*i)) == 5) straightFlush <- TRUE
      if (sum(yTemp %in% c(6+13*i,7+13*i,8+13*i,9+13*i,10+13*i)) == 5) straightFlush <- TRUE
      if (sum(yTemp %in% c(7+13*i,8+13*i,9+13*i,10+13*i,11+13*i)) == 5) straightFlush <- TRUE
      if (sum(yTemp %in% c(8+13*i,9+13*i,10+13*i,11+13*i,12+13*i)) == 5) straightFlush <- TRUE
      if (sum(yTemp %in% c(9+13*i,10+13*i,11+13*i,12+13*i,13+13*i)) == 5) straightFlush <- TRUE
    }
    if (straightFlush == TRUE) ranking <- 9
  }
  ranking
}


#'showdown
#'
#'Determine the ranking of the hands.
#'
#'@param cards : \tabular{ll}{the 7 card hand as 
#'												matrix[nPlayers, 14]  \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2}
#'@return \tabular{ll}{score : the score of the hand in absolute terms as
#'												vector[nPlayers] \tab \cr \tab
#'												 9 = Straight Flush \cr \tab
#'												 8 = Four of a Kind \cr \tab
#'												 7 = Full House \cr \tab
#'												 6 = Flush \cr \tab
#'												 5 = Straight \cr \tab
#'												 4 = Three of a Kind \cr \tab
#'												 3 = Two Pair \cr \tab
#'												 2 = One Pair \cr \tab
#'												 1 = High Card}
#'@seealso \code{\link{dotScorer}}
#'@examples
#'showdown(matrix( c( 2,1,3,2,5,3,6,4,7,1,13,2,14,2,2,3,2,4,5,1,6,2,7,3,13,4,14,4),2,14,byrow=TRUE))
#'@export
showdown <- function(cards) {
  score <- apply(cards,1,dotScorer)
  score
}


#'dotHighcardCompare
#'
#'Determine the player(s) with the high card.
#'
#'@param rankMatrix : \tabular{ll}{the ranks from the 7 card hand as
#'												matrix[nPlayers, 7] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: rank of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col7: rank of card 7}
#'@return winner : absolute position of the winner as
#'												vector
#'@examples
#'dotHighcardCompare(matrix(c(2,4,5,6,7,13,14,2,3,5,6,7,13,14),2,7,byrow=TRUE))
#'dotHighcardCompare(matrix(c(2,3,5,6,7,13,14,2,3,5,6,7,13,14),2,7,byrow=TRUE))
#'@export
dotHighcardCompare <- function(rankMatrix) {
  #rankMatrix <- t(apply(rankMatrix,1,sort,decreasing=TRUE))
  #sort the ranks 
  #CANT APPLY IF RANKMATRIX IS A ROW
  #UNNECESSARY IF RANKMATRIX IS SORTED CORRECTLY
  
  for (step in 2:ncol(rankMatrix)) {
    Tie <- which(rankMatrix[,step-1]==max(rankMatrix[,step-1])) 
    #determine which rows are tied
    rankMatrix[-Tie,] <- 0		
    #zero out the others
    winner <- which(rankMatrix[,step]==max(rankMatrix[,step]))
    #which of the hands have the high card hand
  }
  winner
}


#'dotHighcard
#'
#'Determine the player(s) with a high card hand.
#'
#'@param cards : \tabular{ll}{ the 7 card hand as
#'												matrix[nPlayers, 14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@return winner : absolute position of the winner as
#'												vector 
#'@seealso \code{\link{dotHighcardCompare}}
#'@examples
#'dotHighcard(matrix(c(2,1,14,2,5,3,6,4,7,1,13,2,14,3,2,3,3,4,5,1,6,2,7,3,13,4,14,1),2,14,byrow=TRUE))
#'dotHighcard(matrix(c(2,1,3,2,5,3,6,4,7,1,13,2,14,3,2,3,3,4,5,1,6,2,7,3,13,4,14,1),2,14,byrow=TRUE))
#'@export
dotHighcard <- function(cards) {
  ranks <- seq(from = 1, to = 13, by = 2)	
  #the columns for rank
  temp <- cards[, ranks] 	#assume score = 1 for all players
  #create a copy of cards with ranks only
  #sorts the ranks
  temp <- t(apply(temp,1,sort,decreasing=TRUE))
  #systematic scan
  winner <- dotHighcardCompare(temp)
  winner
}	


#'dotPairRanker
#'
#'Determine the rank of the pair.  Notes: dotPairRanker requires a hand with a score of 2 (i.e., a pair). This functions works best when ranks are sorted in decreasing order.
#'
#'@param oneHand : \tabular{ll}{a sorted hand with ranks only as
#'												vector[7] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: rank of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col7: rank of card 7}
#'@return pairRank : the rank of the pair as
#'												vector
#'@examples
#'dotPairRanker(c(2,2,5,6,7,13,14))
#'@export
dotPairRanker <- function(oneHand) {
  rankValues <- rle(oneHand)$values
  #the unique ranks
  rankLengths <- rle(oneHand)$lengths
  #the length of the unique ranks
  pairRank <- rankValues[rankLengths == 2]
  #the rank of the pair
  pairRank
}


#'dotPair
#'
#'Determine the player(s) with the highest pair and kicker cards.
#'
#'@param nPlayers number of hands as		integer in \{2, ... , 9\}
#'@param cards : \tabular{ll}{the 7 card hands as
#'												matrix[nPlayers, 14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@param score : \tabular{ll}{the score of the hands in absolute terms as
#'												vector[nPlayers] \tab \cr \tab
#'												 9 = Straight Flush \cr \tab
#'												 8 = Four of a Kind \cr \tab
#'												 7 = Full House \cr \tab
#'												 6 = Flush \cr \tab
#'												 5 = Straight \cr \tab
#'												 4 = Three of a Kind \cr \tab
#'												 3 = Two Pair \cr \tab
#'												 2 = One Pair \cr \tab
#'												 1 = High Card}
#'@return winner : absolute position of the winner as
#'												vector
#'@seealso \code{\link{dotPairRanker}} and \code{\link{dotHighcardCompare}}
#'@examples
#'cards <- c(2,3,4,1,1,1,2,3,6,2,2,2,4,4,4,3,3,3,11,11,11,3,3,3,13,13,13)
#'cards <- c(cards,3,3,3,14,14,14,3,3,3,9,9,9,4,4,4)
#'cards <- matrix(cards,nrow=3,ncol=14)
#'dotPair(3,cards,c(2,2,2))
#'@export
dotPair <- function(nPlayers,cards,score) {
  ranks <- seq(from = 1, to = 13, by = 2)	
  #the columns for rank
  temp <- cards[,ranks]	#create a copy of cards with ranks only
  temp[which(score!=2),] <- 0 	
  #when there is not a pair,
  #set the copy to zero.
  temp <- t(apply(temp,1,sort,decreasing=TRUE))
  #sorts the ranks
  pairRanks <- rep(0, nrow(temp))
  #create a vector for pair rank
  pairRanks[which(score==2)] <- apply(temp[which(score==2),],1,dotPairRanker)
  #determine the pairRank
  winner <- which(pairRanks == max(pairRanks))
  #the absolute position of players with the highest pair
  #
  #systematic scan for kicker cards
  temp[-winner,] <- 0
  #if not the highest pair, set to zero
  kickers <- matrix(0, nrow=nrow(temp), ncol=3)
  #create a copy of cards with kickers only.
  for (i in 1:nPlayers){
    if (temp[i,1]==0) kickers[i,1:3] = 0
    #kickers are zero if not highest pair
    else kickers[i,] <- subset(temp[i,], temp[i,] != pairRanks[i])[1:3]
    #otherwise, kickers are 3 highest nonpair cards
  }
  winner <- dotHighcardCompare(kickers)
  #determine the player with the top kicker
  winner
}


#'dotTwoPairRanker
#'
#'Determine the ranks of the two pairs.  Notes: dotTwoPairRanker requires a hand with a score of 3 (i.e., two pairs). This functions works best when ranks are sorted in decreasing order.
#'
#'@param oneHand : \tabular{ll}{a sorted hand with ranks only as
#'												vector[7] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: rank of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col7: rank of card 7}
#'@return pairRank : the rank of the pair as
#'												vector
#'@examples
#'dotTwoPairRanker(c(9,7,5,3,3,2,2))
#'dotTwoPairRanker(c(9,5,5,3,3,2,2))
#'@export
dotTwoPairRanker <- function(oneHand) {
  rankValues <- rle(oneHand)$values
  #the unique ranks
  rankLengths <- rle(oneHand)$lengths
  #the length of the unique ranks
  pairRank <- rankValues[rankLengths == 2]
  #the ranks of the pairs
  pairRank[1:2]
  #the rank of the top two pairs
}


#'dotTwoPairs
#'
#'Determine the player(s) with the highest two pairs and kicker card.
#'
#'@param nPlayers number of hands as		integer in \{2, ... , 9\}
#'@param cards : \tabular{ll}{the 7 card hand as
#'												matrix[nPlayers, 14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@param score : \tabular{ll}{the score of the hand in absolute terms as
#'												vector[nPlayers] \tab \cr \tab
#'												 9 = Straight Flush \cr \tab
#'												 8 = Four of a Kind \cr \tab
#'												 7 = Full House \cr \tab
#'												 6 = Flush \cr \tab
#'												 5 = Straight \cr \tab
#'												 4 = Three of a Kind \cr \tab
#'												 3 = Two Pair \cr \tab
#'												 2 = One Pair \cr \tab
#'												 1 = High Card}
#'@return winner : absolute position of the winner as
#'												vector
#'@seealso \code{\link{dotTwoPairRanker}} and \code{\link{dotHighcardCompare}}
#'@examples
#'cards <- c(2,3,4,5,1,1,1,1,2,3,6,7,2,2,2,2,4,4,4,4,3,3,3,3,11,11,11,11,3,3,3,3)
#'cards <- c(cards,13,13,13,13,3,3,3,3,14,14,14,14,3,3,3,3,14,14,14,14,4,4,4,4)
#'cards <- matrix(cards,nrow=4,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'dotTwoPairs(nPlayers,cards,score)
#'@export
dotTwoPairs <- function(nPlayers,cards,score) {
  ranks <- seq(from = 1, to = 13, by = 2)	
  #the columns for rank
  temp <- cards[,ranks]	#create a copy of cards with ranks only
  temp[which(score!=3),] <- 0 	
  #when there is not a set,
  #set the copy to zero.
  temp <- t(apply(temp,1,sort,decreasing=TRUE))
  #sorts the ranks
  pairRanks <- matrix(0, nPlayers,2)
  #create a vector for pair ranks
  pairRanks[which(score==3),] <- t(apply(temp[which(score==3),],1,dotTwoPairRanker))
  #determine the ranks of the pairs
  winner <- dotHighcardCompare(pairRanks)
  #determine which players have the highest two pair
  temp[-winner,] <- 0
  #if not the highest two pairs, set to zero
  kickers <- matrix(0, nrow=nrow(temp), ncol=1)
  #create a copy of cards with kickers only.
  for (i in 1:nPlayers){
    if (temp[i,1]==0) kickers[i,1] = 0
    #kickers are zero if not highest two pair
    else kickers[i,] <- subset(temp[i,],temp[i,] != pairRanks[i,1] & temp[i,] != pairRanks[i,2])[1]
    #otherwise, kicker is highest card not in the top and bottom pair
  }
  winner <- which(kickers == max(kickers))
  #determine which hand with top and bottom pair has the highest kicker
  winner
}


#'dotTripRanker
#'
#'Determine the rank of the three of a kind.  Note: dotTripRanker requires a hand with a score of 4 (i.e., three of a kind).
#'
#'@param oneHand : \tabular{ll}{a sorted hand with ranks only as
#'												vector[7] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: rank of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col7: rank of card 7}
#'@return tripRank : the rank of the pair as
#'												vector
#'@examples
#'dotTripRanker(c(9,7,5,3,3,3,2))
#'@export
dotTripRanker <- function(oneHand) {
  rankValues <- rle(oneHand)$values
  #the unique ranks
  rankLengths <- rle(oneHand)$lengths
  #the length of the unique ranks
  tripRank <- rankValues[rankLengths == 3]
  #the ranks of the sets
  tripRank[1]
  #the rank of the top set
}


#'dotTrips
#'
#'Determine the player(s) with the highest three of a kind and kicker cards.
#'
#'@param nPlayers number of hands as		integer in \{2, ... , 9\}
#'@param cards : \tabular{ll}{the 7 card hand as
#'												matrix[nPlayers, 14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@param score : \tabular{ll}{the score of the hand in absolute terms as
#'												vector[nPlayers] \tab \cr \tab
#'												 9 = Straight Flush \cr \tab
#'												 8 = Four of a Kind \cr \tab
#'												 7 = Full House \cr \tab
#'												 6 = Flush \cr \tab
#'												 5 = Straight \cr \tab
#'												 4 = Three of a Kind \cr \tab
#'												 3 = Two Pair \cr \tab
#'												 2 = One Pair \cr \tab
#'												 1 = High Card}
#'@return winner : absolute position of the winner as
#'												vector
#'@seealso \code{\link{dotTripRanker}} and \code{\link{dotHighcardCompare}}
#'@examples
#'cards <- c(14,14,4,5,1,2,1,1,10,9,6,7,2,2,2,2,4,4,4,4,3,3,3,3,8,8,8,8,3,3,3,3)
#'cards <- c(cards,13,13,13,13,3,3,3,3,14,14,14,14,3,3,3,3,14,14,14,14,4,4,4,4)
#'cards <- matrix(cards,nrow=4,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'dotTrips(nPlayers,cards,score)
#'
#'cards <- c(14,14,4,5,1,2,1,1,2,3,6,7,2,2,2,2,4,4,4,4,3,3,3,3,11,11,11,11,3,3,3,3)
#'cards <- c(cards,13,13,13,13,3,3,3,3,14,14,14,14,3,3,3,3,14,14,14,14,4,4,4,4)
#'cards <- matrix(cards,nrow=4,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'dotTrips(nPlayers,cards,score)
#'@export
dotTrips <- function(nPlayers,cards,score) {
  ranks <- seq(from = 1, to = 13, by = 2)	
  #the columns for rank
  temp <- cards[,ranks]	#create a copy of cards with ranks only
  temp[which(score!=4),] <- 0 	
  #when there is not a set,
  #set the copy to zero.
  temp <- t(apply(temp,1,sort,decreasing=TRUE))
  #sorts the ranks
  tripRanks <- matrix(0, nPlayers,1)
  #create a vector for trip rank
  tripRanks[which(score==4),] <- apply(temp[which(score==4),],1,dotTripRanker)
  #determine the rank of the set
  winner <- which(tripRanks == max(tripRanks))
  #the absolute position of players with the biggest set
  temp[-winner,] <- 0
  #if not the highest pair, set to zero
  kickers <- matrix(0, nrow=nrow(temp), ncol=2)
  #create a copy of cards with kickers only.
  for (i in 1:nPlayers){
    if (temp[i,1]==0) kickers[i,1:2] = 0
    #kickers are zero if not highest set
    else kickers[i,] <- subset(temp[i,], temp[i,] != tripRanks[i])[1:2]
    #otherwise, kickers are 2 highest unmatched cards
  }
  winner <- dotHighcardCompare(kickers)
  #determine the player with the top kicker
  winner
}


#'dotStraightRanker
#'
#'Returns the rank of the highest card in the straight.
#'
#'@param oneHand : \tabular{ll}{a sorted hand with ranks only as
#'												vector[7] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: rank of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col7: rank of card 7}
#'@return straightRank : the rank of top card in the straight as
#'												integer
#'@examples
#'dotStraightRanker(c(2,3,4,5,6,9,10))
#'dotStraightRanker(c(2,3,3,4,5,6,10))
#'dotStraightRanker(c(2,3,4,5,6,7,10))
#'@export
dotStraightRanker <- function(oneHand) {
  oneHand <- sort(oneHand)
  #sort the values from low to high
  rankValues <- rle(oneHand)$values
  #the unique ranks		
  k <- length(rankValues)
  #the number of unique cards
  kgt4 <- k - 4
  #create a variable for the number of sequences to check
  if (sum(rankValues[c(k,1,2,3,4)] == c(14,2,3,4,5)) == 5) straightRank <- 5
  #check for wrap around
  #aka a low straight or wheel
  for (i in 1:kgt4) {
    #check kgt4 number of sequences
    if (rankValues[i+4] == (rankValues[i]+4)) straightRank <- rankValues[i+4]
    #if the (i+4)th rank is the ith rank plus four 
    #the high card is the (i+4)th rank
  }
  straightRank
}


#'dotStraight
#'
#'Determine the player with the highest straight.
#'
#'@param cards : \tabular{ll}{the 7 card hand as
#'												matrix[nPlayers, 14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@param score : \tabular{ll}{the score of the hand in absolute terms as
#'												vector[nPlayers] \tab \cr \tab
#'												 9 = Straight Flush \cr \tab
#'												 8 = Four of a Kind \cr \tab
#'												 7 = Full House \cr \tab
#'												 6 = Flush \cr \tab
#'												 5 = Straight \cr \tab
#'												 4 = Three of a Kind \cr \tab
#'												 3 = Two Pair \cr \tab
#'												 2 = One Pair \cr \tab
#'												 1 = High Card}
#'@return winner : absolute position of the winner as
#'												vector
#'@seealso \code{\link{dotStraightRanker}}
#'@examples
#'cards <- c(7,1,4,2,4,1,4,3,10,1,11,2,2,2,2,3,3,3,3,3,3,1,1,1,5,5,5)
#'cards <- c(cards,4,4,4,6,6,6,2,2,2,14,14,14,2,2,2)
#'cards <- matrix(cards,nrow=3,ncol=14); cards
#'score <- showdown(cards); score
#'dotStraight(cards, score)
#'
#'cards <- c(2,1,4,2,4,1,4,3,10,1,11,2,2,2,2,3,3,3,3,3,3,1,1,1,5,5,5)
#'cards <- c(cards,4,4,4,6,6,6,2,2,2,14,14,14,2,2,2)
#'cards <- matrix(cards,nrow=3,ncol=14); cards
#'score <- showdown(cards); score
#'dotStraight(cards, score)
#'@export
dotStraight <- function(cards,score) {
  ranks <- seq(from = 1, to = 13, by = 2)	
  #the columns for rank
  temp <- cards[, ranks] 	#assume score = 1 for all players
  #create a copy of cards with ranks only
  temp <- t(apply(temp,1,sort,decreasing=FALSE))
  #sorts the ranks
  temp[-which(score==5),] <- 0
  #if not a straight, set temp to zero. SLIGHTLY UNNECESSARY, BUT GOOD TO KEEP IN
  
  straightRanks <- matrix(0, nrow=nrow(temp), ncol=1)
  #create a variable for the high card in the straight
  straightRanks[which(score==5),] <- apply(temp[which(score==5),],1,dotStraightRanker)
  #determine the high card in each straight
  winner <- which(straightRanks == max(straightRanks))
  #determine the player with the top straight
  winner
}	


#'dotFlushRanker
#'
#'Return the ranks of the 5 highest cards in the flush.
#'
#'@param cardsRow : \tabular{ll}{one 7 card hand as
#'												vector[14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@return \tabular{ll}{flushRank : the rank of 5 high cards in flush as
#'												vector[5] \tab \cr \tab
#'												col1: suit of card 1 in \{2, ... , 14\} \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col5: suit of card n in \{2, ... , 14\}}
#'@examples
#'dotFlushRanker(c(2,1,2,2,5,2,7,2,8,2,9,2,11,1))
#'dotFlushRanker(c(2,1,2,2,5,2,7,2,8,2,9,2,11,2))
#'@export								
dotFlushRanker <- function(cardsRow){
  suits <- seq(from = 2, to = 14, by = 2)
  #the columns of suit
  sortedSuits <- sort(cardsRow[suits])
  #sort the suits from low to high
  suitValues <- rle(sortedSuits)$values
  #the unique suits
  suitLengths <- rle(sortedSuits)$lengths
  #the length of the unique suits
  flushSuit <- suitValues[suitLengths >= 5]
  #the flush suit
  flushIndex <- which(cardsRow[suits] == flushSuit)
  #the index of cards in flush
  flushRank <- cardsRow[2*flushIndex - 1]
  #rank of each card in flush
  flushRank <- sort(flushRank,decreasing=TRUE)[1:5]
  #5 high cards in flush
  flushRank
}


#'dotFlush
#'
#'Determine the player with the highest flush.
#'
#'@param cards : \tabular{ll}{the 7 card hand as
#'												matrix[nPlayers, 14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@param score : \tabular{ll}{the score of the hand in absolute terms as
#'												vector[nPlayers] \tab \cr \tab
#'												 9 = Straight Flush \cr \tab
#'												 8 = Four of a Kind \cr \tab
#'												 7 = Full House \cr \tab
#'												 6 = Flush \cr \tab
#'												 5 = Straight \cr \tab
#'												 4 = Three of a Kind \cr \tab
#'												 3 = Two Pair \cr \tab
#'												 2 = One Pair \cr \tab
#'												 1 = High Card}
#'@return winner : absolute position of the winner as
#'												vector
#'@seealso \code{\link{dotFlushRanker}} and \code{\link{dotHighcardCompare}}
#'@examples
#'cards <- c(2,1,3,3,5,2,6,3,7,3,13,3,14,3,2,3,3,4,5,1,6,3,7,3,13,3,14,3)
#'cards <- matrix(cards,2,14,byrow=TRUE); cards
#'score <- showdown(cards); score
#'dotFlush(cards,score)
#'
#'cards <- c(2,1,3,3,5,3,6,3,7,3,13,3,14,3,2,3,3,4,5,3,6,3,7,3,13,3,14,3)
#'cards <- matrix(cards,2,14,byrow=TRUE);cards
#'score <- showdown(cards); score 
#'dotFlush(cards,score)
#'@export
dotFlush <- function(cards,score) {
  flushRanks <- matrix(0, nrow=nrow(cards), ncol=5)
  #create a vector for each rank in flush
  flushRanks[which(score==6),] <- t(apply(cards[which(score==6),],1,dotFlushRanker))
  #if there is a flush,
  #fill in top 5 ranks of the same suit
  winner <- dotHighcardCompare(flushRanks)
  #determine the player with the top flush
  winner
}


#'dotFullHouseRanker
#'
#'Determine the rank of the top set and the top pair.
#'
#'@param oneHand : \tabular{ll}{the ranks of one 7 card hand as
#'												vector[7] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: rank of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col7: rank of card 7}
#'@return \tabular{ll}{fullHouseRank : the ranks of the high set and the high pair as
#'												vector \tab \cr \tab
#'												col1: the rank of the top set \cr \tab
#'												col2: the rank of the top pair}
#'@examples
#'dotFullHouseRanker(c(2,2,2,5,5,8,9))
#'dotFullHouseRanker(c(2,2,5,5,5,8,9))
#'dotFullHouseRanker(c(2,2,5,5,5,8,8))
#'@export
dotFullHouseRanker <- function(oneHand) {
  #rle is the equivalent of the Unix uniq -c command.
  values <- rle(oneHand)$values
  #compute the values of equal ranks
  lengths <- rle(oneHand)$length
  #compute the lengths of equal ranks 
  #A full Boat could have 2 sets, 1 set and 2 pairs, or 
  #1 set and 1 pair.
  fullHouseRank <- rep(0,2)
  #create a vector for pair ranks
  if (sum(lengths == 3) == 2) {
    #if there are two sets
    #choose the top set as the set
    #choose the bottom set as the pair
    fullHouseRank[1] <- max(values[lengths == 3])
    fullHouseRank[2] <- min(values[lengths == 3])
  }
  else if (sum(lengths == 2) == 2) {
    #if there are two pairs
    #choose the set as the set
    #choose the top pair as the pair
    fullHouseRank[1] <- values[lengths == 3]
    fullHouseRank[2] <- max(values[lengths == 2])
  }
  else if (sum(lengths == 2) == 1) {
    #if there is one pair
    #choose the set as the set
    #choose the pair as the pair
    fullHouseRank[1] <- values[lengths == 3]
    fullHouseRank[2] <- values[lengths == 2]
  }
  fullHouseRank
}


#'dotFullHouse
#'
#'Determine the player with the highest boat.
#'
#'@param cards : \tabular{ll}{the 7 card hand as
#'												matrix[nPlayers, 14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@param score : \tabular{ll}{the score of the hand in absolute terms as
#'												vector[nPlayers] \tab \cr \tab
#'												 9 = Straight Flush \cr \tab
#'												 8 = Four of a Kind \cr \tab
#'												 7 = Full House \cr \tab
#'												 6 = Flush \cr \tab
#'												 5 = Straight \cr \tab
#'												 4 = Three of a Kind \cr \tab
#'												 3 = Two Pair \cr \tab
#'												 2 = One Pair \cr \tab
#'												 1 = High Card}
#'@return winner : absolute position of the winner as
#'												vector
#'@seealso \code{\link{dotFullHouseRanker}}
#'@examples
#'cards <- c(5,10,4,8,1,2,1,1,10,9,6,7,3,2,2,2,5,5,5,5,3,3,3,3,8,8,8,8,3,3,3,3)
#'cards <- c(cards,14,14,14,14,2,2,2,2,14,14,14,14,3,3,3,3,14,14,14,14,4,4,4,4)
#'cards <- matrix(cards,nrow=4,ncol=14); cards
#'score <- showdown(cards); score
#'dotFullHouse(cards,score)
#'
#'cards <- c(5,10,4,8,1,2,1,1,10,9,6,7,3,2,2,2,12,12,12,12,1,1,1,1,12,12,12,12,3,3,3,3)
#'cards <- c(cards,14,14,14,14,2,2,2,2,14,14,14,14,3,3,3,3,14,14,14,14,4,4,4,4)
#'cards <- matrix(cards,nrow=4,ncol=14); cards
#'score <- showdown(cards); score
#'dotFullHouse(cards,score)
#'@export
dotFullHouse <- function(cards,score) {
  ranks <- seq(from = 1, to = 13, by = 2)	
  #the columns for rank
  temp <- cards[,ranks]	#create a copy of cards with ranks only
  temp[-which(score==7),] <- 0
  #when there is not a boat,
  #set the copy to zero.
  #SLIGHTLY UNNECESSARY
  temp <- t(apply(temp,1,sort,decreasing=TRUE))
  #sorts the ranks
  fullHouseRanks <- matrix(0,nrow=nrow(cards),ncol=2)
  #create a variable for the rank of the high set and the high pair
  fullHouseRanks[-which(score==7),] <- 0
  #if there is not a full house, then set to zero
  fullHouseRanks[which(score==7),] <- t(apply(temp[which(score==7),],1,dotFullHouseRanker))
  #determine the rank of the high set and the high pair
  winner <- which(fullHouseRanks[,1] == max(fullHouseRanks[,1]))
  #determine the players with the high set
  fullHouseRanks[-winner,] <- 0
  #if player does not have the high set, zero out their full house
  winner <- which(fullHouseRanks[,2] == max(fullHouseRanks[,2]))
  #determine which player with the high set has the high pair
  winner					
}


#'dotFourOfAKindRanker
#'
#'Determine the rank of the four of a kind and the kicker. This functions assumes ranks are sorted in decreasing order.
#'
#'@param oneHand : \tabular{ll}{the ranks of one 7 card hand as
#'												vector[7] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: rank of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col7: rank of card 7}
#'@return \tabular{ll}{fourOfAKindRank : the ranks of the quads and the high kicker as
#'												vector \tab \cr \tab
#'												col1: the rank of the quads \cr \tab
#'												col2: the rank of the kicker}
#'@examples
#'dotFourOfAKindRanker(c(14,14,14,14,10,7,6))
#'dotFourOfAKindRanker(sort(c(10,14,6,14,7,14,14),decreasing=TRUE))
#'@export
dotFourOfAKindRanker <- function(oneHand) {
  #rle is the equivalent of the Unix uniq -c command.
  values <- rle(oneHand)$values
  #compute the values of equal ranks
  lengths <- rle(oneHand)$length
  #compute the lengths of equal ranks 
  #A full Boat could have 2 sets, 1 set and 2 pairs, or 
  #1 set and 1 pair.
  fourOfAKindRank <- rep(0,2)
  #create a vector for pair ranks
  #choose the four of a kind as the quad
  #choose the high "other" as the kicker
  fourOfAKindRank[1] <- values[lengths == 4]
  fourOfAKindRank[2] <- max(values[lengths != 4])
  fourOfAKindRank
}


#'dotFourOfAKind
#'
#'Determine the player with the highest hand (i.e., four of a kind and kicker) with score of 8.
#'
#'@param nPlayers : number of hands to deal as 
#'												integer in \{2, ... , 9\}
#'@param cards : \tabular{ll}{the 7 card hand as
#'												matrix[nPlayers, 14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@param score : \tabular{ll}{the score of the hand in absolute terms as
#'												vector[nPlayers] \tab \cr \tab
#'												 9 = Straight Flush \cr \tab
#'												 8 = Four of a Kind \cr \tab
#'												 7 = Full House \cr \tab
#'												 6 = Flush \cr \tab
#'												 5 = Straight \cr \tab
#'												 4 = Three of a Kind \cr \tab
#'												 3 = Two Pair \cr \tab
#'												 2 = One Pair \cr \tab
#'												 1 = High Card}
#'@return winner : absolute position of the winner as
#'												vector
#'@seealso \code{\link{dotFourOfAKindRanker}}
#'@examples
#'cards <- c(14,10,5,1,2,1,14,9,7,2,2,2,4,4,4,3,3,3,8,8,8,3,3,3,13,13,13)
#'cards <- c(cards,3,3,3,14,14,14,3,3,3,14,14,14,4,4,4)
#'cards <- matrix(cards,nrow=3,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'dotFourOfAKind(nPlayers,cards,score)
#'
#'cards <- c(3,4,5,1,1,1,8,9,10,1,1,1,14,14,14,1,1,1,14,14,14,2,2,2,7,7,7)
#'cards <- c(cards,3,3,3,14,14,14,3,3,3,14,14,14,4,4,4)
#'cards <- matrix(cards,nrow=3,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'dotFourOfAKind(nPlayers,cards,score)
#'
#'cards <- c(3,4,5,1,1,1,8,9,10,1,1,1,14,14,14,1,1,1,14,14,14,2,2,2,11,11,11)
#'cards <- c(cards,3,3,3,14,14,14,3,3,3,14,14,14,4,4,4)
#'cards <- matrix(cards,nrow=3,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'dotFourOfAKind(nPlayers,cards,score)
#'@export
dotFourOfAKind <- function(nPlayers,cards,score) {
  ranks <- seq(from = 1, to = 13, by = 2)	
  #the columns for rank
  temp <- cards[,ranks]	#create a copy of cards with ranks only
  temp <- t(apply(temp,1,sort,decreasing=TRUE))
  #sorts the ranks
  temp[-which(score==8),] <- 0
  #when there is not a boat,
  #set the copy to zero.
  fourOfAKindRanks <- matrix(0,nrow=nPlayers,ncol=2)
  #create a variable for the rank of the quads and the kicker
  if (sum(which(score==8))==1) {
    fourOfAKindRanks[which(score==8),] <- dotFourOfAKindRanker(temp[which(score==8),])
    #determine the rank of the four of a kind and the kicker
  } else {
    fourOfAKindRanks[which(score==8),] <- t(apply(temp[which(score==8),],1,dotFourOfAKindRanker))
    #determine the rank of the four of a kind and the kicker
  }
  winner <- which(fourOfAKindRanks[,1] == max(fourOfAKindRanks[,1]))
  #determine the players with the high four of a kind
  fourOfAKindRanks[-winner,] <- 0
  #if player does not have the high four of a kind, zero out their quads
  winner <- which(fourOfAKindRanks[,2] == max(fourOfAKindRanks[,2]))
  #determine which player with the high four of a kind has the high kicker
  winner					
}


#'dotStraightFlushRanker
#'
#'Determine the rank of the highest card in a straight flush. This function assumes cards are sorted in ascending order.
#'
#'@param yTempRow : \tabular{ll}{a sorted 7 card hand of numbers as
#'												vector[7] \tab \cr \tab
#'												col1: number of card 1 in \{1, 2, ... , 52\} \cr \tab
#'												col2: number of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col7: number of card 7}
#'@return straightFlushRank : the top card in the straight flush as
#'												integer
#'@examples
#'dotStraightFlushRanker(c(1,2,3,4,5,15,19))
#'dotStraightFlushRanker(c(9,10,11,12,13,35,42))
#'dotStraightFlushRanker(c(9,10,11,12,13,14,35))
#'dotStraightFlushRanker(c(1,2,3,4,13,20,35))
#'dotStraightFlushRanker(c(9,26,14,15,16,17,35))
#'@export
dotStraightFlushRanker <- function(yTempRow) {
  #there is a straight flush
  for (i in 0:3){
    #check each suit for a flush and a wheel
    if (sum(yTempRow %in% c(13+13*i,1+13*i,2+13*i,3+13*i,4+13*i)) == 5) straightFlushRank <- 5
    #check each suit for a flush and a straight
    if (sum(yTempRow %in% c(1+13*i,2+13*i,3+13*i,4+13*i,5+13*i)) == 5) straightFlushRank <- 6
    if (sum(yTempRow %in% c(2+13*i,3+13*i,4+13*i,5+13*i,6+13*i)) == 5) straightFlushRank <- 7
    if (sum(yTempRow %in% c(3+13*i,4+13*i,5+13*i,6+13*i,7+13*i)) == 5) straightFlushRank <- 8
    if (sum(yTempRow %in% c(4+13*i,5+13*i,6+13*i,7+13*i,8+13*i)) == 5) straightFlushRank <- 9
    if (sum(yTempRow %in% c(5+13*i,6+13*i,7+13*i,8+13*i,9+13*i)) == 5) straightFlushRank <- 10
    if (sum(yTempRow %in% c(6+13*i,7+13*i,8+13*i,9+13*i,10+13*i)) == 5) straightFlushRank <- 11
    if (sum(yTempRow %in% c(7+13*i,8+13*i,9+13*i,10+13*i,11+13*i)) == 5) straightFlushRank <- 12
    if (sum(yTempRow %in% c(8+13*i,9+13*i,10+13*i,11+13*i,12+13*i)) == 5) straightFlushRank <- 13
    if (sum(yTempRow %in% c(9+13*i,10+13*i,11+13*i,12+13*i,13+13*i)) == 5) straightFlushRank <- 14
  }
  straightFlushRank
}


#'dotStraightFlush
#'
#'Determine the player with the highest straight flush.
#'
#'@param nPlayers number of hands as			integer in \{2, ... , 9\}
#'@param cards : \tabular{ll}{the 7 card hand as
#'												matrix[nPlayers, 14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@param score : \tabular{ll}{the score of the hand in absolute terms as
#'												vector[nPlayers] \tab \cr \tab
#'												 9 = Straight Flush \cr \tab
#'												 8 = Four of a Kind \cr \tab
#'												 7 = Full House \cr \tab
#'												 6 = Flush \cr \tab
#'												 5 = Straight \cr \tab
#'												 4 = Three of a Kind \cr \tab
#'												 3 = Two Pair \cr \tab
#'												 2 = One Pair \cr \tab
#'												 1 = High Card}
#'@return winner : absolute position of the winner as
#'												vector
#'@seealso \code{\link{dotTransformToNumber}} and \code{\link{dotStraightFlushRanker}}
#'@examples
#'cards <- c(8,13,5,1,1,4,6,2,2,2,3,4,14,14,14,2,2,2,9,9,9,1,1,1,10,10,10)
#'cards <- c(cards,1,1,1,11,11,11,1,1,1,12,12,12,1,1,1)
#'cards <- matrix(cards,nrow=3,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'dotStraightFlush(nPlayers,cards,score)
#'
#'cards <- c(1,1,3,4,2,2,3,4,8,8,1,1,9,9,1,1,10,10,1,1,11,11,1,1,12,12,1,1)
#'cards <- matrix(cards,nrow=2,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'dotStraightFlush(nPlayers,cards,score)
#'@export
dotStraightFlush <- function(nPlayers, cards, score) {
  ranks <- seq(from = 1, to = 13, by = 2)
  #the columns of rank
  suits <- seq(from = 2, to = 14, by = 2)
  #the columns of suit
  yTemp <- matrix(0,nrow=nPlayers,ncol=7)
  #create a copy of the 7 card hand transformed to number
  for (i in 1:nPlayers) {
    yTemp[i,] <- dotTransformToNumber(cards[i,ranks],cards[i,suits])
    #transform cards to number
  }
  yTemp[-which(score==9),] <- 0
  #if not a straight flush, set to zero
  yTemp <- t(apply(yTemp,1,sort))	
  #sort the numbers
  straightFlushRanks <- matrix(0, nrow=nPlayers, ncol=1)
  #create a variable for the top card in the straight flush
  straightFlushRanks[which(score==9),] <- t(apply(yTemp[which(score==9),],1,dotStraightFlushRanker))
  #determine the top card in each straight flush
  winner <- which(straightFlushRanks == max(straightFlushRanks))
  #determine the player with the top straight flush
  winner
}


#'tiebreaker
#'
#'Determine the winner in the presence of any ties.
#'
#'@param nPlayers number of hands as			integer in \{2, ... , 9\}
#'@param cards : \tabular{ll}{the 7 card hand as
#'												matrix[nPlayers, 14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@param score : \tabular{ll}{the score of the hand in absolute terms as
#'												vector[nPlayers] \tab \cr \tab
#'												 9 = Straight Flush \cr \tab
#'												 8 = Four of a Kind \cr \tab
#'												 7 = Full House \cr \tab
#'												 6 = Flush \cr \tab
#'												 5 = Straight \cr \tab
#'												 4 = Three of a Kind \cr \tab
#'												 3 = Two Pair \cr \tab
#'												 2 = One Pair \cr \tab
#'												 1 = High Card}
#'@return winner : the absolute position of the winner(s) as
#'												vector
#'@seealso \code{\link{dotHighcard}}, \code{\link{dotPair}}, \code{\link{dotTwoPairs}}, \code{\link{dotTrips}}, \code{\link{dotStraight}}, \code{\link{dotFlush}}, \code{\link{dotFullHouse}}, \code{\link{dotFourOfAKind}} and \code{\link{dotStraightFlush}}
#'@examples
#'cards <- c(2,1,4,2,5,3,6,4,7,1,13,2,14,3,2,3,3,4,5,1,6,2,7,3,13,4,14,1)
#'cards <- matrix(cards,2,14,byrow=TRUE); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(2,1,3,2,5,3,6,4,7,1,13,2,14,3,2,3,3,4,5,1,6,2,7,3,13,4,14,1)
#'cards <- matrix(cards,2,14,byrow=TRUE); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(2,3,4,5,1,1,1,1,2,3,6,7,2,2,2,2,4,4,4,4,3,3,3,3,11,11,11,11,3,3,3,3)
#'cards <- c(cards,13,13,13,13,3,3,3,3,14,14,14,14,3,3,3,3,9,9,9,9,4,4,4,4)
#'cards <- matrix(cards,nrow=4,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(2,3,4,5,1,1,1,1,2,3,6,7,2,2,2,2,4,4,4,4,3,3,3,3,11,11,11,11,3,3,3,3)
#'cards <- c(cards,13,13,13,13,3,3,3,3,14,14,14,14,3,3,3,3,14,14,14,14,4,4,4,4)
#'cards <- matrix(cards,nrow=4,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(14,14,4,5,1,2,1,1,10,9,6,7,2,2,2,2,4,4,4,4,3,3,3,3,8,8,8,8,3,3,3,3)
#'cards <-c(cards,13,13,13,13,3,3,3,3,14,14,14,14,3,3,3,3,14,14,14,14,4,4,4,4)
#'cards <- matrix(cards,nrow=4,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(14,14,4,5,1,2,1,1,2,3,6,7,2,2,2,2,4,4,4,4,3,3,3,3,11,11,11,11,3,3,3,3)
#'cards <-c(cards,13,13,13,13,3,3,3,3,14,14,14,14,3,3,3,3,14,14,14,14,4,4,4,4)
#'cards <- matrix(cards,nrow=4,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(7,1,4,2,4,1,4,3,10,1,11,2,2,2,2,3,3,3,3,3,3,1,1,1,5,5,5,4,4,4,6,6,6)
#'cards <-c(cards,2,2,2,14,14,14,2,2,2)
#'cards <- matrix(cards,nrow=3,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(2,1,4,2,4,1,4,3,10,1,11,2,2,2,2,3,3,3,3,3,3,1,1,1,5,5,5,4,4,4,6,6,6)
#'cards <-c(cards,2,2,2,14,14,14,2,2,2)
#'cards <- matrix(cards,nrow=3,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(2,1,3,3,5,2,6,3,7,3,13,3,14,3,2,3,3,4,5,1,6,3,7,3,13,3,14,3)
#'cards <- matrix(cards,2,14,byrow=TRUE); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(2,1,3,3,5,3,6,3,7,3,13,3,14,3,2,3,3,4,5,3,6,3,7,3,13,3,14,3)
#'cards <- matrix(cards,2,14,byrow=TRUE); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(5,10,4,8,1,2,1,1,10,9,6,7,3,2,2,2,5,5,5,5,3,3,3,3,8,8,8,8,3,3,3,3)
#'cards <-c(cards,14,14,14,14,2,2,2,2,14,14,14,14,3,3,3,3,14,14,14,14,4,4,4,4)
#'cards <- matrix(cards,nrow=4,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(5,10,4,8,1,2,1,1,10,9,6,7,3,2,2,2,12,12,12,12,1,1,1,1,12,12,12,12)
#'cards <-c(cards,3,3,3,3,14,14,14,14,2,2,2,2,14,14,14,14,3,3,3,3,14,14,14,14,4,4,4,4)
#'cards <- matrix(cards,nrow=4,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(14,10,5,1,2,1,14,9,7,2,2,2,4,4,4,3,3,3,8,8,8,3,3,3,13,13,13)
#'cards <-c(cards,3,3,3,14,14,14,3,3,3,14,14,14,4,4,4)
#'cards <- matrix(cards,nrow=3,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(3,4,5,1,1,1,8,9,10,1,1,1,14,14,14,1,1,1,14,14,14,2,2,2,11,11,11)
#'cards <-c(cards,3,3,3,14,14,14,3,3,3,14,14,14,4,4,4)
#'cards <- matrix(cards,nrow=3,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(8,13,5,1,1,4,6,2,2,2,3,4,14,14,14,2,2,2,9,9,9,1,1,1,10,10,10)
#'cards <-c(cards,1,1,1,11,11,11,1,1,1,12,12,12,1,1,1)
#'cards <- matrix(cards,nrow=3,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'
#'cards <- c(1,1,3,4,2,2,3,4,8,8,1,1,9,9,1,1,10,10,1,1,11,11,1,1,12,12,1,1)
#'cards <- matrix(cards,nrow=2,ncol=14); cards
#'score <- showdown(cards); score
#'nPlayers <- nrow(cards); nPlayers
#'tiebreaker(nPlayers,cards,score)
#'@export
tiebreaker <- function(nPlayers,cards,score) {
  winner <- which(score==max(score))
  if (length(winner) > 1) {
    #if length(max(scores)) >= 2
    #there is a tie
    if (max(score)==1) winner <- dotHighcard(cards)
    if (max(score)==2) winner <- dotPair(nPlayers,cards,score)
    if (max(score)==3) winner <- dotTwoPairs(nPlayers,cards,score)
    if (max(score)==4) winner <- dotTrips(nPlayers,cards,score)
    if (max(score)==5) winner <- dotStraight(cards,score)
    if (max(score)==6) winner <- dotFlush(cards,score)
    if (max(score)==7) winner <- dotFullHouse(cards,score)
    if (max(score)==8) winner <- dotFourOfAKind(nPlayers,cards,score)
    if (max(score)==9) winner <- dotStraightFlush(nPlayers,cards,score)
  }
  winner
}


#'cgiPlayers
#'
#'A primitive method (i.e., does not support classes) for graphics using the plot() function. Built-in support for 2-9 players.  This function was written on a Mac and may not be PC-compatible (yet). You must have already called cgiPlayers(time=1, ...) before calling cgiPlayers(time=2, ...), you must have already called cgiPlayers(time=1, ...) and cgiPlayers(time=2, ...) before calling cgiPlayers(time=3, ...), etc. 
#'
#'@param time : \tabular{ll}{the current round as	integer in \{1, 2, 3, 4\} \tab \cr \tab
#'												1 = pre-flop \cr \tab
#'												2 = flop \cr \tab
#'												3 = turn \cr \tab
#'												4 = river}
#'@param alias names of players	as				vector[nPlayers]
#'@param position dealer position as 			integer in \{2, ..., nPlayers\}
#'@param cards : \tabular{ll}{the 7 card hand as
#'												matrix[nPlayers, 14] \tab \cr \tab
#'												col1: rank of card 1 in \{2, ... , 14\} \cr \tab
#'												col2: suit of card 1 in \{1, 2, 3, 4\} \cr \tab
#'												col3: rank of card 2 \cr \tab
#'												col4: suit of card 2 \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												. \cr \tab
#'												col13: rank of card 7 \cr \tab
#'												col14: suit of card 7}
#'@return In lieu of a return value, cgiPlayers calls the plot() function.
#'@examples
#'alias <- c("Player1","Player2","Player3","Player4","Player5")
#'alias <- c(alias,"Player6","Player7","Player8","Player9")
#'cols1thru5 <- c(2,8,12,14,10,6,14,8,4,2,3,2,4,1,4,3,1,1,13,4,4,5,3,9,8,12,7)
#'cols1thru5 <- c(cols1thru5,3,4,3,2,2,4,2,1,1,3,3,3,3,3,3,3,3,3)
#'cols6thru10 <- c(1,1,1,1,1,1,1,1,1,10,10,10,10,10,10,10,10,10,4,4,4,4,4,4,4,4,4)
#'cols6thru10 <- c(cols6thru10,12,12,12,12,12,12,12,12,12,4,4,4,4,4,4,4,4,4)
#'cols11thru14 <- c(11,11,11,11,11,11,11,11,11,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
#'cols11thru14 <- c(cols11thru14,3,3,3,3,3,3,3,3,3)
#'cards <- matrix(c(cols1thru5,cols6thru10,cols11thru14),nrow=9,ncol=14); cards
#'cgiPlayers(1,alias,9,cards)
#'cgiPlayers(2,alias,9,cards)
#'cgiPlayers(3,alias,9,cards)
#'cgiPlayers(4,alias,9,cards)
#'@export
cgiPlayers <- function(time, alias, position, cards) {
  nPlayers <- nrow(cards)
  width <- 30
  height <- 20
  X1 <- numeric(nPlayers)
  Y1 <- numeric(nPlayers)
  X2 <- numeric(nPlayers)
  Y2 <- numeric(nPlayers)
  X3 <- numeric(5)
  Y3 <- height-5.5
  #PRE-FLOP
  if (time == 1) {
    plot(0,0, xlim=c(0,width), ylim=c(0,height), axes=FALSE, xlab="", ylab="", cex= 0, main="Poker Simulator")
    text(width/2,Y3+.7, "Community")
    X4 <- c(width/2-7.5,width/2+1.5,width/2+1.5,width/2-7.5,width/2-7.5,width/2+1.5,width/2+4.5,width/2+4.5,
            width/2+1.5,width/2+1.5,width/2+4.5,width/2+7.5,width/2+7.5,width/2+4.5)
    Y4 <- c(Y3+.35,Y3+.35,Y3-.35,Y3-.35,Y3+.35,Y3+.35,Y3+.35,Y3-.35,Y3-.35,Y3+.35,Y3+.35,Y3+.35,Y3-.35,Y3-.35)
    points(X4,Y4,"l", col="green")
    if (nPlayers==2) {
      X1 <- seq(from = 10,to = width-10, length = 2)
      Y1 <- rep(3.5,2)
    }
    if (nPlayers==3) {
      X1 <- seq(from = 5,to = width-5, length = 3)
      Y1 <- rep(3.5,3)
    }
    if (nPlayers==4) {
      X1 <- c(5,seq(from = 10,to = width-10, length = 2),width-5)
      Y1 <- c(6,rep(3.5,2),6)
    }
    if (nPlayers==5) {
      X1 <- c(5,seq(from = 5,to = width-5, length = 3),width-5)
      Y1 <- c(6,rep(3.5,3),6)
    }
    if (nPlayers==6) {
      X1 <- c(rep(5,2),seq(from = 10,to = width-10, length = 2),rep(width-5,2))
      Y1 <- c(8.5,6,rep(3.5,2),6,8.5)
    }
    if (nPlayers==7) {
      X1 <- c(rep(5,2),seq(from = 5,to = width-5, length = 3),rep(width-5,2))
      Y1 <- c(8.5,6,rep(3.5,3),6,8.5)
    }
    if (nPlayers==8) {
      X1 <- c(rep(5,3),seq(from = 10,to = width-10, length = 2),rep(width-5,3))
      Y1 <- c(11,8.5,6,rep(3.5,2),6,8.5,11)
    }
    if (nPlayers==9) {
      X1 <- c(rep(5,3),seq(from = 5,to = width-5, length = 3),rep(width-5,3))
      Y1 <- c(11,8.5,6,rep(3.5,3),6,8.5,11)
    }
    for (i in 1:nPlayers) 
    {
      text(X1[i], Y1[i], alias[i])
      for (j in 1:2) {			
        #RANK
        X2 <- X1[i]-2+3*(j-1)
        Y2[i] <- Y1[i] - .5
        if(cards[i,2*j-1]<=10) {
          text(X2,Y2[i], cards[i,2*j-1])
        }
        if(cards[i,2*j-1]==11) {
          text(X2,Y2[i], "J")
        }
        if(cards[i,2*j-1]==12) {
          text(X2,Y2[i], "Q")
        }
        if(cards[i,2*j-1]==13) {
          text(X2,Y2[i], "K")
        }
        if(cards[i,2*j-1]==14) {
          text(X2,Y2[i], "A")
        }			
        #SUIT
        if(cards[i,2*j]==1) {
          text(X2+1,Y2[i], "\u2660") #spades
        }
        if(cards[i,2*j]==2) {
          text(X2+1,Y2[i],"\u2663") #clubs
        }
        if(cards[i,2*j]==3) {
          text(X2+1,Y2[i], "\u2665") #hearts
        }
        if(cards[i,2*j]==4) {
          text(X2+1,Y2[i], "\u2666") #diamonds
        }	
      }
    }
  }		
  #FLOP
  if (time == 2 ) {
    X3 <- seq(from = width/2-6.5, by = 3, length = 3)
    for (i in 1:3) {
      if(cards[1,3+2*i]<=10) {
        text(X3[i],Y3, cards[1,3+2*i])
      }
      if(cards[1,3+2*i]==11) {
        text(X3[i],Y3, "J")
      }
      if(cards[1,3+2*i]==12) {
        text(X3[i],Y3, "Q")
      }
      if(cards[1,3+2*i]==13) {
        text(X3[i],Y3, "K")
      }
      if(cards[1,3+2*i]==14) {
        text(X3[i],Y3, "A")
      }			
      #SUIT
      if(cards[1,4+2*i]==1) {
        text(X3[i]+1,Y3,"\u2660") #spades
      }
      if(cards[1,4+2*i]==2) {
        text(X3[i]+1,Y3,"\u2663") #clubs
      }
      if(cards[1,4+2*i]==3) {
        text(X3[i]+1,Y3, "\u2665") #hearts
      }
      if(cards[1,4+2*i]==4) {
        text(X3[i]+1,Y3, "\u2666") #diamonds
      }
    }
  }
  #TURN
  if (time == 3 ) {
    X3 <- seq(from = width/2-6.5, by = 3, length = 5)
    i <- 4	
    if(cards[1,3+2*i]<=10) {
      text(X3[i],Y3, cards[1,3+2*i])
    }
    if(cards[1,3+2*i]==11) {
      text(X3[i],Y3, "J")
    }
    if(cards[1,3+2*i]==12) {
      text(X3[i],Y3, "Q")
    }
    if(cards[1,3+2*i]==13) {
      text(X3[i],Y3, "K")
    }
    if(cards[1,3+2*i]==14) {
      text(X3[i],Y3, "A")
    }			
    #SUIT
    if(cards[1,4+2*i]==1) {
      text(X3[i]+1,Y3,"\u2660") #spades
    }
    if(cards[1,4+2*i]==2) {
      text(X3[i]+1,Y3,"\u2663") #clubs
    }
    if(cards[1,4+2*i]==3) {
      text(X3[i]+1,Y3, "\u2665") #hearts
    }
    if(cards[1,4+2*i]==4) {
      text(X3[i]+1,Y3, "\u2666") #diamonds
    }
  }
  #RIVER	
  if (time == 4 ) {
    X3 <- seq(from = width/2-6.5, by = 3, length = 5)
    i <- 5	
    if(cards[1,3+2*i]<=10) {
      text(X3[i],Y3, cards[1,3+2*i])
    }
    if(cards[1,3+2*i]==11) {
      text(X3[i],Y3, "J")
    }
    if(cards[1,3+2*i]==12) {
      text(X3[i],Y3, "Q")
    }
    if(cards[1,3+2*i]==13) {
      text(X3[i],Y3, "K")
    }
    if(cards[1,3+2*i]==14) {
      text(X3[i],Y3, "A")
    }			
    #SUIT
    if(cards[1,4+2*i]==1) {
      text(X3[i]+1,Y3,"\u2660") #spades
    }
    if(cards[1,4+2*i]==2) {
      text(X3[i]+1,Y3,"\u2663") #clubs
    }
    if(cards[1,4+2*i]==3) {
      text(X3[i]+1,Y3, "\u2665") #hearts
    }
    if(cards[1,4+2*i]==4) {
      text(X3[i]+1,Y3, "\u2666") #diamonds
    }
  }
}