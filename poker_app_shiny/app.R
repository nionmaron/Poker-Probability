# Carregar os pacotes necessários
library(shiny)
library(shinymanager)
library(shinydashboard)
source("Function_Poker.R")
library(poker)
library(shinyWidgets)
library(DT)


Score<-1:9
my_score<-c("","","","","","","","","")
score_dict_en <- c("High Card", "One Pair", "Two Pair", "Three of a Kind", "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush")
score_dict_pt <- c("Carta Alta", "Um Par", "Dois Pares", "Trinca", "Sequência", "Flush", "Full House", "Quadra", "Sequência de Mesmo Naipe")
Prob_Simulation<- c(50.1177,42.2569,4.7539,2.1128,0.3925,0.1965,0.1441,0.0240,0.00139)

# Vetor com os nomes das cartas
cards <- c(paste0(rep(2:10, each = 4), c("H","S","C","D")), 
           paste0(rep(c('J', 'Q', 'K', 'A'), each = 4), c("H","S","C","D")))

# Separe e ordene as cartas por naipe
# cards <- sort(cards)
cards_by_suit <- split(cards, substr(cards, nchar(cards), nchar(cards)))

# baralho
cardDeck <- c(outer(c(2:10,"J","Q","K","A"),
                    c("H","S","C","D"),
                    paste0))

n_cards <- 1:length(cardDeck)
Data_frame_cards <- data.frame(n_cards, cardDeck)

# Create a sequence of 52 numbers (representing the cards)
# Cria uma sequência de 52 números (representando as cartas)
SEQ_CARDS <- seq(52)

# Definir a interface do usuário
ui <- dashboardPage(
  dashboardHeader(title = "Poker"),
  #dashboardSidebar(disable = TRUE),
  dashboardSidebar(
    helpText(" Configurações Iniciais para Simulação "),
    selectInput("language", "Escolha um idioma:",choices = c("English", "Português")),
    numericInput("n_simulation", "Número de Simulações para Cálculo das probabilidades", value = 300, max=1000, min = 1,step=1),  # Valor inicial e restrição mínima
    numericInput("n_players", "Número de Jogadores na mesa", value = 4, min = 2, max=9,step=1),  # Valor inicial e restrição mínima
    actionButton("default", "Redefinir")
  ),
  
  dashboardBody(
    tags$head(
      tags$script("
      $(window).resize(function() {
        if ($(window).width() <= 768) {
          $('.value-box .inner h3').css('font-size', '12px');
        } else {
          $('.value-box .inner h3').css('font-size', '');
        }
      });
      "),
      tags$style(
        HTML("
        .centered {
          display: flex;
          justify-content: center;
          align-items: center;
          height: 90%;
        }
        .centered-text {9
        display: flex;
        align-items: center;
        justify-content: center;
        
        }
        .caixa-limite {
        max-width: 900px;
        margin: 10px;
        text-align: center;
        align-items: center;

        }
        @font-face {
        font-family: 'Open Sans';
        src: url('OpenSans-Regular.ttf') format('truetype');
        }
        
        body {
        font-family: 'Open Sans', sans-serif;
        }
        
     #texto_dinamico span {
          font-size: 12px;
          margin-bottom: 0;
          padding-bottom: 0;
        }
        
      #minha-linha {
          height: 2px;
          background-color: blue;
          margin-bottom: 10px;
      }
      
      @media (max-width: 1024px) {
        body {
          font-size: 10px;
        }
        h1 {
          font-size: 14px;
        }
        h2 {
          font-size: 12px;
        }
        h3 {
          font-size: 5px;
        }
        

      }
        
      ")
      )
    ),
    div(class = "caixa-limite", 
    fluidRow(
      tags$div(class = "centered-text",h1(textOutput("resultado_texto"))),
      #column(width = 6, align = "center", offset = 3,textOutput("resultado_texto")),
      box(background = "red",solidHeader = FALSE, width = 12, uiOutput("hearts")),
      box(background = "red", solidHeader = TRUE, width = 12, uiOutput("diamonds")),
      box(background = "black", solidHeader = TRUE, width = 12, uiOutput("spades")),
      box(background = "black", solidHeader = TRUE, width = 12, uiOutput("clubs")),
      box(background = "olive", solidHeader = TRUE, width = 12, uiOutput("status"))
    ),
    
    fluidRow(
      column(
        width = 6, 
        div(
          class = "centered-text",
          actionButton("reset", "Reiniciar Jogo", style = "gradient", class = "btn-warning btn-lg",style ="border-radius: 50%;font-weight: bold;font-size: 1.2em;width: 20em;margin: 5px;")
        )
      ),

      column(
        width = 6, 
        div(
          class = "centered",
          actionButton("reload", "Reload", style = "gradient", class = "btn-warning btn-lg",style ="border-radius: 50%;font-weight: bold;font-size: 1.2em;width: 20em;margin: 5px;")
        )
      )
    ),
    
    tags$div(style = "margin-bottom: 10px;"),  # Adiciona espaço de 10 pixels abaixo dos botões
      fluidRow(
      #div(id = "minha-linha"),
      #tags$div(class = "centered-text",h2("Jogador 01")),
      box(title = "Jogador 01: Estatísticas",background = "olive", solidHeader = FALSE, width = 12,
        #box(background = "red",solidHeader = FALSE, width = 12, uiOutput("cards")),
        #uiOutput("texto_dinamico"),
        box(background = "green", solidHeader = TRUE, width = 8, uiOutput("player01")),
        box(background = "green", solidHeader = TRUE, width = 4, uiOutput("score")),
        box(background = "green", solidHeader = TRUE, width = 4, uiOutput("normal")),
        box(background = "green", solidHeader = TRUE, width = 4, uiOutput("simulation")),
        box(background = "green", solidHeader = TRUE, width = 4, uiOutput("decision")),
        ),
      
      box(title = "Resultado da Simulação: Probabilidades do jogo", status = "primary", solidHeader = TRUE, width = 12,
          DTOutput('oker_table')),
    
      box(title = "Mensagens", status = "primary", solidHeader = TRUE, width = 12,
          verbatimTextOutput("message"))
    ),
    
 
    # includeHTML("mesa.html"),  # Incluir o arquivo HTML
  )),
  tags$style("
    .btn-block {
      width: 100%;
    }
  ")
  
)

#######################################################################################################################################################################################################

# Defina o servidor
server <- function(input, output, session) {
  
  # Variável reativa para armazenar o valor anterior
  previous_value <- reactiveVal()

  output$resultado_texto <- renderText({
    
    texto <-Game_Guidance(lang_app(input$language),selected_cards)
    return(texto)
  })
  
  # https://www.compart.com/en/unicode/search?q=check#characters
  # <i class="fa-solid fa-face-frown"></i>
  # <i class="fa-solid fa-face-grin-stars"></i>
  # <i class="fa-solid fa-face-meh"></i>
  # <i class="fa-solid fa-dice-d6"></i>
  
  # Gere os botões das cartas
  output$player01 <- renderUI({
    tagList(
      tags$span( paste0(paste(convert_to_unicode(selected_cards$hand), collapse = " ")," "
      ),style = "font-weight: bold;font-size: 2.5em;padding:0px;margin: 0px;text-decoration: underline;"),
      tags$span(" ",style = "font-weight: bold;font-size: 2.5em;padding-bottom: 5px;margin-bottom: 5px;"),
      #tags$br(),
      tags$span(paste("",paste(convert_to_unicode(selected_cards$table), collapse = " ")
      ),style = "font-weight: bold;font-size: 2.5em;padding-bottom: 5px;margin-bottom: 5px;"),
    )
  })
 
  output$normal <- renderUI({
    tagList(
      tags$span(paste0((round(1/input$n_players*100,2)),"%"),style = "font-weight: bold;font-size: 2.5em;padding: 0px;margin: 0px;"),
      tags$h3(paste("Probabilidade de",input$n_players,"jogadores"),style = "font-weight: bold;font-size: 1em;padding: 0px;margin:0px;"),
    )
  })

  calcularFuncao2 <- function() {
  
    result2 <- reactive({
      if (length(selected_cards$hand) == 2 && length(selected_cards$table) ==0) {
        df_r<-SIMULATION_POKER_NION( Data_frame_cards = Data_frame_cards,
                                     N_SIMULATION=input$n_simulation,
                                     nPlayers = input$n_players,
                                     CARTA_PLAYER01_01 = selected_cards$hand[1],
                                     CARTA_PLAYER01_02 = selected_cards$hand[2],
                                     ROUND_ONE_01="",
                                     ROUND_ONE_02="",
                                     ROUND_ONE_03="",
                                     ROUND_TWO="",
                                     ROUND_THREE=""
        )
        previous_value(df_r)  # Armazenar o valor atual para uso posterior
        data.frame(df_r)
        
      }else  if (length(selected_cards$hand) == 2 && length(selected_cards$table) ==3) {
        df_r<- SIMULATION_POKER_NION( Data_frame_cards = Data_frame_cards,
                                        N_SIMULATION=input$n_simulation,
                                        nPlayers = input$n_players,
                                        CARTA_PLAYER01_01 = selected_cards$hand[1],
                                        CARTA_PLAYER01_02 = selected_cards$hand[2],
                                        ROUND_ONE_01=selected_cards$table[1],
                                        ROUND_ONE_02=selected_cards$table[2],
                                        ROUND_ONE_03=selected_cards$table[3],
                                        ROUND_TWO="",
                                        ROUND_THREE=""
        )
        previous_value(df_r)  # Armazenar o valor atual para uso posterior
        data.frame(df_r)
        
        }else if (length(selected_cards$hand) == 2 && length(selected_cards$table) ==4) {
          df_r<-  SIMULATION_POKER_NION( Data_frame_cards = Data_frame_cards,
                                          N_SIMULATION=input$n_simulation,
                                          nPlayers = input$n_players,
                                          CARTA_PLAYER01_01 = selected_cards$hand[1],
                                          CARTA_PLAYER01_02 = selected_cards$hand[2],
                                          ROUND_ONE_01=selected_cards$table[1],
                                          ROUND_ONE_02=selected_cards$table[2],
                                          ROUND_ONE_03=selected_cards$table[3],
                                          ROUND_TWO=selected_cards$table[4],
                                          ROUND_THREE=""
          )
          previous_value(df_r)  # Armazenar o valor atual para uso posterior
          data.frame(df_r)
          
        }else if (length(selected_cards$hand) == 2 && length(selected_cards$table) ==5) {
          df_r<- SIMULATION_POKER_NION( Data_frame_cards = Data_frame_cards,
                                          N_SIMULATION=input$n_simulation,
                                          nPlayers = input$n_players,
                                          CARTA_PLAYER01_01 = selected_cards$hand[1],
                                          CARTA_PLAYER01_02 = selected_cards$hand[2],
                                          ROUND_ONE_01=selected_cards$table[1],
                                          ROUND_ONE_02=selected_cards$table[2],
                                          ROUND_ONE_03=selected_cards$table[3],
                                          ROUND_TWO=selected_cards$table[4],
                                          ROUND_THREE=selected_cards$table[5]
          )
          previous_value(df_r)  # Armazenar o valor atual para uso posterior
          data.frame(df_r)
          
        } else if (!is.null(previous_value())) {
        data.frame(previous_value())
      }
    })
    
    output$message <- renderPrint({
    print( data.frame(previous_value()))
      
    })
  
    output$simulation <- renderUI({
      result_table <- result2()
      tagList(
        tags$span(paste(round(as.numeric(result_table$PROB[1]),2),"%"),style = "font-weight: bold;font-size: 2.5em;padding: 0px;margin: 0px;"),
        tags$h3(paste("Probabilidade Simulação:",input$n_simulation),style = "font-weight: bold;font-size: 1em;padding: 0px;margin:0px;"),
      )
    })
    
    output$score <- renderUI({
      result_table <- result2()
      tagList(
        tags$span(paste(get_poker_score(result_table$Score_P01[1],language = lang_app(input$language))),style = "font-weight: bold;font-size: 2.5em;padding: 0px;margin: 0px;"),
        #tags$h3(paste("Pontuação"),style = "font-weight: bold;font-size: 1em;padding: 0px;margin:0px;"),
      )
    })
    output$decision <- renderUI({
      result_table <- result2()
      tagList(
        tags$span(paste(result_table$Decision[1]),style = "font-weight: bold;font-size: 2.5em;padding: 0px;margin: 0px;"),
        tags$h3(paste("Sugestão"),style = "font-weight: bold;font-size: 1em;padding: 0px;margin:0px;"),
      )
    })
  
  output$valueBox4 <- renderValueBox({
    result_table <- result2()
    valueBox(
      paste(get_poker_score(result_table$Score_P01[1],language = lang_app(input$language))),
      paste("Sugestão:"),
      icon = icon("pie-chart"),
      color = "orange"
    )
  })
  
  
  output$oker_table <- renderDT({
    df_r<-get_poker_score_df(lang_app(input$language))
    
    if(length(selected_cards$hand)>1){
      
      df_r$Probability<-as.vector(unlist(previous_value()[1,9:17]))}
    
    DT::datatable(df_r,rownames=FALSE,filter = c("none"), options = list(dom = 't'),
                  caption = "Esta tabela mostra as decisões de poker com base em diferentes probabilidades.") %>% 
      formatStyle(columns = "Hand", 
                  fontWeight = 'bold', 
                  width = '30%',
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>% 
      
      formatStyle(columns = "Probability", 
                  fontWeight = 'bold', 
                  width = '70%',
                  background = styleColorBar(range(df_r$Probability), 'steelblue',angle = 90),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
    
  })
  
  
  }

  
  # Reiniciar o valor do campo de entrada numérico para o valor inicial
  observeEvent(input$default, {
    updateNumericInput(session, "n_simulation", value = 300)
    updateNumericInput(session, "n_players", value = 4)
    
  })
  
  # Variável para armazenar as cartas selecionadas
  selected_cards <- reactiveValues(hand = character(0), table = character(0), clicked = character(0))
  num_users <- reactiveVal(0)
  
  # Atualize num_users quando o input muda
  observeEvent(input$num_users, {
    num_users(input$num_users)
  })
  
  # Função para gerar um botão de carta
  cardButton <- function(card) {
    actionButton(inputId = card, color="warning",
                 #label = tags$img(src = paste0(card,".png"), height = "100px"), size= "sm", # tags$img(src = paste0(card,".png"), height = "100px") # card
                 label = convert_to_unicode(card),
                 style ="border-radius: 50%;font-weight: bold;font-size: 1.45em;width: 2.5em;height: 2.5em;",
                 #style ="border-radius: 20px;",
                 #style = "background-image: linear-gradient(to right, rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), linear-gradient(to right, #ff0000, #00ff00);color: white;",
                 #style = "background-color: transparent; border: none;",
                 class = ifelse(card %in% c(selected_cards$hand, selected_cards$table), "btn btn-primary", "btn btn-default"))
  }
  
  # Gere os botões das cartas
  output$hearts <- renderUI({
    lapply(cards_by_suit$H, function(card) {
      tagList(
        cardButton(card),
        tags$span(style = "margin-right: 2px;")
      )
    })
  })
  
  output$spades <- renderUI({
    lapply(cards_by_suit$S, function(card) {
      tagList(
        cardButton(card),
        tags$span(style = "margin-right: 2px;")
      )
    })
  })
  
  output$diamonds <- renderUI({
    lapply(cards_by_suit$D, function(card) {
      tagList(
        cardButton(card),
        tags$span(style = "margin-right: 2px;")
      )
    })
  })
  
  output$clubs <- renderUI({
    lapply(cards_by_suit$C, function(card) {
      tagList(
        cardButton(card),
        tags$span(style = "margin-right: 2px;")
      )
    })
  })
  
  output$status <- renderUI({
      tagList(
        tags$h3({if(length(selected_cards$hand)<=2 & length(selected_cards$table)==0){
          "Pre-Flop"
        } else if(length(selected_cards$table)<=3) {
          paste("Pre-Flop","\u2794","Flop")
        } else if(length(selected_cards$table)==4) {
          paste("Pre-Flop","\u2794","Flop","\u2794","Turn")
        } else if(length(selected_cards$table)==5) {
          paste("Pre-Flop","\u2794","Flop","\u2794","Turn","\u2794","River")
        }
        },style = "font-weight: bold;font-size: 2em;padding: 0;margin: 0;")
      )
  })
  
  
  # Quando um botão de carta é clicado, adicione a carta à seleção
  observe({
    for (card in cards) {
      if (isTruthy(input[[card]]) && !(card %in% selected_cards$clicked)) {
        if (length(c(selected_cards$hand, selected_cards$table)) < 7 &&
            !(card %in% c(selected_cards$hand, selected_cards$table))) {
          if (length(selected_cards$hand) < 2) {
            selected_cards$hand <- c(selected_cards$hand, card)
          } else {
            selected_cards$table <- c(selected_cards$table, card)
          }
        }
        selected_cards$clicked <- c(selected_cards$clicked, card)
      }
    }
    
  })
  
  # Quando o botão de resetar é clicado, limpe a seleção
  observeEvent(input$reset, {
    selected_cards$hand <- character(0)
    selected_cards$table <- character(0)
    selected_cards$clicked <- character(0)
    result<-data.frame()
    prob_player1<-"25%"
  })
  

  
  # Cálculo inicial
  calcularFuncao2()
  
  observeEvent(input$n_players, {
    calcularFuncao2()
  })
  
  observeEvent(input$n_simulation, {
    calcularFuncao2()
  })
  
  observeEvent(input$reload, {
    calcularFuncao2()
  })
  
}

# Executar o aplicativo
shinyApp(ui = ui, server = server)
