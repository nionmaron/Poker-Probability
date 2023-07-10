# Carregar os pacotes necessários
library(shiny)
library(shinymanager)
library(shinydashboard)
source("Function_Poker.R")
library(poker)
library(shinyWidgets)

# Vetor com os nomes das cartas
cards <- c(paste0(rep(2:10, each = 4), c("H","S","C","D")), 
           paste0(rep(c('J', 'Q', 'K', 'A'), each = 4), c("H","S","C","D")))

# Separe e ordene as cartas por naipe
# cards <- sort(cards)
cards_by_suit <- split(cards, substr(cards, nchar(cards), nchar(cards)))
cards_by_suit
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
  dashboardHeader(title = "Poker Card Selector"),
  #dashboardSidebar(disable = TRUE),
  dashboardSidebar(
    helpText(" Configurações Iniciais para Simulação "),
    selectInput("language", "Escolha um idioma:",choices = c("English", "Português")),
    numericInput("n_simulation", "Número de Simulações para Cálculo das probabilidades", value = 300, max=1000, min = 1,step=1),  # Valor inicial e restrição mínima
    numericInput("n_players", "Número de Jogadores na mesa", value = 4, min = 2, max=9,step=1),  # Valor inicial e restrição mínima
    actionButton("default", "Redefinir")
  ),
  
  
  dashboardBody(
    fluidRow(
      box(title = "Copas (Hearts)",background = "red",solidHeader = FALSE, width = 12, uiOutput("hearts")),
      box(title = "Diamantes (Diamonds)",background = "red", solidHeader = TRUE, width = 12, uiOutput("diamonds")),
      box(title = "Espadas (Spades)",background = "black", solidHeader = TRUE, width = 12, uiOutput("spades")),
      box(title = "Paus (Clubs)", background = "black", solidHeader = TRUE, width = 12, uiOutput("clubs")),
      box(title = "Cartas selecionadas", status = "primary", solidHeader = TRUE, width = 12,
          verbatimTextOutput("description")),
      #box(title = "Número de jogadores na mesa", status = "primary", solidHeader = TRUE, width = 12,
          #radioButtons("num_users", label = NULL, choices = 1:9, inline = TRUE),
          #),
      box(actionBttn("reset", "Reiniciar Jogo", style = "gradient", color = "warning", size = "lg")),
      box(actionBttn("reload", "Reload", style = "gradient", color = "warning", size = "lg")),
      box(title = "Mensagens", status = "primary", solidHeader = TRUE, width = 12,
          verbatimTextOutput("message"))
    )
    # includeHTML("mesa.html"),  # Incluir o arquivo HTML
  )
)

# Defina o servidor
server <- function(input, output, session) {
  
  
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
    actionButton(inputId = card, 
                 label = card, 
                 class = ifelse(card %in% c(selected_cards$hand, selected_cards$table), "btn btn-primary", "btn btn-default"))
  }
  
  # Gere os botões das cartas
  output$hearts <- renderUI({
    lapply(cards_by_suit$H, cardButton)
  })
  
  output$spades <- renderUI({
    lapply(cards_by_suit$S, cardButton)
  })
  
  output$diamonds <- renderUI({
    lapply(cards_by_suit$D, cardButton)
  })
  
  output$clubs <- renderUI({
    lapply(cards_by_suit$C, cardButton)
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
  })
  
  # Exibir a descrição das cartas selecionadas
  output$description <- renderPrint({
    txt<-{
      if (length(selected_cards$hand) < 2) {
        "Selecione duas cartas para sua mão."
      } else if (length(selected_cards$table) < 3) {
        "Selecione três cartas para a mesa."
      } else if (length(selected_cards$table) < 4) {
        "Selecione uma quarta carta para a mesa."
      } else if (length(selected_cards$table) < 5) {
        "Selecione uma quinta carta para a mesa."
      } else {
        "Todas as cartas foram selecionadas. Boa sorte!"
      }
    }
    cat(txt,"\nMão:", 
        
        paste(convert_to_unicode(selected_cards$hand), collapse = "-"),
        "\nMesa:",
        paste(convert_to_unicode(selected_cards$table), collapse = "-"))
  })
  
  
  # Exibir uma mensagem dependendo da etapa do jogo
  calcularFuncao <- function() {
  output$message <- renderPrint({
    
    if (length(selected_cards$hand) == 2 && length(selected_cards$table) ==0) {
      cat("Número de Simulações:",input$n_simulation)
      cat("\nJogadores:",input$n_players)
      SIMULATION_POKER_NION( Data_frame_cards = Data_frame_cards,
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
    }
    
    if (length(selected_cards$hand) == 2 && length(selected_cards$table) ==3) {
      SIMULATION_POKER_NION( Data_frame_cards = Data_frame_cards,
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
    }
    
    if (length(selected_cards$hand) == 2 && length(selected_cards$table) ==4) {
      SIMULATION_POKER_NION( Data_frame_cards = Data_frame_cards,
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
    }
    
    if (length(selected_cards$hand) == 2 && length(selected_cards$table) ==5) {
      SIMULATION_POKER_NION( Data_frame_cards = Data_frame_cards,
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
    }
    
    

    
  })
  
  }# recalcular
  
  # Cálculo inicial
  calcularFuncao()
  
  observeEvent(input$n_players, {
    calcularFuncao()
  })
  
  observeEvent(input$n_simulation, {
    calcularFuncao()
  })
  
  observeEvent(input$reload, {
    calcularFuncao()
  })
  
}

# Executar o aplicativo
shinyApp(ui = ui, server = server)
