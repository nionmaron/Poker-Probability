# Carregar os pacotes necessários
library(shiny)
library(shinymanager)
library(shinydashboard)
source("Function_Poker.R")  # <- Suas funções personalizadas devem estar aqui
# library(poker)
library(shinyWidgets)
library(DT)

# ------------------------------------------------------------
# 1) Crie o dicionário de textos
# ------------------------------------------------------------
text_dict <- list(
  pt = list(
    app_title         = "NionPoker",
    sidebar_title     = "Configurações Iniciais para Simulação",
    language_label    = "Escolha um idioma:",
    n_simulation_lbl  = "Número de Simulações para Cálculo das probabilidades",
    n_players_lbl     = "Número de Jogadores na mesa",
    reset_input_btn   = "Redefinir",
    reset_game_btn    = "Reiniciar Jogo",
    reload_btn        = "Recarregar",
    
    # Boxes e Mensagens
    box_player_title  = "Jogador 01: Estatísticas",
    box_result_title  = "Resultado da Simulação: Probabilidades do jogo",
    box_message_title = "Mensagens",
    
    probability_players = "Probabilidade de %d jogadores",
    probability_sim     = "Probabilidade Simulação: %d",
    pontuacao_label     = "Pontuação",
    sugestao_label      = "Sugestão",
    
    # Fases
    fase_preflop = "Pre-Flop",
    fase_flop    = "Flop",
    fase_turn    = "Turn",
    fase_river   = "River"
  ),
  en = list(
    app_title         = "NionPoker",
    sidebar_title     = "Initial Settings for Simulation",
    language_label    = "Choose a language:",
    n_simulation_lbl  = "Number of Simulations to Calculate Probability",
    n_players_lbl     = "Number of Players at the table",
    reset_input_btn   = "Reset",
    reset_game_btn    = "Reset Game",
    reload_btn        = "Reload",
    
    # Boxes and Messages
    box_player_title  = "Player 01: Statistics",
    box_result_title  = "Simulation Result: Game Probabilities",
    box_message_title = "Messages",
    
    probability_players = "Probability for %d players",
    probability_sim     = "Simulation Probability: %d",
    pontuacao_label     = "Score",
    sugestao_label      = "Suggestion",
    
    # Stages
    fase_preflop = "Pre-Flop",
    fase_flop    = "Flop",
    fase_turn    = "Turn",
    fase_river   = "River"
  )
)

# ------------------------------------------------------------
# 2) Função para mapear selectInput em "pt" / "en"
# ------------------------------------------------------------
lang_app <- function(user_input) {
  if (user_input == "English") {
    return("en")
  } else {
    return("pt")
  }
}

# ------------------------------------------------------------
# 3) Dados e variáveis auxiliares
# ------------------------------------------------------------
Score <- 1:9
my_score <- c("","","","","","","","","")
score_dict_en <- c("High Card", "One Pair", "Two Pair", "Three of a Kind", "Straight", "Flush", "Full House", "Four of a Kind", "Straight Flush")
score_dict_pt <- c("Carta Alta", "Um Par", "Dois Pares", "Trinca", "Sequência", "Flush", "Full House", "Quadra", "Sequência de Mesmo Naipe")
Prob_Simulation <- c(50.1177,42.2569,4.7539,2.1128,0.3925,0.1965,0.1441,0.0240,0.00139)

# Vetor com os nomes das cartas
cards <- c(paste0(rep(2:10, each = 4), c("H","S","C","D")), 
           paste0(rep(c('J', 'Q', 'K', 'A'), each = 4), c("H","S","C","D")))

# Separe e ordene as cartas por naipe
cards_by_suit <- split(cards, substr(cards, nchar(cards), nchar(cards)))

# baralho
cardDeck <- c(outer(c(2:10,"J","Q","K","A"),
                    c("H","S","C","D"),
                    paste0))

n_cards <- 1:length(cardDeck)
Data_frame_cards <- data.frame(n_cards, cardDeck)


# ------------------------------------------------------------
# 4) Definir a interface do usuário (UI)
#    A ideia aqui é usar textOutput() ou deixar labels vazios
#    para serem atualizados via server.
# ------------------------------------------------------------
ui <- tagList(
  # Definir o título e o favicon no head
  tags$head(
    tags$title("NionPoker"),
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "description", content = "Poker app to learn card probability")
  ),
  
  dashboardPage(skin = "green",
                dashboardHeader(
                  title = tagList(
                    icon("diamond"), 
                    textOutput("header_title", inline = TRUE)  # Título dinâmico
                  )
                ),
                
                dashboardSidebar(
                  helpText(textOutput("sidebar_title")),  # Texto dinâmico
                  
                  # SelectInput sem label fixo; será atualizado via server
                  selectInput("language", "", choices = c("English", "Português")),
                  
                  numericInput("n_simulation", "", value = 300, max=1000, min = 1, step=1),
                  numericInput("n_players", "", value = 4, min = 2, max=9, step=1),
                  
                  actionButton("default", "")  # Botão sem label fixo
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
            .centered-text {
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
                        tags$div(class = "centered-text", h1(textOutput("resultado_texto"))),
                        
                        box(background = "red",    solidHeader = FALSE, width = 12, uiOutput("hearts")),
                        box(background = "red",    solidHeader = TRUE,  width = 12, uiOutput("diamonds")),
                        box(background = "black",  solidHeader = TRUE,  width = 12, uiOutput("spades")),
                        box(background = "black",  solidHeader = TRUE,  width = 12, uiOutput("clubs")),
                        box(background = "olive",  solidHeader = TRUE,  width = 12, uiOutput("status"))
                      ),
                      
                      fluidRow(
                        column(
                          width = 6, 
                          div(class = "centered-text",
                              actionButton("reset", "",  # sem label fixo; atualiza via server
                                           style = "gradient", 
                                           class = "btn-warning btn-lg",
                                           style ="border-radius: 50%;font-weight: bold;font-size: 1.2em;width: 20em;margin: 5px;")
                          )
                        ),
                        
                        column(
                          width = 6, 
                          div(class = "centered",
                              actionButton("reload", "",  # sem label fixo; atualiza via server
                                           style = "gradient", 
                                           class = "btn-warning btn-lg",
                                           style ="border-radius: 50%;font-weight: bold;font-size: 1.2em;width: 20em;margin: 5px;")
                          )
                        )
                      ),
                      
                      tags$div(style = "margin-bottom: 10px;"),  # Espaço
                      
                      fluidRow(
                        box(title = textOutput("player_box_title"), background = "olive", solidHeader = FALSE, width = 12,
                            box(background = "green", solidHeader = TRUE, width = 8, uiOutput("player01")),
                            box(background = "green", solidHeader = TRUE, width = 4, uiOutput("score")),
                            box(background = "green", solidHeader = TRUE, width = 4, uiOutput("normal")),
                            box(background = "green", solidHeader = TRUE, width = 4, uiOutput("simulation")),
                            box(background = "green", solidHeader = TRUE, width = 4, uiOutput("decision"))
                        ),
                        
                        box(title = textOutput("result_box_title"), status = "primary", solidHeader = TRUE, width = 12,
                            DTOutput('oker_table')
                        ),
                        
                        box(title = textOutput("message_box_title"), status = "primary", solidHeader = TRUE, width = 12,
                            verbatimTextOutput("message"))
                      )
                  ),
                  
                  tags$style(".btn-block { width: 100%; }")
                )
  )
)


# ------------------------------------------------------------
# 5) Defina o servidor (server)
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # ---------- ATUALIZAÇÃO DE TEXTOS CONFORME O IDIOMA ----------
  observe({
    current_lang <- lang_app(input$language)
    
    # Atualiza os textos fixos da barra lateral
    output$sidebar_title <- renderText({
      text_dict[[current_lang]]$sidebar_title
    })
    
    # Atualiza título do cabeçalho
    output$header_title <- renderText({
      text_dict[[current_lang]]$app_title
    })
    
    # Labels de inputs
    updateSelectInput(session, "language",
                      label = text_dict[[current_lang]]$language_label
    )
    updateNumericInput(session, "n_simulation",
                       label = text_dict[[current_lang]]$n_simulation_lbl
    )
    updateNumericInput(session, "n_players",
                       label = text_dict[[current_lang]]$n_players_lbl
    )
    updateActionButton(session, "default",
                       label = text_dict[[current_lang]]$reset_input_btn
    )
    
    # Botões de reiniciar/reload
    updateActionButton(session, "reset",
                       label = text_dict[[current_lang]]$reset_game_btn
    )
    updateActionButton(session, "reload",
                       label = text_dict[[current_lang]]$reload_btn
    )
    
    # Títulos das boxes
    output$player_box_title <- renderText({
      text_dict[[current_lang]]$box_player_title
    })
    output$result_box_title <- renderText({
      text_dict[[current_lang]]$box_result_title
    })
    output$message_box_title <- renderText({
      text_dict[[current_lang]]$box_message_title
    })
  })
  # ------------------------------------------------------------
  
  # Variável reativa para armazenar o valor anterior (resultado da simulação)
  previous_value <- reactiveVal()
  
  output$resultado_texto <- renderText({
    # Exemplo: Game_Guidance() recebe "pt" ou "en"
    texto <- Game_Guidance(lang_app(input$language), selected_cards)
    return(texto)
  })
  
  # ---------- UI do Jogador 01 ----------
  output$player01 <- renderUI({
    tagList(
      tags$span(
        paste0(
          paste(convert_to_unicode(selected_cards$hand), collapse = " "), " "
        ),
        style = "font-weight: bold;font-size: 2.5em;padding:0px;margin: 0px;text-decoration: underline;"
      ),
      tags$span(" ", style = "font-weight: bold;font-size: 2.5em;padding-bottom: 5px;margin-bottom: 5px;"),
      tags$span(
        paste("", paste(convert_to_unicode(selected_cards$table), collapse = " ")),
        style = "font-weight: bold;font-size: 2.5em;padding-bottom: 5px;margin-bottom: 5px;"
      )
    )
  })
  
  # Probabilidade normal (1 / n_players)
  output$normal <- renderUI({
    current_lang <- lang_app(input$language)
    tagList(
      tags$span(
        paste0(round(1/input$n_players*100,2), "%"),
        style = "font-weight: bold;font-size: 2.5em;padding: 0px;margin: 0px;"
      ),
      tags$h3(
        sprintf(text_dict[[current_lang]]$probability_players, input$n_players),
        style = "font-weight: bold;font-size: 1em;padding: 0px;margin:0px;"
      )
    )
  })
  
  # ---------- SIMULAÇÃO ----------
  calcularFuncao2 <- function() {
    
    result2 <- reactive({
      if (length(selected_cards$hand) == 2 && length(selected_cards$table) == 0) {
        
        df_r <- SIMULATION_POKER_NION(
          Data_frame_cards = Data_frame_cards,
          N_SIMULATION = input$n_simulation,
          nPlayers = input$n_players,
          CARTA_PLAYER01_01 = selected_cards$hand[1],
          CARTA_PLAYER01_02 = selected_cards$hand[2],
          ROUND_ONE_01 = "",
          ROUND_ONE_02 = "",
          ROUND_ONE_03 = "",
          ROUND_TWO = "",
          ROUND_THREE = ""
        )
        previous_value(df_r)
        data.frame(df_r)
        
      } else if (length(selected_cards$hand) == 2 && length(selected_cards$table) == 3) {
        
        df_r <- SIMULATION_POKER_NION(
          Data_frame_cards = Data_frame_cards,
          N_SIMULATION = input$n_simulation,
          nPlayers = input$n_players,
          CARTA_PLAYER01_01 = selected_cards$hand[1],
          CARTA_PLAYER01_02 = selected_cards$hand[2],
          ROUND_ONE_01 = selected_cards$table[1],
          ROUND_ONE_02 = selected_cards$table[2],
          ROUND_ONE_03 = selected_cards$table[3],
          ROUND_TWO = "",
          ROUND_THREE = ""
        )
        previous_value(df_r)
        data.frame(df_r)
        
      } else if (length(selected_cards$hand) == 2 && length(selected_cards$table) == 4) {
        
        df_r <- SIMULATION_POKER_NION(
          Data_frame_cards = Data_frame_cards,
          N_SIMULATION = input$n_simulation,
          nPlayers = input$n_players,
          CARTA_PLAYER01_01 = selected_cards$hand[1],
          CARTA_PLAYER01_02 = selected_cards$hand[2],
          ROUND_ONE_01 = selected_cards$table[1],
          ROUND_ONE_02 = selected_cards$table[2],
          ROUND_ONE_03 = selected_cards$table[3],
          ROUND_TWO = selected_cards$table[4],
          ROUND_THREE = ""
        )
        previous_value(df_r)
        data.frame(df_r)
        
      } else if (length(selected_cards$hand) == 2 && length(selected_cards$table) == 5) {
        
        df_r <- SIMULATION_POKER_NION(
          Data_frame_cards = Data_frame_cards,
          N_SIMULATION = input$n_simulation,
          nPlayers = input$n_players,
          CARTA_PLAYER01_01 = selected_cards$hand[1],
          CARTA_PLAYER01_02 = selected_cards$hand[2],
          ROUND_ONE_01 = selected_cards$table[1],
          ROUND_ONE_02 = selected_cards$table[2],
          ROUND_ONE_03 = selected_cards$table[3],
          ROUND_TWO = selected_cards$table[4],
          ROUND_THREE = selected_cards$table[5]
        )
        previous_value(df_r)
        data.frame(df_r)
        
      } else if (!is.null(previous_value())) {
        data.frame(previous_value())
      }
    })
    
    # Mostrar mensagens
    output$message <- renderPrint({
      print( data.frame(previous_value()) )
    })
    
    # Probabilidade simulada
    output$simulation <- renderUI({
      result_table <- result2()
      tagList(
        tags$span(
          ifelse(length(selected_cards$hand) > 1,
                 paste(round(as.numeric(result_table$PROB[1]),2), "%"),
                 "-"),
          style = "font-weight: bold;font-size: 2.5em;padding: 0px;margin: 0px;"
        ),
        tags$h3(
          sprintf(text_dict[[lang_app(input$language)]]$probability_sim, input$n_simulation),
          style = "font-weight: bold;font-size: 1em;padding: 0px;margin:0px;"
        )
      )
    })
    
    # Pontuação
    output$score <- renderUI({
      result_table <- result2()
      lang_now <- lang_app(input$language)
      tagList(
        tags$span(
          if (length(selected_cards$hand) > 1) {
            paste(get_poker_score(result_table$Score_P01[1], language = lang_now))
          } else {
            "-"
          },
          style = "font-weight: bold;font-size: 2.5em;padding: 0px;margin: 0px;"
        ),
        tags$h3(
          text_dict[[lang_now]]$pontuacao_label,
          style = "font-weight: bold;font-size: 1em;padding: 0px;margin:0px;"
        )
      )
    })
    
    # Decisão / Sugestão
    output$decision <- renderUI({
      result_table <- result2()
      lang_now <- lang_app(input$language)
      tagList(
        tags$span(
          ifelse(length(selected_cards$hand)>1,
                 paste(result_table$Decision[1]),
                 "-"),
          style = "font-weight: bold;font-size: 2.5em;padding: 0px;margin: 0px;"
        ),
        tags$h3(
          text_dict[[lang_now]]$sugestao_label,
          style = "font-weight: bold;font-size: 1em;padding: 0px;margin:0px;"
        )
      )
    })
    
    # Tabela de probabilidades
    output$oker_table <- renderDT({
      df_r <- get_poker_score_df(lang_app(input$language))
      
      if(length(selected_cards$hand) > 1){
        df_r$Probability <- as.vector(unlist(previous_value()[1,9:17]))
      }
      
      DT::datatable(
        df_r, rownames = FALSE, filter = c("none"),
        options = list(dom = 't'),
        caption = "Esta tabela mostra as decisões de poker com base em diferentes probabilidades."
      ) %>% 
        formatStyle(
          columns = "Hand", 
          fontWeight = 'bold', 
          width = '30%',
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'right'
        ) %>% 
        formatStyle(
          columns = "Probability", 
          fontWeight = 'bold', 
          width = '70%',
          background = styleColorBar(range(df_r$Probability), 'steelblue', angle = 90),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'right'
        )
    })
    
  }
  
  # Chama a função de cálculo inicial
  calcularFuncao2()
  
  # Observers para recalcular quando mudam inputs
  observeEvent(input$n_players, {
    calcularFuncao2()
  })
  observeEvent(input$n_simulation, {
    calcularFuncao2()
  })
  observeEvent(input$reload, {
    calcularFuncao2()
  })
  
  # --------- REDEFINIR INPUTS -----------
  observeEvent(input$default, {
    updateNumericInput(session, "n_simulation", value = 300)
    updateNumericInput(session, "n_players", value = 4)
  })
  
  # --------- MANIPULAÇÃO DE CARTAS SELECIONADAS -----------
  selected_cards <- reactiveValues(hand = character(0), table = character(0), clicked = character(0))
  
  # Função para criar botões de cartas
  cardButton <- function(card) {
    actionButton(
      inputId = card,
      color = "warning",
      label = convert_to_unicode(card),
      style = "border-radius: 50%;font-weight: bold;font-size: 1.45em;width: 2.5em;height: 2.5em;",
      class = ifelse(card %in% c(selected_cards$hand, selected_cards$table),
                     "btn btn-primary", 
                     "btn btn-default")
    )
  }
  
  # Render dos botões por naipe
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
  
  # Fases do jogo
  output$status <- renderUI({
    current_lang <- lang_app(input$language)
    tagList(
      tags$h3({
        if(length(selected_cards$hand) <= 2 & length(selected_cards$table) == 0){
          text_dict[[current_lang]]$fase_preflop
        } else if(length(selected_cards$table) <= 3) {
          paste(
            text_dict[[current_lang]]$fase_preflop, 
            "\u2794", 
            text_dict[[current_lang]]$fase_flop
          )
        } else if(length(selected_cards$table) == 4) {
          paste(
            text_dict[[current_lang]]$fase_preflop, 
            "\u2794", 
            text_dict[[current_lang]]$fase_flop, 
            "\u2794", 
            text_dict[[current_lang]]$fase_turn
          )
        } else if(length(selected_cards$table) == 5) {
          paste(
            text_dict[[current_lang]]$fase_preflop, 
            "\u2794", 
            text_dict[[current_lang]]$fase_flop,
            "\u2794", 
            text_dict[[current_lang]]$fase_turn, 
            "\u2794", 
            text_dict[[current_lang]]$fase_river
          )
        }
      },
      style = "font-weight: bold;font-size: 2em;padding: 0;margin: 0;")
    )
  })
  
  # Quando um botão de carta é clicado, adiciona a carta à seleção
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
  
  # Quando clica em "Reiniciar Jogo"
  observeEvent(input$reset, {
    selected_cards$hand <- character(0)
    selected_cards$table <- character(0)
    selected_cards$clicked <- character(0)
    
    result <- data.frame()
  })
  
}

# ------------------------------------------------------------
# 6) Executar o aplicativo
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)
