library(shiny)
library(shinyjs)  # Pacote para permitir JavaScript
library(shinythemes)

# Vetor com os nomes das cartas
cards <- c(paste0(rep(c('A', 2:10, 'J', 'Q', 'K'), each = 4), c('H', 'S', 'D', 'C')))

# Separe e ordene as cartas por naipe
#cards <- sort(cards)
cards_by_suit <- split(cards, substr(cards, nchar(cards), nchar(cards)))

# Crie a interface do usuário
ui <- fluidPage(
  shinyjs::useShinyjs(),  # Ativar o shinyjs
  titlePanel("Seletor de Cartas de Poker"),
  theme = shinytheme("cerulean"),  # Use o tema cerulean para melhorar a estética
  fluidRow(
    column(2,
           actionButton("reset", "Resetar seleção")),
    column(2, 
           h3("Copas"),
           uiOutput("hearts")),
    column(2,
           h3("Espadas"),
           uiOutput("spades")),
    column(2,
           h3("Ouros"),
           uiOutput("diamonds")),
    column(2,
           h3("Paus"),
           uiOutput("clubs")),
    column(4, 
           h3("Selecionadas"),
           verbatimTextOutput("description")),
    column(12, 
           h3("Mensagens"),
           verbatimTextOutput("message"))
  )
)

# Defina o servidor
server <- function(input, output, session) {
  # Variável para armazenar as cartas selecionadas
  selected_cards <- reactiveValues(hand = character(0), table = character(0))
  
  # Função para gerar um botão de carta
  cardButton <- function(card) {
    tags$div(id = card,
             class = ifelse(card %in% c(selected_cards$hand, selected_cards$table), "btn btn-primary", "btn btn-default"),
             card,
             onclick = sprintf("Shiny.setInputValue('%s', 1, {priority: 'event'})", card))
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
      if (isTruthy(input[[card]])) {
        if (length(c(selected_cards$hand, selected_cards$table)) < 7 &&
            !(card %in% c(selected_cards$hand, selected_cards$table))) {
          if (length(selected_cards$hand) < 2) {
            selected_cards$hand <- c(selected_cards$hand, card)
          } else {
            selected_cards$table <- c(selected_cards$table, card)
          }
          shinyjs::addClass(selector = sprintf("#%s", card), class = "btn-primary")
          shinyjs::removeClass(selector = sprintf("#%s", card), class = "btn-default")
        }
      }
    }
  })
  
 # Quando o botão de resetar é clicado, limpe a seleção
observeEvent(input$reset, {
  selected_cards$hand <- character(0)
  selected_cards$table <- character(0)
  lapply(cards, function(card) {
    updateActionButton(session, card, card, class = "btn btn-default", icon = NULL)
  })
})
  
  output$description <- renderPrint({
    cat("Mão:\n", 
        paste(selected_cards$hand, collapse = ", "),
        "\nMesa:\n",
        paste(selected_cards$table, collapse = ", "))
  })
  
  output$message <- renderPrint({
    num_cards <- length(c(selected_cards$hand, selected_cards$table))
    if(num_cards==2){cat("Primeira etapa completa! Selecione as 3 cartas da mesa.")}
  })
}

# Execute o aplicativo
shinyApp(ui = ui, server = server)

