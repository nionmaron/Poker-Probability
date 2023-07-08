
# Load 'poker' library
# Carrega a biblioteca 'poker'
library(poker)

# Load custom functions from 'Function_Poker.R' file
# Carrega as funções personalizadas do arquivo 'Function_Poker.R'
source("Function_Poker.R")

# Create a deck of cards
# Cria um baralho de cartas

# "H" - Hearts (Copas)
# "S" - Spades (Espadas)
# "C" - Clubs (Paus)
# "D" - Diamonds (Ouros)

cardDeck <- c(outer(c(2:10,"J","Q","K","A"),
                    c("H","S","C","D"),
                    paste0))

n_cards <- 1:length(cardDeck)
Data_frame_cards <- data.frame(n_cards, cardDeck)

# Create a sequence of 52 numbers (representing the cards)
# Cria uma sequência de 52 números (representando as cartas)
SEQ_CARDS <- seq(52)

# Display the deck
# Exibe o baralho
cardDeck

# Test the functions
# Testa as funções
testRoundOfPoker(2)
NION_POKER(9)

# Poker simulation settings
# Configurações de simulação de poker
N_SIMULATION <- 300  # Number of simulations / Número de simulações
PLAYERS <- 4         # Number of players / Número de jogadores

{
  # Define the initial cards of the players
  # Define as cartas iniciais dos jogadores
  CARD01 <- "QH"
  CARD02 <- "10H"
  
  # Define the flop cards (first 3 community cards)
  # Define as cartas do flop (3 primeiras cartas comunitárias)
  CARD03 <- "5S"
  CARD04 <- "7C"
  CARD05 <- "AD"
  
  # Define the turn and river cards (4th and 5th community cards)
  # Define as cartas do turn e do river (4ª e 5ª cartas comunitárias)
  CARD06 <- "JS"
  CARD07 <- ""
  
  nPlayers <- PLAYERS
  CARTA_PLAYER01_01 <- CARD01
  CARTA_PLAYER01_02 <- CARD02
  ROUND_ONE_01 <- CARD03
  ROUND_ONE_02 <- CARD04
  ROUND_ONE_03 <- CARD05
  ROUND_TWO <- CARD06
  ROUND_THREE <- CARD07
  
  # Run the poker simulation
  # Executa a simulação de poker
  SIMULATION_POKER_NION(
    Data_frame_cards = Data_frame_cards,
    N_SIMULATION,
    nPlayers = nPlayers,
    CARTA_PLAYER01_01 = CARTA_PLAYER01_01,
    CARTA_PLAYER01_02,
    ROUND_ONE_01,
    ROUND_ONE_02,
    ROUND_ONE_03,
    ROUND_TWO,
    ROUND_THREE
  )
}

# Criar o Ás de Espadas
as_de_espadas <- paste0("A", "\u2660")
print(as_de_espadas)

dotPairRanker(c(3,3,5,6,7,13,4))


# 9 = Straight Flush
# 8 = Four of a Kind
# 7 = Full House
# 6 = Flush
# 5 = Straight
# 4 = Three of a Kind
# 3 = Two Pair
# 2 = One Pair
# 1 = High Card


dotHighcard(matrix(c(2,1,14,2,5,3,6,4,7,1,13,2,14,3,2,3,3,4,5,1,6,2,7,3,13,4,14,1),2,14,byrow=TRUE))

