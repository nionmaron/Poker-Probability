
# carregar as bibliotecas necessárias
library(keras)
library(dplyr)
library(purrr)
library(tensorflow)


# definir o caminho para o conjunto de dados de cartas de baralho
caminho_dados <- "CARDS/"

# carregar as imagens de cartas de baralho e suas etiquetas
cartas <- list.files(caminho_dados, full.names = TRUE) %>%
  map(image_load) # ou outra função para ler as imagens
  
  map(as.array) %>% # transformar em matriz
  #map(add_margin, 10, 10, 10, 10, "black") %>% # adicionar margem às imagens
  map(as.raster) %>% # transformar em objeto raster
  map(as.vector) %>% # transformar em vetor
  set_names(basename(list.files(caminho_dados))) # definir os nomes

nipes <- c("copas", "espadas", "ouros", "paus")
valores <- c("ás", "dois", "três", "quatro", "cinco", "seis", "sete", "oito", "nove", "dez", "valete", "dama", "rei")

rotulos <- expand.grid(nipes, valores) %>%
  mutate(rotulo = paste0(Var1, " ", Var2)) %>%
  pull(rotulo)

# criar um data frame com as imagens e as etiquetas
dados <- data.frame(imagem = cartas, rotulo = factor(rotulos))
