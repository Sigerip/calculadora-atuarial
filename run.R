# run.R - Script principal para executar a aplicacao

# Limpar ambiente
rm(list = ls())

# Carregar bibliotecas principais
library(shiny)

# Carregar arquivos
source("global.R")
source("ui.R")
source("server.R")

# Inicia o app
shinyApp(ui = ui, server = server)
