library(shiny)

source("global.R")
source("ui.R")
source("server.R")

# ------------------------------------------------
# Inicializar aplicação Shiny
# ------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)