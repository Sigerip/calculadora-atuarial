library(shiny)
setwd("~/Projects/calc-oiatuarial")

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