# ui.R - Interface do Usuario Corrigida
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)


ui <- fluidPage(
  
  # Titulo geral do aplicativo
  titlePanel("Calculadora Atuarial"),
  
  # Tema visual
  theme = shinytheme("cerulean"),
  
  # Layout
  sidebarLayout(
    
    # Painel lateral com inputs
    sidebarPanel(
      
      # Largura do painel lateral
      width = 4,
      
      # Titulo para os inputs
      h4("Parâmetros de Entrada"),
      
      # Input para o genero
      selectInput(inputId = "genero", label = "Selecione o Gênero:", choices = c("Masculino", "Feminino"), selected = "Masculino"),
      
      # Input para a tábua - Ligado ao server.R
      uiOutput("tabua_ui"),
      
      # Input para a idade do participante
      numericInput(inputId = "idade", label = "Idade Atual:", value = 30, min = 0, max = 120),
      
      # Input para o tempo de contribuicao
      numericInput(inputId = "tempo_contribuicao", label = "Tempo de Contribuição (anos):", value = 10, min = 1, max = 100),
      
      # Input para a taxa de juros com formatação percentual
      sliderInput(inputId = "taxa_de_juros", label = "Taxa de Juros Anual:", min = 0, max = 20, value = 6, step = 0.5, post = "%"),
      
      # Input para o beneficio projetado com formatação de moeda
      autonumericInput(inputId = "beneficio_projetado", label = "Valor do Benefício Projetado:",  value = 50000, currencySymbol = "R$ ", currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", minimumValue = 0),

      # Input para botão de ação
      actionButton("calcular", "Calcular")
      
    ),
    
    # Painel principal para outputs
    mainPanel(
      width = 8,
      
      # Paineis
      tabsetPanel(
        
        # Aba 1 - Visualização dos produtos atuariais
        tabPanel("Cálculo Atuarial",

                # Título da aba 
                h3("Resultados dos Cálculos Atuariais"),

                # ID para corresponder ao server.R
                verbatimTextOutput("resultados_calculos")
        ),
        
        # Aba 2 - Visualização da tábua selecionada
        tabPanel("Dados da Tábua", 
                # Título da aba
                h3("Dados da Tábua Selecionada"),
                
                # ID para corresponder ao server.R
                DT::dataTableOutput("tabela_exposta")
        )
      )
    )
  )
)