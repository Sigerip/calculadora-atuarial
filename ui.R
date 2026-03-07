ui <- fluidPage(
  
  titlePanel("Simulador de Seguro de Vida"),
  
  tabsetPanel(

    id = "paginas",
    
    tabPanel(
      "Seus dados",
      value = "pg01",
      
      fluidRow(
        column(
          width = 6,
          
          h3("Informações pessoais"),
          
          textInput(
            "nome",
            "Nome completo",
            placeholder = "Digite seu nome"
          ),
          
          numericInput(
            "idade",
            "Idade",
            value = NA,
            min = 18,
            max = 100
          ),
          
          numericInput(
            "renda",
            "Renda mensal (R$)",
            value = NA,
            min = 0,
            step = 500
          ),
          
          br(),
          
          actionButton(
            "calcular",
            "Calcular capital segurado ideal",
            class = "btn-primary"
          )
          
        )
      )
      
    ),
    
    tabPanel( # Página 2 - Tela que mostra o capital recomendado.
      "Capital recomendado",
      value = "pg02",
      
      fluidRow(
        column(
          width = 8,
          
          h2("Seu capital segurado recomendado"),
          
          br(),
          
          wellPanel(
            
            h3(textOutput("capital_texto")),
            
            p("Este valor é apenas ilustrativo."),
            
            p(
              "Com base nas informações fornecidas, ",
              "este seria o capital segurado estimado ",
              "para garantir proteção financeira aos seus dependentes."
            )
            
          ),
          
          br(),
          
          actionButton(
            "ver_simulacoes",
            "Ver simulações de preço",
            class = "btn-success"
          )
          
        )
      )
      
    ),
    
    tabPanel( # Página 3 - Tela que mostra as simulações de preço, deve ser permitido alguma simulação.
      "Simulações",
      value = "pg03",
      
      h2("Simulações de preço para seu seguro"),
      
      br(),
      
      tableOutput("tabela_simulacoes"),
      
      br(),
      
      wellPanel(
        p("Valores meramente ilustrativos:"),
        
        tags$ul(
          tags$li("Plano Essencial: R$ 85 / mês"),
          tags$li("Plano Proteção Familiar: R$ 120 / mês"),
          tags$li("Plano Premium: R$ 165 / mês")
        )
      ),
      
      br(),
      
      actionButton(
        "avaliar",
        "Responder pesquisa de satisfação",
        class = "btn-primary"
      )
      
    ),
    
    tabPanel( # Página 4 - Tela de pesquisa de satisfação.
      "Pesquisa", 
      value = "pg04",
      
      h2("Pesquisa de satisfação"),
      
      br(),
      
      textInput(
        "comentario",
        "O que você achou da simulação?",
        placeholder = "Digite seu comentário"
      ),
      
      sliderInput(
        "nota",
        "De 0 a 10, como você avalia a experiência?",
        min = 0,
        max = 10,
        value = 8
      ),
      
      selectInput(
        "recomendaria",
        "Você recomendaria este simulador?",
        choices = c(
          "Sim",
          "Talvez",
          "Não"
        )
      ),
      
      br(),
      
      actionButton(
        "enviar",
        "Enviar resposta",
        class = "btn-success"
      ),
      
      br(),
      br(),
      
      textOutput("agradecimento")
      
    )
    
  )
)