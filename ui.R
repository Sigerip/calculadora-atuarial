ui <- fluidPage(
  
  titlePanel("Simulador de Seguro de Vida"),
  
  tags$head(
    tags$style(HTML("
      
      body {
        background-image: url('https://images.unsplash.com/photo-1450101499163-c8848c66ca85');
        background-size: cover;
        background-position: center;
        background-attachment: fixed;
      }
      
      .form-card {
        background-color: rgba(255,255,255,0.95);
        padding: 40px;
        border-radius: 20px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.25);
        max-width: 500px;
        margin: auto;
        margin-top: 80px;
      }
      
      .titulo-card {
        text-align: center;
        margin-bottom: 30px;
      }
      
    "))
  ),

  tabsetPanel(

    id = "paginas",

    
    tabPanel(
      "Seus dados",
      value = "pg01",

      div(
        class = "form-card",
        
        h2("Informações pessoais", class = "titulo-card"),
        
        textInput(
          "nome",
          "Nome completo",
          placeholder = "Digite seu nome"
        ),

        radioButtons(
          "sexo",
          "Sexo",
          choices = c(
            "Masculino",
            "Feminino"
          ),
          inline = TRUE
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
        
        ),

      br(),

      div(
        style = "text-align: center;",
        actionButton(
          "calcular",
          "Calcular capital segurado ideal",
          class = "btn-primary"
        )
      ),       
    ), # end tabPanel pg01
    
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