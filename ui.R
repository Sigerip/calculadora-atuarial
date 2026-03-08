ui <- fluidPage(
  
  titlePanel("Simulador de Seguro de Vida"),


  #Imagem de fundo séria, será que não tem uma mais legal ?
  #url('https://images.unsplash.com/photo-1450101499163-c8848c66ca85')

  tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

  tabsetPanel(

    id = "paginas",

    tabPanel(
      "Início",
      value = "pg00",
      
      div(class = "resultado-container",
          
          # título da aplicação
          
          h2("Simulador de Seguro de Vida"),
          
          
          # card principal
          
          div(class = "card-resultado",
              fluidRow(
                  column(
                    width = 4,
                    
                    tags$img(
                      src = "pg00_familia.png",
                      width = "100%"
                    )
                  ),
                  
                  column(
                    width = 8,
                    h2("Bem-vindo ao"),
                    h1("Simulador de Seguro de Vida!")
                  )
              )
          ),
          
          br(),
          
          div(class = "card-explicacao", # card explicação
              
              h3("Aqui você poderá:"),
              
              tags$ul(class = "lista-explicacao",
                      
                      tags$li(
                        "Simular o capital necessário para ",
                        tags$b("garantir a segurança financeira"),
                        " da sua família em caso de falecimento."
                      ),
                      
                      tags$li(
                        "Calcular valores de ",
                        tags$b("seguro de vida"),
                        " adequados para sua necessidade."
                      ),
                      
                      tags$li(
                        "Comparar os custos de diferentes ",
                        tags$b("planos de seguro de vida.")
                      )
              ),
              
              br(),
              
              div(
                class = "alert alert-warning",
                
                tags$b("Aviso: "),
                "Este simulador oferece estimativas baseadas nas informações ",
                "que você fornecer. Para uma análise detalhada e personalizada, ",
                "consulte um corretor de seguros profissional."
              )
          ),
          
          br(),
          
          div( # botão iniciar
            style = "text-align:center;",
            
            actionButton(
              "iniciar_simulacao",
              "Iniciar Simulação",
              class = "btn-navegacao"
            )
          ),
          
          br(),
          
          
          # link de ajuda
          div(
            style = "text-align:center;",
            
            p(
              "Precisa de ajuda? ",
              tags$a(
                "Clique aqui",
                href = "#",
                target = "_blank"
              )
            )
          ),
          
          
          br(),
          
          
          # disclaimer inferior
          
          div(class = "card-explicacao",
              
              p(
                tags$b("Aviso importante: "),
                "Este simulador oferece estimativas baseadas nas informações ",
                "fornecidas. Os resultados não constituem oferta ou recomendação ",
                "de contratação de seguro."
              )
          )
          
      )
    ),

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
          class = "btn-navegacao"
        )
      ),       
    ), # end tabPanel pg01
    
    tabPanel(
  "Capital recomendado",
  value = "pg02",
  
  div(class="resultado-container",
      
      # título da seção
      
      div(class="resultado-header",
          h2("Proteção financeira para quem depende de você")
      ),
      
      fluidRow(
        
        column(
          width = 4,
          
          tags$img(
            src="pg03_familia.jpg",
            width="100%"
          )
        ),
        
        column(
          width = 8,
          
          div(class="card-resultado",
              
              h3("Capital segurado recomendado"),
              
              div(class="capital-valor",
                  textOutput("capital_recomendado")
              ),
              
              p(
                "Esse valor poderia substituir aproximadamente ",
                tags$b("20 anos"),
                " da sua renda atual."
              )
          )
        )
      ),
      
      
      # explicação
      
      div(class="card-explicacao",
          
          h3("Por que esse valor?"),
          
          tags$ul(class="lista-explicacao",
                  tags$li("sua renda mensal informada"),
                  tags$li("sua idade atual"),
                  tags$li("um horizonte de proteção familiar de longo prazo")
          ),
          
          p(
            "O objetivo é que sua família tenha tempo para se reorganizar ",
            "financeiramente sem perda brusca de ",
            tags$b("padrão de vida.")
          )
      ),
      
      
      # simulacoes
      
      div(class="simulacoes-container",
          
          h3("Simulações de custo do seguro"),
          
          p(
            "Para um capital segurado próximo de ",
            tags$b(textOutput("capital_recomendado_inline")),
            ", o custo estimado poderia variar aproximadamente entre:"
          ),
          
          
          fluidRow(
            
            column(
              4,
              div(class="card-plano",
                  h4("Plano Essencial"),
                  div(class="preco-plano","R$ 85 / mês")
              )
            ),
            
            column(
              4,
              div(class="card-plano",
                  h4("Plano Proteção Familiar"),
                  div(class="preco-plano","R$ 120 / mês")
              )
            ),
            
            column(
              4,
              div(class="card-plano",
                  h4("Plano Premium"),
                  div(class="preco-plano","R$ 165 / mês")
              )
            )
          )
      ),
      
      br(),
      
      div(style="text-align:center;",
          
          actionButton(
            "ver_simulacoes",
            "Ver simulações detalhadas",
            class="btn-navegacao"
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
        class = "btn-navegacao"
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