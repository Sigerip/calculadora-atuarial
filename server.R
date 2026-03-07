server <- function(input, output, session) {
  
  capital_recomendado <- eventReactive(input$calcular, {
    
    req(input$renda, input$idade)
    
    renda_anual <- input$renda * 12
    idade <- input$idade
    
    k <- if (idade < 35) {
      20
    } else if (idade < 45) {
      18
    } else if (idade < 55) {
      15
    } else {
      10
    }
    
    capital <- renda_anual * k
    
    return(capital)
    
  })

  # Botões para navegação entre as páginas --------------------------------
  observeEvent(input$calcular, {

    updateTabsetPanel(
      session,
      "paginas",
      selected = "pg02"
    )
  })

  observeEvent(input$ver_simulacoes, {

    updateTabsetPanel(
      session,
      "paginas",
      selected = "pg03"
    )

  })

  observeEvent(input$avaliar, {

    updateTabsetPanel(
      session,
      "paginas",
      selected = "pg04"
    )

  })

  output$capital_texto <- renderText({
    
    req(capital_recomendado())
    
    paste0(
      "Capital segurado recomendado: R$ ",
      format(round(capital_recomendado(), 0),
             big.mark = ".",
             decimal.mark = ",")
    )
    
  })
  
  
  # ------------------------------------------------
  # Simulações de preço
  # ------------------------------------------------
  
  output$tabela_simulacoes <- renderTable({
    
    req(capital_recomendado())
    
    capital <- capital_recomendado()
    
    data.frame(
      Plano = c("Essencial", "Proteção Familiar", "Premium"),
      Capital_Segurado = c(
        capital,
        capital * 1.2,
        capital * 1.5
      ),
      Premio_Mensal = c(
        85,
        120,
        165
      )
    )
    
  })
  
  
  # ------------------------------------------------
  # Pesquisa de satisfação
  # ------------------------------------------------
  
  observeEvent(input$enviar, {
    
    output$agradecimento <- renderText({
      
      paste0(
        "Obrigado pela avaliação, ",
        input$nome,
        "! Sua nota foi ",
        input$nota,
        "."
      )
      
    })
    
  })
  
}