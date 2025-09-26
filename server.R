# server.R - Lógica do Servidor (O "Cérebro" do Aplicativo)

server <- function(input, output, session) {
  
  # A lista de opções para tábua - depende do gênero
  output$tabua_ui <- renderUI({
    
    # Define as opções de tábuas para cada gênero
    tabuas_masc <- c("AT 2000 Masculina", "SSA Masculina (Geracional)")
    tabuas_fem <- c("AT 2000 Feminina")
    opcoes <- switch(input$genero, "Masculino" = tabuas_masc, "Feminino" = tabuas_fem)
    
    # Cria a caixa de seleção
    selectInput(inputId = "tabua", label = "Selecione a Tábua:", choices = opcoes, selected = opcoes[1])
  })
  
  # Verificar se é uma tábua geracional
  eh_tabua_geracional <- reactive({
    
    # Garante que o código só rode depois que o usuário selecionar uma tábua
    req(input$tabua)
    
    # Verifica se a palavra "Geracional"
    grepl("Geracional", input$tabua)
  })
  
  # Carregar e preparar a tábua
  dados_tabua <- reactive({
    
    # Garante que uma tábua foi selecionada
    req(input$tabua)
    
    # Decide qual função de carregamento usar com base no tipo da tábua
    if (eh_tabua_geracional()) {
      
      # Se for geracional
      dados <- carregar_tabua_geracional(input$tabua)
      
    } else {
      
      # Se for estática
      dados <- carregar_tabua_estatica(input$tabua)
    }
    
    # Arredonda os números da tabela para uma melhor visualização
    
    # Arredondamento para tábuas estáticas
    if (nrow(dados) > 0 && !eh_tabua_geracional()) {
      dados[, sapply(dados, is.numeric)] <- round(dados[, sapply(dados, is.numeric)], 6)
      dados$lx <- round(dados$lx, 0)}
    
    # Arredondamento para tábuas geracionais
    else if (nrow(dados) > 0 && eh_tabua_geracional()) {
      numeric_cols <- sapply(dados, is.numeric)
      dados[, numeric_cols] <- round(dados[, numeric_cols], 6)
      if ("lx" %in% colnames(dados)) {
        dados$lx <- round(dados$lx, 0)}}
    
    # Retorna o tabela carregada
    return(dados)
  })
  
  # Exibição da tábua
  output$tabela_exposta <- DT::renderDataTable({
    
    # Cria uma tabela interativa
    DT::datatable(
      dados_tabua(),
      options = list(pageLength = 15, scrollY = "450px", scrollCollapse = TRUE),
      rownames = FALSE)})
  
  # Cálculo
  resultado_calculado <- eventReactive(input$calcular, {
    
    # Garante que os inputs essenciais não estão vazios antes de prosseguir
    req(input$idade, input$taxa_de_juros, input$tempo_contribuicao)
    
    # Pega os dados da tábua já carregados
    dados <- dados_tabua()
    req(nrow(dados) > 0)
    
    # Armazena os inputs do usuário em variáveis
    idade_atual <- input$idade
    tempo_contri <- input$tempo_contribuicao
    
    # Prepara as variáveis para as funções de cálculo
    taxa_juros_decimal <- input$taxa_de_juros / 100
    inicio_cobertura <- tempo_contri
    
    # Para a tábua geracional
    if (eh_tabua_geracional()) {
      
      # Define o período de cálculo baseado nas janelas da tábua
      idades_disponiveis <- unique(dados$x)
      janelas_disponiveis <- unique(dados$janela)
      final_cobertura <- max(janelas_disponiveis, na.rm = TRUE)
      
      # Ajusta o período de cobertura para não ultrapassar a idade máxima da tábua
      idade_maxima <- max(idades_disponiveis, na.rm = TRUE)
      final_cobertura <- min(final_cobertura, idade_maxima - (idade_atual + tempo_contri))
      
      # Tenta executar o cálculo
      tryCatch({
        
        # Chama a função específica para anuidades geracionais
        resultado <- ax_geracional(
          inicio_cobertura = inicio_cobertura,
          final_cobertura = final_cobertura,
          i = taxa_juros_decimal,
          x = idade_atual,
          tabua_x = dados
        )
        
        # Retorna o resultado numérico
        return(resultado)
        
      }, 
      
      # Mensagem de erro
      error = function(e) {
        return(paste("Erro no cálculo (tábua geracional):", e$message))
      })} 
    
    # Tábua estática
    else {
      
      # Define o período de cálculo para a tábua estática
      final_cobertura <- (nrow(dados) - 1) - (idade_atual + tempo_contri)
      
      # Tenta executar o cálculo
      tryCatch({
        
        # Chama a função padrão para anuidades estáticas
        resultado <- ax(
          inicio_cobertura = inicio_cobertura, 
          final_cobertura = final_cobertura, 
          i = taxa_juros_decimal, 
          x = idade_atual, 
          dados = dados
        )
        
        # Retorna o resultado numérico
        return(resultado)},
      
      # Mensagem de erro
      error = function(e) {
        return(paste("Erro no cálculo (tábua estática):", e$message))
      })
    }
  })
  
  # Exibição do resultado
  output$resultados_calculos <- renderPrint({
    
    # Caixa de cálculo
    resultado_final <- resultado_calculado()
    cat("Cálculo Atuarial (ax):\n")
    cat("Valor da anuidade: ", round(resultado_final, 5), "\n")
    cat("\nParâmetros utilizados:\n")
    cat("- Idade atual:", input$idade, "anos\n")
    cat("- Idade de gozo do benefício:", input$idade + input$tempo_contribuicao, "anos\n")
    cat("- Taxa de juros:", input$taxa_de_juros, "%\n")
    cat("- Tábua:", input$tabua, "\n")
    cat("- Tipo de tábua:", ifelse(eh_tabua_geracional(), "Geracional", "Estática"), "\n")
    
  })
  
}