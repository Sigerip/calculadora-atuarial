# global.R - Variáveis e Configurações Globais

# Carregar bibliotecas necessárias
library(shiny)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(tidyr)
library(dplyr)

# Definir variáveis globais
app_title <- "Calculadora Atuarial"

# Função para carregar e processar os dados das tábuas
carregar_tabua_estatica <- function(nome_tabua) {
  
  # Mapear nomes das tábuas para arquivos CSV
  arquivo <- switch(nome_tabua,
                    "AT 2000 Masculina" = "dados/at_2000_masc.csv",
                    "AT 2000 Feminina"  = "dados/at_2000_fem.csv",
                    NULL)

  # Ler o arquivo e calcula a tábua
  tryCatch({dados_raw <- read.csv(arquivo, header = FALSE, sep = ",", dec = ".", stringsAsFactors = FALSE)
    
    # Probabolidade de morte
    qx <- as.numeric(dados_raw[, 1])
    
    # Idades
    n <- length(qx)
    x <- 0:(n - 1)
    
    # Probabolidade de sobrevivência
    px <- 1 - qx
    
    # Raiz
    radix <- 100000
    
    # Cálculo de lx
    lx <- numeric(n)
    lx[1] <- radix
    for (i in 2:n) {
      lx[i] <- lx[i-1] * px[i-1]
    }
    
    # Cálculo de dx
    dx <- lx * qx
    
    # Cálculo de Lx
    Lx <- numeric(n)
    for (i in 1:(n-1)) {
      Lx[i] <- (lx[i] + lx[i+1]) / 2
    }
    Lx[n] <- lx[n] / qx[n]
    
    # Cálculo Tx
    Tx <- numeric(n)
    Tx[n] <- Lx[n]
    for (i in (n-1):1) {
      Tx[i] <- Tx[i+1] + Lx[i]
    }
    
    # Cálculo de ex
    ex <- Tx / lx
    
    # Monta a tábua de vida
    tabua_de_vida <- data.frame(
      Idade = x, 
      qx = qx, 
      px = px, 
      lx = lx, 
      dx = dx, 
      Lx = Lx, 
      Tx = Tx, 
      ex = ex
    )
    
    return(tabua_de_vida)
    
  },
  
  # Retorna um data frame vazio casa não encontre o arquivo
  error = function(e) {
    warning("Erro ao ler ou processar o arquivo ", arquivo, ": ", e$message)
    return(data.frame())
  })
}


# Função para carregar e processar os dados das tábuas geracionais
carregar_tabua_geracional <- function(nome_tabua) {
  
  # Mapear nomes das tábuas para arquivos CSV
  arquivo <- switch(nome_tabua,
                    "SSA Masculina (Geracional)" = "dados/SSA_masc.csv",
                    NULL)
  
  # Lê o arquivo e cálcula a tábua
  tryCatch({
    
    # Ler o arquivo
    df <- read.csv(arquivo)
    
    # Faz o melt da tábua
    df_longo <- pivot_longer(df, cols = -x, names_to = "janela", values_to = "qx")
    
    # Converter as colunas para o tipo numérico
    df_longo$qx <- as.numeric(df_longo$qx)
    df_longo$x <- as.numeric(df_longo$x)
    
    # Retira o "X" da coluna "janela" e formata para numérico
    df_longo$janela <- gsub("X", "", df_longo$janela)
    df_longo$janela <- as.numeric(df_longo$janela)
    
    # Organizar com base na coluna "janela"
    df_longo <- arrange(df_longo, janela)
    
    # Raiz da tábua
    radix <- 100000
    
    # Calculo da tábua de vida
    tabuas_de_vida <- df_longo %>%
      
      # Agrupa por janela
      group_by(janela) %>%
      
      # Calcula a tábua de forma vetorizada
      mutate(
        # px
        px = 1 - qx,
        
        # lx 
        lx = radix * cumprod(c(1, head(px, -1))),
        
        # dx
        dx = lx * qx,
        
        # Lx
        Lx = (lx + lead(lx)) / 2,
        Lx = ifelse(is.na(Lx), lx / 2, Lx),
        
        # Tx
        Tx = rev(cumsum(rev(Lx))),
        
        # ex
        ex = Tx / lx) %>%
      
      # Desagrupa ao final
      ungroup()
    
    return(tabuas_de_vida)
    
  },
  
  # Retorna um data frame vazio case não consigo ler o arquivo
  error = function(e) {
    warning("Erro ao ler ou processar o arquivo ", arquivo, ": ", e$message)
    return(data.frame())
  })
}

# Função para anuidade de uma cabeça
ax <- function(inicio_cobertura, final_cobertura, i, x, dados) {
    
    # Função de desconto
    v <- 1 / (1 + i)

    # Vetor para armazenar parcelas da anuidade
    vetor_soma <- numeric()

    # Loop para encontrar a parcela em cada tempo t
    for (t in inicio_cobertura:final_cobertura) {
        
        # Verificar se a idade x+t está dentro dos limites da tábua
        if ((x + t + 1) > nrow(dados)) {
          break
        }
        
        # Fator de desconto financeiro no tempo t
        v_t <- v^t
        
        # Dividor da probabilidade
        lx_x <- dados$lx[x]

        # Numerador da probabilidae
        lx_x_t <- dados$lx[x + t]
        
        # Probabilidade de sobrevivência
        t_p_x <- lx_x_t / lx_x

        # Parcela da anuidade no tempo t
        a_x_t <- v_t * t_p_x

        # Adiciona ao vetor para soma
        vetor_soma <- c(vetor_soma, a_x_t)
    }

    # Retorna o valor da anuidade
    resultado <- sum(vetor_soma, na.rm = TRUE)
    
    return(resultado)
}

# Função para anuidade de uma cabeça
ax_geracional <- function(inicio_cobertura, final_cobertura, i, x, tabua_x) {
  
  # Função de desconto
  v <- 1 / (1 + i)
  
  # Vetor para armazenar parcelas da da anuidade
  vetor_soma <- numeric()
  
  # Loop para encontrar a parcela em cada tempo t
  for (t in inicio_cobertura:final_cobertura) {
    
    tabua <- filter(tabua_x, janela == t)
    
    # Fator de desconto financeiro no tempo t
    v_t <- v^t
    
    # Probabilidade de sobrevivência entre x e x + t para o segurado x
    t_p_x <- (tabua$lx[tabua$x == (x + t)]) / (tabua$lx[tabua$x == x])
    
    # Parcela da anuidade no tempo t
    a_x <- v_t * t_p_x
    
    # Adiciona ao vetor para soma
    vetor_soma <- c(vetor_soma, a_x)}
  
  # Retorna o valor da unidade
  return(sum(vetor_soma, na.rm = TRUE))
  
}