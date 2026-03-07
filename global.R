# ------------------------------------------------
# Bibliotecas utilizadas no aplicativo
# ------------------------------------------------

library(dplyr)


# ------------------------------------------------
# Parâmetros globais do simulador
# ------------------------------------------------

# taxa de juros real hipotética utilizada nas simulações
taxa_juros <- 0.03


# ------------------------------------------------
# Função para calcular multiplicador por idade
# ------------------------------------------------

multiplicador_idade <- function(idade) {
  
  if (idade < 35) {
    return(20)
  } 
  
  if (idade < 45) {
    return(18)
  } 
  
  if (idade < 55) {
    return(15)
  } 
  
  return(10)
  
}


# ------------------------------------------------
# Função para capital segurado recomendado
# ------------------------------------------------

capital_recomendado <- function(renda_mensal, idade) {
  
  renda_anual <- renda_mensal * 12
  
  k <- multiplicador_idade(idade)
  
  capital <- renda_anual * k
  
  return(capital)
  
}


# ------------------------------------------------
# Função para gerar simulações de prêmio
# ------------------------------------------------

simular_planos <- function(capital) {
  
  data.frame(
    
    Plano = c(
      "Essencial",
      "Proteção Familiar",
      "Premium"
    ),
    
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
  
}