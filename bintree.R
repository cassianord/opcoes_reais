# Retorna um data frame contendo a árvore binomial
bintree <- function(S0, vol, dT, r, n_steps)
{
  # Número de nós na árvore
  N_nodes <- (n_steps+1)*(1 + (n_steps+1))/2 
  
  # Diferença de tempo entre os passos
  dT_step <- dT/n_steps
  
  # Fator de desconto para um passo
  D_step <- exp(-r*dT_step) 
  
  # Fator de alta
  u <- exp(vol*dT_step^0.5)
  
  # Fator de queda
  d <- 1/u 
  
  # Probabilidade de alta neutra ao risco
  p <- (exp((r)*dT_step) - d)/(u - d)
  
  # criar um data frame para armazenar os nós da árvore mapeandos os nós em linha
  df <- data.frame(i_node = 1:N_nodes, step=NA, N_u = NA, S = NA)

  for (i in n_steps:0) # indexador do último passo para o primeiro
  {
    for (j in 0:i) # para cada passo indexado acima, criar uma série de 0 até o valor do passo
    {
      i_node <- (i+1)*(1+(i+1))/2-j  # definir id do nó
      df$step[i_node] <- i  # passo
      df$N_u[i_node] <- j  # número de multiplicação u
      df$S[i_node] <- S0 * d^(i-j) * u^j # valor do ativo
      
   }
  }
  return(df)
}





