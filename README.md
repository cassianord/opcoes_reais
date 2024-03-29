# Cálculo de opções reais utilizando árvores binomiais

<br>

### **Setup**

Abaixo, carregamos as bibliotecas necessárias. Também definimos a opção
para não mostrar os resultados em notação científica.

``` r
library(dplyr)
library(tidyr)
options(scipen=999)
```

Além disso, utilizaremos a função `bintree.R` para a obter a árvore
binomial básica do ativo:

``` r
source("bintree.R")
```

<br>

## **1ª Etapa: calcular a árvore binomial inicial (sem opções)**

**Estabelecendo os parâmetros iniciais**

Aqui definimos os valores que serão utilizados para calcular a árvore
binomial. O valor do ativo consistirá no Valor Presente Líquido (VPL) do
projeto, e os demais parâmetros seguem como abaixo:

``` r
VPL <- 112667.9 # Valor Presente Líquido
volat <- .093 # volatilidade
tempo <- 5 # vida do projeto, em anos
rf <- .085 # taxa livre de risco
passos <- 10 # número de passos da árvore binomial
```

**Calculando a árvore binomial inicial**

Em seguida, vamos aplicar a função `bintree.R` para cálculo da árvore
binomial, considerando:

-   S0: valor do ativo no tempo t=0
-   vol: volatilidade
-   dT: tempo até a maturidade (no caso de opções reais, vida do
    projeto)
-   r: taxa livre de risco
-   n_steps: número de passos da árvore

``` r
arvore <- bintree(S0=VPL, vol=volat, dT=tempo, r=rf, n_steps=passos)
```

Posteriormente, estruturamos os valores com indexadores de linha e
coluna, para construir uma matriz:

``` r
arvore2 <- arvore %>% group_by(step) %>% 
              mutate(row = row_number(step)) %>%
              mutate(col = step + 1) %>%
              ungroup %>%
              select(row, col, S)

matriz_valores <- t(as.matrix(spread(arvore2, row, S)[,-1]))
```

<br>

## **2ª Etapa: calculando as árvores com opções reais**

**Parâmetros gerais para as opções reais**

Os parâmetros abaixos são utilizados em todas as aplicações posteriores.
São parâmetros das opções já utilizados na função anterior, mas aqui
vamos defini-los de modo explícito, pois vamos utilizá-los nos cálculos
a seguir.

``` r
dt <- tempo / passos # tamanho de cada passo = tempo dividido pelo número de passos
u <- exp((volat) * sqrt(dt)) # Fator de multiplicação de alta
d <- 1/u # Fator de multiplicação de queda
p <- (exp(rf*dt)-d)/(u-d) # Probabilidade de u

l <- nrow(matriz_valores) # Número de linhas da matriz da árvore binomial
c <- ncol(matriz_valores) # Número de colunas da matriz da árvore binomial
```

<br>

### **Opção de Expansão**

No caso de uma expansão, é possível realizar um investimento adicional
para elevar o valor do projeto. Precisamos, então, definir esses
parâmetros. Supondo uma expansão que custe R$40.000,00, e que aumente o
VPL em 45%, teremos então:

``` r
exp_custo <- 40000 # Custo da expansão
exp_fator <- 1.45 # Fator de expansão
```

Então criamos primeiramente uma matriz intermediária, contendo os
valores com a expansão, quando ela se revelar favorável, ou os valores
originais, caso contrário.

``` r
matriz_int_exp <- matrix(NaN, nrow = l, ncol = c) # Criar uma matriz de igual dimensão à original
for(i in 1:l) {
  for(j in 1:c) {
    
    matriz_int_exp[i,j] <- max(matriz_valores[i,j], (matriz_valores[i,j]*exp_fator)-exp_custo)
    # Em cada nó da árvore (célula da matriz), escolher o máximo entre o valor original e o valor com expansão
  }
}
```

Por fim, calculamos a árvore com a opção de expansão:

``` r
matriz_exp <- matrix(NaN, nrow = l, ncol = c) # Criar uma matriz de igual dimensão à original
matriz_exp[, c] <- matriz_int_exp[, c] # Adicionar valores com expansão à última coluna (fim da árvore)

for(j in (c-1):1) {
  
  for(i in 1:l) {
    
    matriz_exp[i,j] <- ifelse(is.na(matriz_int_exp[i,j]), NA,
                        max( (matriz_exp[i,j+1]*p + 
                               matriz_exp[i+1,j+1]*(1-p)) * exp(-rf*dt),
                                    
                                   matriz_int_exp[i,j]))
    # Seguindo a ordem reversa pelas colunas, calcular o valor de cada célula (desde que não NA originalmente)
    # como o valor presente da média ponderada dos nós que lhe sucedem, e depois escolher entre o máximo
    # deste valor e o valor da matriz intermediária
  }
}
```

<br>

### **Opção de Contração**

No caso de uma contração, é possível reduzir a escala do projeto,
diminuindo seu valor, mas obtendo um valor residual com a venda de parte
dos ativos. Precisamos, então, definir esses parâmetros. Supondo uma
contração gere um valor residual de R$50.000,00, e que reduza o VPL em
35%, teremos então:

``` r
con_res <- 50000 # Receita com a contração
con_fator <- .65 # Fator da contração
```

Criamos, então, a matriz intermediária, contendo os valores com a
contração, quando ela se revelar favorável, ou os valores originais,
caso contrário.

``` r
matriz_int_con <- matrix(NaN, nrow = l, ncol = c) # Criar uma matriz de igual dimensão à original
for(i in 1:l) {
  for(j in 1:c) {
    
    matriz_int_con[i,j] <- max(matriz_valores[i,j], (matriz_valores[i,j]*con_fator)+con_res)
    # Em cada nó da árvore (célula da matriz), escolher o máximo entre o valor original e o valor com contração
  }
}
```

Por fim, calculamos a árvore com a opção de contração:

``` r
matriz_con <- matrix(NaN, nrow = l, ncol = c) # Criar uma matriz de igual dimensão à original
matriz_con[, c] <- matriz_int_con[, c] # Adicionar valores com contração à última coluna (fim da árvore)

for(j in (c-1):1) {
  
  for(i in 1:l) {
    
    matriz_con[i,j] <- ifelse(is.na(matriz_int_con[i,j]), NA,
                                max( (matriz_con[i,j+1]*p + 
                                        matriz_con[i+1,j+1]*(1-p)) * exp(-rf*dt),
                                     
                                     matriz_int_con[i,j]))
    # Seguindo a ordem reversa pelas colunas, calcular o valor de cada célula (desde que não NA originalmente)
    # como o valor presente da média ponderada dos nós que lhe sucedem, e depois escolher entre o máximo
    # deste valor e o valor da matriz intermediária
  }
}
```

<br>

### **Opção de Abandono**

Caso o cenário para o projeto seja muito desfavorável, é possível
abandoná-lo. Isso cessa os fluxos de caixa futuros, mas possibilita
obter um valor residual com a venda dos ativos. Aqui o parâmetro a ser
definido é justamente esse valor residual. Supondo que os ativos possam
ser vendidos a qualquer momento por R$90.000,00:

``` r
aban_res <- 90000 # Valor residual em caso de abandono
```

Na sequência, criamos a matriz intermediária, contendo os valores com o
abandono, quando ele se revelar favorável, ou os valores originais, caso
contrário.

``` r
matriz_int_aban <- matrix(NaN, nrow = l, ncol = c) # Criar uma matriz de igual dimensão à original
for(i in 1:l) {
  for(j in 1:c) {
    
    matriz_int_aban[i,j] <- max(matriz_valores[i,j], aban_res)
    # Em cada nó da árvore (célula da matriz), escolher o máximo entre o valor original e o valor com abandono
  }
}
```

Por fim, calculamos a árvore com a opção de abandono:

``` r
matriz_aban <- matrix(NaN, nrow = l, ncol = c) # Criar uma matriz de igual dimensão à original
matriz_aban[, c] <- matriz_int_aban[, c] # Adicionar valores com abandono à última coluna (fim da árvore)

for(j in (c-1):1) {
  
  for(i in 1:l) {
    
    matriz_aban[i,j] <- ifelse(is.na(matriz_int_aban[i,j]), NA,
                                max( (matriz_aban[i,j+1]*p + 
                                        matriz_aban[i+1,j+1]*(1-p)) * exp(-rf*dt),
                                     
                                     matriz_int_aban[i,j]))
    # Seguindo a ordem reversa pelas colunas, calcular o valor de cada célula (desde que não NA originalmente)
    # como o valor presente da média ponderada dos nós que lhe sucedem, e depois escolher entre o máximo
    # deste valor e o valor da matriz intermediária
  }
}
```
