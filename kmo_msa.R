# Teste KMO (Kaiser-Meyer-Olkin) - avalia a adequação do tamanho amostra. 
#Varia entre 0 e 1, onde: zero indica inadequado para análise fatorial, aceitável se for maior que 0.5, recomendado acima de 0.8.

# MSA: Measure of Sampling Adequacy - 
# Similar ao KMO essa medida verifica se existe uma estrutura fatorial nos dados. 
# Calculada separadamente para cada variável, pois o objetivo é verificar se
# uma dada variável pode ser explicada pelas as demais. 
#Valores baixos indicam que a variável analisada por ser retirada da análise sem maiores prejuízos.
kmo_msa <- function(x)
{
  x <- subset(x, complete.cases(x)) # Omitindo valores faltantes
  r <- cor(x) # Matrix de correlacao
  r2 <- r^2 # coeficiente de correlação ao quadrado
  i <- solve(r) # inverso da matriz de correlacoes
  d <- diag(i) # Elementos da diagonal da matriz inversa
  p2 <- (-i/sqrt(outer(d, d)))^2 # coeficiente de correlação parcial ao quadrado
  diag(r2) <- diag(p2) <- 0 # Removendo elementos da diagonal
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}
