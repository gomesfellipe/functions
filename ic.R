# Fonte: http://www.leg.ufpr.br/Rpira/Rpira/node11.html

# Calcula o interalo de confianca para a média
ic.m <- function(x, conf = 0.95){
   n <- length(x)
   media <- mean(x)
   variancia <- var(x)
   quantis <- qt(c((1-conf)/2, 1 - (1-conf)/2), df = n-1)
   ic <- media + quantis * sqrt(variancia/n)
   return(ic)
