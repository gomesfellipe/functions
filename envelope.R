#Envelope
envelope<-function(x){
  n <- length(x)
  nsim <- 100 # Número de simulações
  conf <- 0.95 # Coef. de confiança
  # Dados simulados ~ normal
  dadossim <- matrix(rnorm(n*nsim, mean = mean(x), sd = sd(x)), nrow = n)
  dadossim <- apply(dadossim,2,sort)
  # Limites da banda e média
  infsup<-apply(dadossim,1,quantile, probs = c((1 - conf) / 2,(1 + conf) / 2))
  xbsim <- rowMeans(dadossim)
  faixay <- range(x, dadossim)
  qq0 <- qqnorm(x, main = "", xlab = "Quantis teóricos N(0,1)", pch = 20, ylim = faixay)
  eixox <- sort(qq0$x)
  lines(eixox, xbsim)
  lines(eixox, infsup[1,], col = "red")
  lines(eixox, infsup[2,], col = "red")
}