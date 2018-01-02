#Different tests to evaluate the normality of the data
normalidade<-function(x){
  t1 <- ks.test(x, "pnorm",mean(x), sd(x)) # KS  
  t2 <- lillie.test(x) # Lilliefors
  t3 <- cvm.test(x) # Cramér-von Mises
  t4 <- shapiro.test(x) # Shapiro-Wilk 
  t5 <- sf.test(x) # Shapiro-Francia
  t6 <- ad.test(x) # Anderson-Darling
  t7<-pearson.test(x) # Pearson Test of Normality
  
  testes <- c(t1$method, t2$method, t3$method, t4$method, t5$method,t6$method,t7$method)
  valorp <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value, t5$p.value,t6$p.value,t7$p.value)
  
  resultados <- cbind(valorp)
  rownames(resultados) <- testes
  print(resultados, digits = 4)
  
}