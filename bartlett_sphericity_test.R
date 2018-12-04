#Teste de Bartlett - a hipótese nula da matriz de correlação ser uma matriz identidade ( $| R | = 1$ ), 
#isto é, avalia se os componentes fora da diagonal principal são zero. 
#O resultado significativo indica que existem algumas relações entre as variáveis.

bartlett_sphericity_test <- function(x)
{
  method <- "Teste de esfericidade de Bartlett"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) # Omitindo valores faltantes
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "X-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}
