plot_kmeans = function(df, clusters, runs, standardize=T) {
  library(psych)
  library(ggplot2)
  
  #standardize
  if (standardize) df = std_df(df)
  
  #cluster
  tmp_k = kmeans(df, centers = clusters, nstart = 100)
  
  #factor
  tmp_f = fa(df, 2, rotate = "none")
  
  #collect data
  tmp_d = data.frame(matrix(ncol=0, nrow=nrow(df)))
  tmp_d$cluster = as.factor(tmp_k$cluster)
  tmp_d$fact_1 = as.numeric(tmp_f$scores[, 1])
  tmp_d$fact_2 = as.numeric(tmp_f$scores[, 2])
  tmp_d$label = rownames(df)
  
  #plot
  g = ggplot(tmp_d, aes(fact_1, fact_2, color = cluster)) + geom_point() + geom_text(aes(label = label), size = 3, vjust = 1, color = "black")
  return(g)
}