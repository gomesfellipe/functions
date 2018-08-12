# Aplica as interacoes de algum calculo dois a dois em um data.table()
# Referencia: http://analysingstuffs.xyz/2017/12/01/visualizing-the-correlations-between-categorical-variables-with-r-a-cramers-v-heatmap/

interaction_all <- function(df, func){
  library(data.table) # data mgmt
  library(gtools) # combination
  cat_var_grid <- data.table(combinations(n = ncol(df), r = 2, v = names(df), repeats.allowed = FALSE))
  
  do.call(rbind,
          apply(cat_var_grid, 1, function(x){
            tmp <- as.character(x)
            vec1 <- unlist(df[,tmp[1], with = FALSE])
            vec2 <- unlist(df[,tmp[2], with = FALSE])
            
            data.table(
              variable_x = tmp[1],
              variable_y = tmp[2],
              # chi2 = chisq.test(x = vec1, vec2, correct=FALSE)$p.value,
              v_cramer = func(x = vec1, y = vec2)
            )
          }))
  
}
