#correlations for ggpairs
my_fn <- function(data, mapping, method="lm", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}