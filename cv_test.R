
# Cramer's V calculation --------------------------------------------------

# Function to compute Cramer's V
# Font:
# https://www.r-bloggers.com/example-8-39-calculating-cramers-v/
# http://analysingstuffs.xyz/2017/12/01/visualizing-the-correlations-between-categorical-variables-with-r-a-cramers-v-heatmap/

cv_test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x)[1] * (min(length(unique(x))[1],length(unique(y))[1]) - 1)))
  return(as.numeric(CV))
}
