# source: http://ggobi.github.io/ggally/#canonical_correlation_analysis

# Plot of correlations between the explanatory variables and the variable response of the model

lm_with_cor <- function(data, mapping, ..., method = "pearson") {
  x <- eval(mapping$x, data)
  y <- eval(mapping$y, data)
  cor <- cor(x, y, method = method)
  ggally_smooth_lm(data, mapping, ...) +
    ggplot2::geom_label(
      data = data.frame(
        x = min(x, na.rm = TRUE),
        y = max(y, na.rm = TRUE),
        lab = round(cor, digits = 3)
      ),
      mapping = ggplot2::aes(x = x, y = y, label = lab),
      hjust = 0, vjust = 1,
      size = 5, fontface = "bold",
      inherit.aes = FALSE # do not inherit anything from the ...
    )
}


#Example:

## Data:
data(psychademic)
psych_variables <- attr(psychademic, "psychology")

## Example:
ggduo(
  psychademic, rev(psych_variables), academic_variables,
  mapping = aes(color = sex),
  types = list(continuous = wrap(lm_with_cor, alpha = 0.25)),
  showStrips = FALSE,
  title = "Between Academic and Psychological Variable Correlation",
  xlab = "Psychological",
  ylab = "Academic",
  legend = c(5,2)
) +
  theme(legend.position = "bottom")