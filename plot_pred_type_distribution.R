plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$fit >= threshold & df$class == 1, "TP", v)
  v <- ifelse(df$fit >= threshold & df$class == 0, "FP", v)
  v <- ifelse(df$fit < threshold & df$class == 1, "FN", v)
  v <- ifelse(df$fit < threshold & df$class == 0, "TN", v)
  
  df$fit_type <- v
  
  ggplot(data=df, aes(x=class, y=fit)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=fit_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Limite em %.2f", threshold))+
    theme_bw()
}

# Ref: https://github.com/joyofdata/joyofdata-articles/blob/master/roc-auc/log_reg.R
