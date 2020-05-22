# Evalue function -----------------------------------------------------------------------------

evalue_model_umbal <- function(TP, TN, FP, FN){
  
  # Precision = minimizing false positives is the focus
  precision = TP / (TP + FP)
  
  # Recall: minimizing false negatives
  recall = TP / (TP + FN)
  
  # F1: combine both precision and recall into a single measure that captures both properties
  f1 = (2 * precision * recall) / (precision + recall)
  
  return(
    glue::glue("
               Precisao: {precision}
               Recall: {recall}
               F1: {f1}
               ")
  )
}

# Task 1 --------------------------------------------------------------------------------------

# modelo RFE down-sample

##           Reference
## Prediction negative positive
##   negative      224       10
##   positive      161       25

evalue(
  TP = 25,
  TN = 224,
  FP = 161,
  FN = 10
)

# Reference
# Prediction negative positive
# negative        0        7
# positive        0       28

evalue(
  TP = 28,
  TN = 0,
  FP = 0,
  FN = 7
)


