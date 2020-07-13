# Performance metrics for binary regressions
# Only working with 2x2 matrix

# Notes:

# TP = True Positives
# TN = True Negatives
# FP = False Positives
# FN = False Negatives

# ACC = Accuracy 
# TPR = Sensitivity or true positive rate
# TNR = Specifity or true negative rate
# AUC = Area under the curve
# CSI = Jaccard index or critical sucess index
# SSI = Sokal and Sneath index
# FAITH = Faith index
# PDIF = Pattern Difference
# GS = Gilbert Skill Score

# Creating the functions
ACC <- function(x){
  if (dim(x)[1] != dim(x)[2] && dim(x)[1]!=2){
    print("Error. A quadratic matrix is required")
  }
  else{
    TP = x[1,1]
    TN = x[2,2]
    FP = x[1,2]
    FN = x[2,1]
    ACC = (TP + TN) / (TP + TN + FP + FN)
  }
  return(ACC)
}

TPR <- function(x){
  if (dim(x)[1] != dim(x)[2] && dim(x)[1]!=2){
    print("Error. A quadratic matrix is required")
  }
  else{
    TP = x[1,1]
    TN = x[2,2]
    FP = x[1,2]
    FN = x[2,1]
    TPR = TP / (TP + FN)
  }
  return(TPR)
}

TNR <- function(x){
  if (dim(x)[1] != dim(x)[2] && dim(x)[1]!=2){
    print("Error. A quadratic matrix is required")
  }
  else{
    TP = x[1,1]
    TN = x[2,2]
    FP = x[1,2]
    FN = x[2,1]
    TNR = TN / (TN + FP)
  }
  return(TNR)
}

CSI <- function(x){
  if (dim(x)[1] != dim(x)[2] && dim(x)[1]!=2){
    print("Error. A quadratic matrix is required")
  }
  else{
    TP = x[1,1]
    TN = x[2,2]
    FP = x[1,2]
    FN = x[2,1]
    CSI = TP / (TP + FP + FN)
  }
  return(CSI)
}

SSI <- function(x){
  if (dim(x)[1] != dim(x)[2] && dim(x)[1]!=2){
    print("Error. A quadratic matrix is required")
  }
  else{
    TP = x[1,1]
    TN = x[2,2]
    FP = x[1,2]
    FN = x[2,1]
    SSI = TP / (TP + (2 * FP) + (2 * FN))
  }
  return(SSI)
}

FAITH <- function(x){
  if (dim(x)[1] != dim(x)[2] && dim(x)[1]!=2){
    print("Error. A quadratic matrix is required")
  }
  else{
    TP = x[1,1]
    TN = x[2,2]
    FP = x[1,2]
    FN = x[2,1]
    FAITH = (TP + (0.5 * TN) ) / (TP + FP + FN + TN)
  }
  return(FAITH)
}

PDIF <- function(x){
  if (dim(x)[1] != dim(x)[2] && dim(x)[1]!=2){
    print("Error. A quadratic matrix is required")
  }
  else{
    TP = x[1,1]
    TN = x[2,2]
    FP = x[1,2]
    FN = x[2,1]
    PDIF = (4 * FP * FN) / (TP + FP + FN + TN)^2
  }
  return(PDIF)
}

GS <- function(x){
  if (dim(x)[1] != dim(x)[2] && dim(x)[1]!=2){
    print("Error. A quadratic matrix is required")
  }
  else{
    TP = x[1,1]
    TN = x[2,2]
    FP = x[1,2]
    FN = x[2,1]
    GS = ((TP * TN) - (FP * FN)) / (((FN + FP)*(TP + FP + FN + TN)) + ((TP*TN)-(FP*FN)))
  }
  return(GS)
}

P <- function(x){
  if (dim(x)[1] != dim(x)[2] && dim(x)[1]!=2){
    print("Error. A quadratic matrix is required")
  }
  else{
    TP = x[1,1]
    TN = x[2,2]
    FP = x[1,2]
    FN = x[2,1]
    P = TP / (TP + FP)
  }
  return(P)
}

R <- function(x){
  if (dim(x)[1] != dim(x)[2] && dim(x)[1]!=2){
    print("Error. A quadratic matrix is required")
  }
  else{
    TP = x[1,1]
    TN = x[2,2]
    FP = x[1,2]
    FN = x[2,1]
    R = TP / (TP + FN)
  }
  return(R)
}

F1 <- function(x){
  if (dim(x)[1] != dim(x)[2] && dim(x)[1]!=2){
    print("Error. A quadratic matrix is required")
  }
  else{
    TP = x[1,1]
    TN = x[2,2]
    FP = x[1,2]
    FN = x[2,1]
    F1 = (2 * (TP / (TP + FN)) * (TP / (TP + FP)) ) / ((TP / (TP + FN)) + (TP / (TP + FP)))
  }
  return(F1)
}

summary.measures <- function(x){
  acc <- ACC(x)
  tpr <- TPR(x)
  tnr <- TNR(x)
  csi <- CSI(x)
  ssi <- SSI(x)
  faith <- FAITH(x)
  pdif <- PDIF(x)
  gs <- GS(x)
  p <- P(x)
  r <- R(x)
  f1 <- F1(x)
  cat(paste("The model performance measures are:",
        paste("Accuracy (ACC):", round(acc,4)),
        paste("True Positive Rate (TPR):",round(tpr,4)),
        paste("True Negative Rate (TNR):",round(tnr,4)),
        paste("Jaccard Index or Critical Sucess Index (CSI):",round(csi,4)),
        paste("Sokal and Sneath Index (SSI):",round(ssi,4)),
        paste("Faith Index (FAITH):",round(faith,4)),
        paste("Pattern Difference (PDIF):",round(pdif,4)),
        paste("Gilbert Skill Score (GS):",round(gs,4)),
        paste("Precision (P):",round(p,4)),
        paste("Recall (R):",round(r,4)),
        paste("F1 Score (F1):",round(f1,4)),
        sep = '\n'))
  cat('\n\n\n')
  return(data.frame(ACC = acc,
                    TPR = tpr,
                    TNR = tnr,
                    CSI = csi,
                    SSI = ssi,
                    FAITH = faith,
                    PDIF = pdif,
                    GS = gs,
                    P = p,
                    R = r,
                    F1 = f1))
}
