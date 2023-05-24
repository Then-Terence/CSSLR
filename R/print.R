
#' @export
print.csslr.model.analysis.lr.calib <- function(x, ...) {
  listNames <- names(x)

  df <- data.frame()
  for (name in listNames) {
    mse <- x[[name]]$MSE
    emse <- x[[name]]$EMSE
    statistic <- x[[name]]$statistic
    p.value <- x[[name]]$p.value
    df <- rbind(df, c(mse, emse, statistic, p.value))
  }
  listNames[1] <- "Full"
  for (i in 2:length(listNames))
    listNames[i] <- paste("without ", listNames[i], sep="")
  df$temp <- listNames
  df <- df[, c(5,1,2,3,4)]
  colnames(df) <- c('Model','MSE','EMSE','Test Statistic','p-value')

  cat("\nResults of the calibration check:\n")
  print(df, row.names=F, right=F)
}

#' @export
print.csslr.model.analysis.lr.calib.test <- function(x, ...) {
  listNames <- names(x)

  df <- data.frame()
  for (name in listNames) {
    statistic <- x[[name]]$statistic
    p.value <- x[[name]]$p.value
    df <- rbind(df, c(statistic, p.value))
  }
  df$temp <- listNames
  df <- df[, c(3,1,2)]
  colnames(df) <- c('Excluded Variable','Test Statistic','p-value')

  cat("\nResults of the incremental calibration test:\n")
  print(df, row.names=F, right=F)
}

#' @export
print.csslr.model.analysis.glm.lr.test <- function(x, ...) {
  listNames <- names(x)

  df <- data.frame()
  for (name in listNames) {
    statistic <- x[[name]]$Chisq[2]
    p.value <- x[[name]]$`Pr(>Chisq)`[2]
    df <- rbind(df, c(statistic, p.value))
  }
  df$temp <- listNames
  df <- df[, c(3,1,2)]
  colnames(df) <- c("Excluded Variable", "Test Statistic", "p-value")
  
  cat("\nResults of the incremental Likelihood Ratio tests:\n")
  print(df, row.names=F, right=F)
}

#' @export
print.csslr.model.analysis.lr.roc <- function(x, ...) {
  listNames <- names(x)

  df <- data.frame()
  for (name in listNames) {
    if (getOption('csslr.use.ar') == FALSE) {
      powerMeasure <- x[[name]]$auc
    } else {
      powerMeasure <- x[[name]]$ar
    }
    numBads <- x[[name]]$numCases
    numGoods <- x[[name]]$numControls
    df <- rbind(df, c(powerMeasure, numBads, numGoods))
  }
  listNames[1] <- 'Full'
  for (i in seq(2, length(listNames), length.out = length(listNames) - 1))
    listNames[i] <- paste("without ", listNames[i], sep="")
  df$temp <- listNames
  df <- df[, c(4,1,2,3)]
  if (getOption('csslr.use.ar') == FALSE) {
    colnames(df) <- c('Model','AUC','#Bads','#Goods')
    cat("\nArea below the ROC curve:\n")
  } else {
    colnames(df) <- c('Model','AR','#Bads','#Goods')
    cat("\nAccuracy Ratio:\n")
  }

  print(df, row.names=F, right=F)
}

#' @export
print.csslr.model.analysis.lr.ic <- function(x, ...) {
  listNames <- names(x)

  df <- data.frame()
  for (name in listNames) {
    aic <- x[[name]]$aic
    bic <- x[[name]]$bic
    df <- rbind(df, c(aic, bic))
  }
  listNames[1] <- "Full"
  for (i in seq(2, length(listNames), length.out = length(listNames) - 1))
    listNames[i] <- paste("without ", listNames[i], sep="")
  df$temp <- listNames
  df <- df[, c(3,1,2)]
  colnames(df) <- c('Model','AIC','BIC')

  cat("\nInformation Criteria:\n")
  print(df, row.names=F, right=F)
}

#' @export
print.csslr.model.analysis.lr.roc.test <- function(x, ...) {
  listNames <- names(x)

  df <- data.frame()
  for (i in seq(2, length(listNames), length.out = length(listNames) - 1)) {
    name <- listNames[i]
    statistic <- x[[name]]$statistic
    p.value <- x[[name]]$p.value
    df <- rbind(df, c(statistic, p.value))
  }
  if (length(listNames) > 1) {
    df$temp <- listNames[2:length(listNames)]
    df <- df[, c(3,1,2)]
    colnames(df) <- c('Excluded Variable','Test Statistic','p-value')
  }

  cat("\nResults of the incremental discriminative power test:\n")
  print(df, row.names=F, right=F)
}

#' @export
print.csslr.model.compare.lr <- function(x, ...) {
  cat("\nResults of the model comparison:\n")
  cat("Mean probability model 1: ", x[["mean.prob"]][["lr.model1"]], "\n", sep="")
  cat("Mean probability model 2: ", x[["mean.prob"]][["lr.model2"]], "\n", sep="")
  if (!is.null(x[["ic"]][["glm.model1"]]$aic)) {
    cat("AIC model 1: ", x[["ic"]][["lr.model1"]]$aic, "\n", sep="")
    cat("AIC model 2: ", x[["ic"]][["lr.model2"]]$aic, "\n", sep="")
    cat("BIC model 1: ", x[["ic"]][["lr.model1"]]$bic, "\n", sep="")
    cat("BIC model 2: ", x[["ic"]][["lr.model2"]]$bic, "\n", sep="")
  }
  if (getOption('csslr.use.ar') == FALSE) {
    cat("Area below the ROC curve model 1: ", x[["roc.test"]]$roc1$auc, "\n", sep="")
    cat("Area below the ROC curve model 2: ", x[["roc.test"]]$roc2$auc, "\n", sep="")
    cat("Test on difference of AUC: ", x[["roc.test"]]$statistic, " (test statistic), ",
        x[["roc.test"]]$p.value, " (p-value)\n", sep="")
  } else {
    cat("Accuracy ratio of model 1: ", x[["roc.test"]]$roc1$ar, "\n", sep="")
    cat("Accuracy ratio of model 2: ", x[["roc.test"]]$roc2$ar, "\n", sep="")
    cat("Test on difference of AR: ", x[["roc.test"]]$statistic, " (test statistic), ",
        x[["roc.test"]]$p.value, " (p-value)\n", sep="")
  }
  cat("Calibration check for model 1: ", x[["calib"]][["lr.model1"]]$MSE, " (MSE), ",
      x[["calib"]][["lr.model1"]]$EMSE, " (EMSE), ", x[["calib"]][["lr.model1"]]$statistic,
      " (test statistic), ", x[["calib"]][["lr.model1"]]$p.value, " (p-value)\n", sep="")
  cat("Calibration check for model 2: ", x[["calib"]][["lr.model2"]]$MSE, " (MSE), ",
      x[["calib"]][["lr.model2"]]$EMSE, " (EMSE), ", x[["calib"]][["lr.model2"]]$statistic,
      " (test statistic), ", x[["calib"]][["lr.model2"]]$p.value, " (p-value)\n", sep="")
  cat("Test on difference of MSE: ", x[["calib.test"]]$statistic, " (test statistic), ",
      x[["calib.test"]]$p.value, " (p-value)\n", sep="")
}

#' @export
print.csslr.stats.roc <- function(x, ...) {
  if (getOption('csslr.use.ar') == FALSE) {
    cat("\nArea below the ROC curve:\n", x$auc)
  } else {
    cat("\nAccuracy Ratio:\n", 2.0 * x$auc - 1.0)
  }
}
