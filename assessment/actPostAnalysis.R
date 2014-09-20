library("data.table")

calculateQuality <- function(folderName, fqsFile) {
  #good.cart <- findGoodCliquesFromCart(folderName)
  good.lm = findGoodCliquesFromLM(folderName)
  
  allCliques <- readQuasiCliques(fqsFile)
  quality <- nrow(good.lm) / length(allCliques)
  
  list(QualityScore=quality, QualifiedCliques=good.lm)
}

computeRootMeanSquareDeviation<- function(df) {
  numerator <- sum((df$actual - df$predicted)^2)
  RMSD <- sqrt(numerator/nrow(df))
  return(RMSD)
}

returnRMSDTable <- function(experimentResult) {
  dfs <- split(experimentResult, experimentResult$target)
  rmsd.list <- unlist(lapply(dfs,
                            function(x) computeRootMeanSquareDeviation(x)))
  x <- as.matrix(rmsd.list)
  data.frame(target = row.names(x), RMSD = x[,1])
}

findGoodCliquesFromCart <- function(folderName, method='cart') {
  fileNames <- list.files(paste(folderName, method, sep='/'), full.names = TRUE) 
  # For each quasi clique, assess its CART experiment results
  good <- NULL
  for(fn in fileNames) {
    # Load result.cart variable: 
    # a list of 2 elements: index (clique index), result (data frame of actual & predicted values)
    load(fn)
    #log(paste("Examing cart clique", result.cart$index))
    x <- isGoodRealValueClique(result.cart$result)
    if(x$boolean) {
      good <- rbind(good, data.frame(index = result.cart$index,
                                     target = x$best$target,
                                     NRMSD <- x$best$NRMSD))
    }
  }
  return(good)
}

findGoodCliquesFromLM <- function(folderName, method='lm') {
  fileNames <- list.files(paste(folderName, method, sep='/'), full.names = TRUE) 
  good = NULL
  for(fn in fileNames) {
    load(fn)
    log(paste("Examing lm clique", result.lm$index))
    x = isGoodRealValueClique(result.lm$result)
    if(x$boolean) {
      good = rbind(good, data.frame(index=result.lm$index,
                                    target = x$best$target,
                                    SMAPE <- x$best$SMAPE))
    }
  }
  return(good)
}

isGoodRealValueClique <- function(experimentResult) {
  boolean <- FALSE
  best <- NULL
  #nrmsd.threshold = 0.1
  smape.threshold = 0.1
  
  #nrmsd <- computeNormalizedRootMeanSquareDeviation(experimentResult)
  
  # Force R not to use expotional notation.
  options("scipen"=100, "digits"=4)
  x = computeSymmetricMeanAbsolutePercentageError(experimentResult)
  #nrmsd.min = min(nrmsd$NRMSD)
  minSAMPE = min(x$SMAPE)
  
  if(minSAMPE < smape.threshold) {
    boolean <- TRUE
    best <- x[which.min(x$SMAPE),]
  }
  
  list(boolean=boolean, best=best)
}

computeSymmetricMeanAbsolutePercentageError <- function(experimentResult) {
  dt <- data.table(experimentResult)
  smape <- as.data.frame(dt[, computeSMAPE(actual, predicted), by=list(target)])
  setnames(smape, names(smape), c('target', 'SMAPE'))
  return(smape) 
}

computeSMAPE <- function(actual, predicted) {
  df <- data.frame(actual=actual, predicted=predicted)
  smape <- unlist(apply(df, 1,
                      function(x) {
                        abs(x[1] - x[2]) / abs(x[1] + x[2])
                      }))
  
  # If both actual & predicted value equals zero, it'll result in NaN.
  # Eliminate NaNs
  smape = smape[!is.nan(smape)]
  # Set the Inf values 1000000 if the sum of actual & predicted equals zero
  smape[which(smape == Inf)] = 1000000
  
  sum(smape) / length(smape)
}

computeNRMSD <- function(actual, predicted) {
  rmsd = computeRootMeanSquareDeviation(data.frame(actual=actual, predicted=predicted))
  rmsd / (max(actual) - min(actual))
}

computeNormalizedRootMeanSquareDeviation <- function(experimentResult) {
  dt <- data.table(experimentResult)
  nrmsd <- as.data.frame(dt[, computeNRMSD(actual, predicted), by=list(target)])
  setnames(nrmsd, names(nrmsd), c('target', 'NRMSD'))
  return(nrmsd) 
}



















getLinearRegressionResults <- function(filenames) {
  df.results <- NULL
  for(fn in filenames) {
    load(fn)  # Will have a result.lm variable in the environment after loading a file
    df.results <- rbind(df.results, result.lm)
  }
  return(df.results)
}

getRegressionTreeResults <- function(filenames) {
  df.results <- NULL
  for(fn in filenames) {
    load(fn)  # Will have a result.cart variable in the environment after loading a file
    df.results <- rbind(df.results, result.lm)
  }
  return(df.results)
}

# Cliques of which both regular r square and predicted r square >= 0.65 are considered good
findGoodCliquesFromLinearRegreassion <- function(df) {
  res <- df[df$r_square >= 0.65 & df$predicted_r_square >= 0.65, ]
  return(res)
}