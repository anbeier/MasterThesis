library("data.table")

calculateQuality <- function(folderName, fqsFile) {
  good.cart <- findGoodCliquesFromCart(folderName)
  # use MAPE
  
  allCliques <- readingQuasiCliques(fqsFile)
  quality <- nrow(good.cart) / length(allCliques)
  
  list(QualityScore=quality, QualifiedCliques=good.cart)
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
                                     SMAPE <- x$best$SMAPE))
    }
  }
  return(good)
}

isGoodRealValueClique <- function(experimentResult) {
  boolean <- FALSE
  best <- NULL
  smape.threshold = 10
  
  smape <- computeSymmetrMeanAbsPercentageError(experimentResult)
  smape.min = min(smape$SMAPE)
  if(smape.min <= smape.threshold) {
    boolean <- TRUE
    best <- smape[which.min(smape$SMAPE),]
  }
  
  list(boolean=boolean, best=best)
}

computeSymmetrMeanAbsPercentageError <- function(experimentResult) {
  computeSMAPE <- function(actual, predicted) {
    df <- data.frame(acutal=actual, predicted=predicted)
    lp <- unlist(apply(df, 1,
                       function(x) {
                         abs(x[1] - x[2]) / (x[1] + x[2])
                       }))
    sum(lp) * 100 / nrow(df)
  }
  
  dt <- data.table(experimentResult)
  smape <- as.data.frame(dt[, computeSMAPE(actual, predicted), by=list(target)])
  colnames(smape) = c('target', 'SMAPE')
  return(smape)  # data.frame(target, SMAPE)
}





















calculateQuality <- function(delta, alpha) {
  data <- readExperimentResults(delta, alpha)
  good.lm <- findGoodCliquesFromLinearRegreassion(data)
  totalNum <- length(unique(data$clique_index))
  goodNum <- length(unique(good$clique_index))
  return(goodNum / totalNum)
}

readExperimentResults <- function(delta, alpha) {
  files <- getFilePaths(delta, alpha)
  res.cart <- getRegressionTreeResults(files$cart)
  res.lm <- getLinearRegressionResults(files$lm)
  list(cart = res.cart, lm = res.lm)
}

# returns a folder name in form of 'act_deltaX_alphaX'
getFolderName <- function(datasetName, delta, alpha) {
  prefix <- paste(paste('delta', delta, sep = ''), paste('alpha', alpha, sep = ''), sep = '_')
  folderName <- paste(datasetName, prefix, sep = '_')
  return(folderName)
}

getFilePaths <- function(delta, alpha) {
  folder <- getFolderName('act', delta, alpha)  
  foldernames <- list.files(folder)  ## a list of file names
  folderpaths <- list.files(folder, full.name = TRUE)  
  
  lm.fn <- NULL  ## initial file names
  cart <- NULL
  i <- 1  ## initial folder index
  while(i <= length(foldernames)) {
    fs <- list.files(folderpaths[i], full.names=TRUE)
    if(foldernames[i] == 'lm') {
      lm.fn <- fs
    } else if(foldernames[i] == 'cart') {
      cart.fn <- fs
    }
    i = i + 1
  }
  list(cart = cart.fn, lm = lm.fn)
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