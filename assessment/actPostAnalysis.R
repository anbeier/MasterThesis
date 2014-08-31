# Return list(QualityScore, QualifiedCliques)
calculateQuality <- function(folderName, fqsFile) {
  good.? <- findGoodCliquesFrom?(folderName)
  # use MAPE
  
  allCliques <- readingQuasiCliques(fqsFile)
  quality <- nrow(good.?) / length(allCliques)
  
  list(QualityScore=quality, QualifiedCliques=good.?)
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