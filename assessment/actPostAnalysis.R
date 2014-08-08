calculateQuality <- function(delta, alpha) {
  data <- readExperimentResults(delta, alpha)
  good <- findGoodCliquesFromLinearRegreassion(data)
  totalNum <- length(unique(data$clique_index))
  goodNum <- length(unique(good$clique_index))
  return(goodNum / totalNum)
}

readExperimentResults <- function(delta, alpha) {
  folder <- getFolderName('act', delta, alpha)
  filenames <- list.files(folder, full.name = TRUE)  ## a list of file names
  res.lm <- getLinearRegressionResults(filenames)
  return(res.lm)
}

getFolderName <- function(datasetName, delta, alpha) {
  prefix <- paste(paste('delta', delta, sep = ''), paste('alpha', alpha, sep = ''), sep = '_')
  folderName <- paste(datasetName, prefix, sep = '_')
  return(folderName)
}

getLinearRegressionResults <- function(filenames) {
  df.results <- NULL
  for(fn in filenames) {
    load(fn)  # Will have a result.lm variable in the environment after loading a file
    df.results <- rbind(df.results, result.lm)
  }
  return(df.results)
}

# Cliques of which both regular r square and predicted r square >= 0.65 are considered good
findGoodCliquesFromLinearRegreassion <- function(df) {
  res <- df[df$r_square >= 0.65 & df$predicted_r_square >= 0.65, ]
  return(res)
}