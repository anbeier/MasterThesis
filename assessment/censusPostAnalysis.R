getDirectory <- function(delta, alpha) {
  prefix <- paste(paste('delta', delta, sep = ''), paste('alpha', alpha, sep = ''), sep = '_')
  foldername <- paste('census', prefix, sep = '_')
  return(foldername)
}

getFileNames <- function(directory) {
  filenames <- 
  return(filenames)
}

getFileContents <- function(filenames) {
  dfErrorOfEachColumn <- NULL
  dfCriticalErrorOfEachClique <- NULL
  for(fn in filenames) {
    load(fn)    # Will have a rslt value in the environment after loading a file
    index <- rep(rslt$clique_index, nrow(rslt$details))
    df <- data.frame(clique_index = index, rslt$details)
    dfErrorOfEachColumn <- rbind(dfErrorOfEachColumn, df)
    df <- data.frame(clique_index = rslt$clique_index, min_error = rslt$min_error, 
                     error_threshold = rslt$threshold, avg_error = rslt$avg_error)
    dfCriticalErrorOfEachClique <- rbind(dfCriticalErrorOfEachClique, df)
  }
  list(ErrorOfEachColumn = dfErrorOfEachColumn, 
       CriticalErrorOfEachClique = dfCriticalErrorOfEachClique)
}

readExperimentResults <- function(delta, alpha) {
  dir <- getDirectory(delta, alpha)
  filenames <- list.files(dir, full.names = TRUE)  # only 98 files, it should be 102
  results <- getFileContents(filenames)
}