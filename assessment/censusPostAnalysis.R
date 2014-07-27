readExperimentResults <- function(dataset, delta, alpha) {
  dir <- getDirectory(dataset, delta, alpha)
  filenames <- getFileNames(dir)
  res.rules <- getRulesReport(filenames$rules)
  res.svm <- getSVMReport(filenames$svm)
}

getDirectory <- function(dataset, delta, alpha) {
  prefix <- paste(paste('delta', delta, sep = ''), paste('alpha', alpha, sep = ''), sep = '_')
  folderName <- paste(dataset, prefix, sep = '_')
  return(folderName)
}

# Return a list of filenames w.r.t. experiment methods
getFileNames <- function(directory) {
  folderpaths <- list.files(directory, full.names=TRUE)
  foldernames <- list.files(directory)
  filenames <- NULL
  i <- 1  ## initial folder index
  while(i <= length(foldernames)) {
    files <- list.files(folderpaths[i], full.names=TRUE)
    if(foldernames[i] == 'rules') {
      if(is.null(filenames)) {
        filenames <- list(svm = files)
      } else {
        filenames <- list(filenames, svm = files)
      }
    } else if(foldernames[i] == 'svm') {
      if(is.null(filenames)) {
        filenames <- list(svm = files)
      } else {
        filenames <- list(filenames, svm = files)
      }
    }
    i = i + 1
  }
  return(filenames)
}

getRulesReport <- function(filenames) {
  dfExperimentsDetails <- NULL
  for(fn in filenames) {
    load(fn) # Will have a result.rules variable in the environment after loading a file
    dfExperimentsDetails <- rbind(dfExperimentsDetails, result.rules)
  }
  return(dfExperimentsDetails)
}

getSVMReport <- function(filenames) {
  dfExperimentsDetails <- NULL
  dfCriticalErrorOfEachClique <- NULL
  for(fn in filenames) {
    load(fn)    # Will have a result.svm variable in the environment after loading a file
    ## index <- rep(rslt$clique_index, nrow(rslt$details))
    df <- data.frame(clique_index = result.svm$clique_index, result.svm$details)
    dfExperimentsDetails <- rbind(dfExperimentsDetails, df)
    df <- data.frame(clique_index = result.svm$clique_index, min_error = result.svm$min_error, 
                     error_threshold = result.svm$threshold, avg_error = result.svm$avg_error)
    dfCriticalErrorOfEachClique <- rbind(dfCriticalErrorOfEachClique, df)
  }
  list(dfExperimentsDetails = dfExperimentsDetails, 
       CriticalErrorOfEachClique = dfCriticalErrorOfEachClique)
}

getColumnsWithinThreshold <- function(resultsList) {
  errorOfEachColumn <- resultsList$ErrorOfEachColumn
  criticalErrorOfEachClique <- resultsList$CriticalErrorOfEachClique
  # Cliques whose minimal error is greater than 0.4 is considered as bad.
  criticalErrorOfEachClique <- criticalErrorOfEachClique[criticalErrorOfEachClique$min_error < 0.4, ]
  listOfCliques <- split(errorOfEachColumn, errorOfEachColumn$clique_index)
  df <- NULL
  for(qs in listOfCliques) {
    i <- qs$clique_index[1]
    threshold <- criticalErrorOfEachClique$error_threshold[which(criticalErrorOfEachClique$clique_index == i)]
    qs <- qs[qs$testing_error <= threshold, ]
    df <- rbind(df, qs)
  }
  return(df)
}