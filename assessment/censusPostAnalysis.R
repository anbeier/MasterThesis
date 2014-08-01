findGoodCliques <- function() {
  ls <- readExperimentResults(dataset, delta, alpha)
  goodFromRules <- findGoodCliquesFromRules(ls$association.rules)
  goodFromSVM <- findGoodCliquesFromSVM(ls$support.vector.machine)
}

readExperimentResults <- function(dataset, delta, alpha) {
  dir <- getDirectory(dataset, delta, alpha)
  filenames <- getFileNames(dir)
  res.rules <- getRulesResults(filenames$rules)
  res.svm <- getSVMResults(filenames$svm)
  list(association.rules = res.rules,
       support.vector.machine = res.svm)
}

getDirectory <- function(dataset, delta, alpha) {
  prefix <- paste(paste('delta', delta, sep = ''), paste('alpha', alpha, sep = ''), sep = '_')
  folderName <- paste(dataset, prefix, sep = '_')
  return(folderName)
}

# Return a list of filenames w.r.t. experiment methods, e.g. rules, svm
getFileNames <- function(directory) {
  folderpaths <- list.files(directory, full.names=TRUE)
  foldernames <- list.files(directory)
  rules.fn <- NULL  ## initial file names
  svm.fn <- NULL
  i <- 1  ## initial folder index
  while(i <= length(foldernames)) {
    fs <- list.files(folderpaths[i], full.names=TRUE)
    if(foldernames[i] == 'rules') {
      rules.fn <- fs
    } else if(foldernames[i] == 'svm') {
      svm.fn <- fs
    }
    i = i + 1
  }
  list(rules = rules.fn, svm = svm.fn)
}

getRulesResults <- function(filenames) {
  dfExperimentsDetails <- NULL
  for(fn in filenames) {
    load(fn) # Will have a result.rules variable in the environment after loading a file
    dfExperimentsDetails <- rbind(dfExperimentsDetails, result.rules)
  }
  return(dfExperimentsDetails)
}

getSVMResults <- function(filenames) {
  experiment.details <- NULL
  critical.errors <- NULL
  for(fn in filenames) {
    load(fn)    # Will have a result.svm variable in the environment after loading a file
    ## index <- rep(rslt$clique_index, nrow(rslt$details))
    df <- data.frame(clique_index = result.svm$clique_index, result.svm$details)
    experiment.details <- rbind(experiment.details, df)
    df <- data.frame(clique_index = result.svm$clique_index, min_error = result.svm$min_error, 
                     error_threshold = result.svm$threshold, avg_error = result.svm$avg_error)
    critical.errors <- rbind(critical.errors, df)
  }
  list(experiment.details = experiment.details, 
       critical.errors = critical.errors)
}

# expected_error_in_factor is set to validate predictions that are a little better than a random guessing.
# Columns of which testing error smaller than/equals expected error are considerd good
findGoodCliquesFromSVM <- function(resultsFromSVM) {
  data <- resultsFromSVM$experiment.details
  df.expected <- data[data$testing_error <= data$expected_error_in_factor, ]
  return(df.expected)
}

# If some rule had a lift of 1, it would imply that the probability of occurrence of the antecedent and 
# that of the consequent are independent of each other. 
# If the lift is > 1, that lets us know the degree to which those two occurrences are dependent on one another, 
# and makes those rules potentially useful for predicting the consequent in future data sets.
findGoodCliquesFromRules <- function(resultsFromRules) {
  
}

# To be continued
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