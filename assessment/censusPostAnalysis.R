analysing <- function(dataset, inputs) {
  # for each experiment situation, calculate quality score
    # input: a list of fqsFile, resultFolder (there will be at least svm, rules in this folder)
    # output: quality score
  # a data frame of all quality scores
}

calculateQualityScore <- function(folderName) {
  # For each method, find out a set of good cliques, then get the intersection
  good.svm <- findGoodCliquesFromSVM(folderName)
  
  for(method in methods) {
    fileNames <- list.files(paste(folderName, method, sep = '/'), full.names = TRUE)
    
  }
  
  result <- readExperimentResults(delta, alpha)
  good.rules <- findGoodCliquesFromRules(result$association.rules, dataset)  ## 1478 vs 1981?
  good.svm <- findGoodCliquesFromSVM(result$support.vector.machine) 
  good.cliques <- cliquesFulfillingAllCriterion(good.rules, good.svm)
  return(length(good.cliques) / length(all.cliques))
}

findGoodCliquesFromSVM <- function(folderName, method = 'svm', dataset) {
  fileNames <- list.files(paste(folderName, method, sep='/'), full.names = TRUE)
  # A data frame with 3 columns: column, randomguess, threshold
  maxErrors <- calculateErrorThresholds(dataset)
  
  # For each quasi clique, assess its SVM experiment results
  indices <- NULL
  for(fn in fileNames) {
    # Load result.svm variable: 
    # a list of 2 elements: index (clique index), result (data frame of actual & predicted values)
    load(fn)
    if(isGoodSVMClique(result.svm$result, maxErrors)) {
      indices <- c(indices, result.svm$index)
    }
  }
  return(indices)
  
  
  data <- resultsFromSVM$experiment.details
  df.expected <- data[data$testing_error <= data$expected_error_in_factor, ]
  return(df.expected)
}

calculateErrorThresholds <- function(dataset) {
  temp <- lapply(colnames(dataset), 
                 function(x, data = dataset) {
                   cat <- length(levels(data[, x]))
                   randomguess <- (cat - 1)/cat
                   # Assumed that 50% of an error rate from random guessing is 
                   # considered as a corresponding threshold
                   threshold <- randomguess/2
                   data.frame(column = x, 
                              randomguess = randomguess, 
                              threshold = threshold)
                 })
  # A data frame with 3 columns: column, randomguess, threshold
  Reduce(function(...) merge(..., all = TRUE), temp)
}

isGoodSVMClique <- function(experimentResult, errorThresholds) {
  
  # Do assessment for each target column 
  dfs <- split(experimentResult, experimentResult$target)
  
  provedTestError <- assessTestingError(dfs, errorThresholds)
  if(!is.null(provedTestError)) {
    provedF1Score <- assessF1Score(dfs)
  } else {
    return(FALSE)
  }
  accuracy <- calculateAccurary(dataframe)
  dor <- calculateDiagnosticOddsRatio(dataframe)
  f1 <- calculateF1Score(dataframe)
}

assessTestingError <- function(result.list.by.target, errorThresholds) {
  dfs <- lapply(result.list.by.target,
                function(x) {
                  errors <- unlist(apply(x, 1, 
                                         function(y) {
                                           if(y[2] != y[3]) {
                                             'error'
                                           }}))
                  error <- length(errors) / nrow(x)
                  data.frame(target = x$target[1], testerror = error)
                })
  
  # A data frame with 2 columns: target, testerror
  df <- Reduce(function(...) merge(..., all=TRUE), dfs)
  
  # Find out target columns of which the test error <= threshold
  dfs <- apply(df, 1,
               function(x, thres = errorThresholds) {
                 testerror <- round(as.numeric(x[2]), 2)
                 t <- round(as.numeric(thres[thres$column == x[1],][3]), 2)
                 if(testerror <= t) {
                   data.frame(target=x[1], testerror=x[2], thres[thres$column == x[1],][3])
                 }
               })
  dfs <- delete.Nulls(dfs)
  # If there exists at least one target column fulfilling the criteria mentioned above,
  # it will return a data frame with 3 columns: target, testerror, threshold.
  # If not, NULL will be returned.
  Reduce(function(...) merge(..., all=TRUE), dfs)
}

# Delete NULL entries in a list
delete.Nulls <- function(aList) {
  aList[unlist(lapply(aList, length) != 0)]
}

assessF1Score <- function(result.list.by.target) {
  f1.list <- unlist(lapply(result.list.by.target,
                           function(x) {
                             cm <- confusionMatrix(x$predicted, x$actual)
                             y <- as.data.frame(cm$byClass)
                             y <- modifyColnames(y)
                             precision.u <- sum(y$PosPredValue)
                             recall.u <- sum(y$Sensitivity)
                             2* precision.u * recall.u / (precision.u + recall.u)
                           }))
  
  # Return F1 scores that are > 0.5
  f1.list <- unlist(lapply(f1.list,
                           function(x) {
                             if(!is.na(x)) {
                               if(x > 0.5) {
                                 return(x)
                               }
                             } else {
                               NULL
                             }
                           }))
  return(f1.list)
}

readExperimentResults <- function(delta, alpha) {
  directory <- getDirectory('census', delta, alpha)
  filenames <- getFileNames(directory)
  res.rules <- getRulesResults(filenames$rules)
  res.svm <- getSVMResults(filenames$svm)  ## res.svm is a list of two data frames
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



# If some rule had a lift of 1, it would imply that the probability of occurrence of the antecedent and 
# that of the consequent are independent of each other. 
# If the lift is > 1, that lets us know the degree to which those two occurrences are dependent on one another, 
# and makes those rules potentially useful for predicting the consequent in future data sets.
findGoodCliquesFromRules <- function(resultsFromRules, dataset) {
  dominators <- getAllDominantColumnValues(dataset)
  data <- resultsFromRules[resultsFromRules$lift >= 1.1, ]
  dfs <- split(data, f = data[, 'clique_index'])  ## Split data into data frames w.r.t. clique_index
  res <- Reduce(function(...) merge(..., all=T), 
                lapply(dfs, 
                       function(x, dom = dominators) {
                         temp <- isGoodClique(x, dom)
                         if(temp$isGood) {
                           temp$goodRules
                         } else {
                           NA
                         }
                       }))
  res$y <- NULL
  return(res)
}

# Tested
# A clique is considered good if there is at least one column fulfilling the conditions.
isGoodClique <- function(qs, dominators) {
  dfs <- split(qs, f = qs[, 'outcome_column'])  ## Split data into data frames w.r.t. outcome_column
  isGood <- FALSE
  columns <- NULL
  for(df in dfs) {
    for(lvl in levels(as.factor(df$outcome_value))) {
      if(!isColumnValueDominant(df$outcome_column[1], lvl, dominators)) {
        isGood <- TRUE
        columns <- c(columns, df$outcome_column[1])
      }      
    }
  }
  df <- qs[qs$outcome_column %in% unique(columns), ]   
  list(isGood = isGood, goodRules = df)
}

# Tested
# Return a list of data frames indicating the dominant values according to each column in the dataset.
getAllDominantColumnValues <- function(dataset) {
  rowNum = nrow(dataset)
  ls <- NULL
  for(column in colnames(dataset)) {
    ls.level <- split(dataset[, column], f = dataset[, column])  ## a list of vectors w.r.t. a specific level of this column
    df.level.prop <- calculateLevelProportions(ls.level, rowNum)
    df.level.dominant <- getDominantLevels(df.level.prop)
    if(length(ls) == 0) {
      ls <- list(column = df.level.dominant)
      names(ls) = column
    } else if(length(ls) > 0) {
      ls <- c(ls, list(column = df.level.dominant))
      names(ls)[length(ls)] = column     
    }
  }
  return(ls)
}

# Tested
calculateLevelProportions <- function(list.of.levels, dim.rows) {
  level.names <- NULL
  level.proportions <- NULL
  for(v in list.of.levels) {
    level.names <- c(level.names, as.character(v[1]))
    level.proportions <- c(level.proportions, round(length(v) / dim.rows, 4))
  }
  data.frame(level = level.names, proportion = level.proportions)
}

# Tested
# The levels of which proportions are greater than a threshold proportion are considerd dominant.
getDominantLevels <- function(df) {
  prop <- sort(df$proportion)
  diff <- NULL
  i <- 1  ## initial index
  while(i < length(prop)) {
    temp <- prop[i+1] - prop[i]
    diff <- c(diff, temp)
    i <- i + 1
  }
  thres <- prop[which.max(diff)]  ## Mark the index of the highest differece
  df <- df[df$proportion > thres, ]
  return(df)
}

isColumnValueDominant <- function(col, val, dominators) {
  df <- dominators[[col]]
  if(val %in% df$level) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

cliquesFulfillingAllCriterion <- function(result1, result2) {
  cliques1 <- unique(result1$clique_index)
  cliques2 <- unique(result2$clique_index)
  res <- Reduce(intersect, list(cliques1, cliques2))
  return(res)
}