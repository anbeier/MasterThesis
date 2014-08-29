fqsFileList <- function() {
  ls <- NULL
  f <- 'census_fqs_delta0.7_alpha0.5.txt'
  ls <- append(ls, f)
  f <- 'census_fqs_delta0.8_alpha0.5.txt'
  ls <- append(ls, f)
  f <- 'census_fqs_delta0.9_alpha0.5.txt'
  ls <- append(ls, f)
  return(ls)
}

folderList <- function() {
  ls <- NULL
  f <- 'census_delta0.7_alpha0.5'
  ls <- append(ls, f)
  f <- 'census_delta0.8_alpha0.5'
  ls <- append(ls, f)
  f <- 'census_delta0.9_alpha0.5'
  ls <- append(ls, f)
  return(ls)
}

analysing <- function(dataset, fqsFiles, folders) {
  # A data frame with 3 columns: column, randomguess, threshold
  # maxErrors <- calculateErrorThresholds(dataset)
  
  # Get numbers of fqs files
  num <- length(fqsFileList)
  
  for (i in 1:num) {
    x <- calculateQuality(folders[i], fqsFiles[i])
    quality <- x$QualityScore
    qualifiedCliques <- x$QualifiedCliques
  }
  # for each experiment situation, calculate quality score
    # input: a list of fqsFile, resultFolder (there will be at least svm, rules in this folder)
    # output: quality score, qualified cliques
  # a data frame of all quality scores
}

# Return list(QualityScore, QualifiedCliques)
calculateQuality <- function(folderName, fqsFile) {
  # For each method, find out a set of good cliques, then get the intersection
  good.svm <- findGoodCliquesFromSVM(folderName)
  good.bayes <- findGoodCliquesFromBayes(folderName)
  
  good.all <- intersectGoodResults(good.svm, good.bayes)
  
  allCliques <- readingQuasiCliques(fqsFile)
  quality <- nrow(good.all) / length(allCliques)
  
  list(QualityScore=quality, QualifiedCliques=good.all)
}

# Return data.frame(index, target)
findGoodCliquesFromSVM <- function(folderName, method = 'svm') {
  fileNames <- list.files(paste(folderName, method, sep='/'), full.names = TRUE) 
  # For each quasi clique, assess its SVM experiment results
  good <- NULL
  for(fn in fileNames) {
    # Load result.svm variable: 
    # a list of 2 elements: index (clique index), result (data frame of actual & predicted values)
    load(fn)
    log(paste("Examing svm clique", result.svm$index))
    x <- isGoodFactorClique(result.svm$result)
    if(x$boolean) {
      good <- rbind(good, data.frame(index = result.svm$index,
                                     target = x$target))
    }
  }
  return(good)
}

# Return data.frame(index, target)
findGoodCliquesFromBayes <- function(folderName, method='bayes') {
  fileNames <- list.files(paste(folderName, method, sep='/'), full.names = TRUE)
  good <- NULL
  for(fn in fileNames) {
    # Load result.bayes variable
    load(fn)
    log(paste("Examing bayes clique", result.bayes$index))
    x <- isGoodFactorClique(result.bayes$result)
    if(x$boolean) {
      good <- rbind(good, data.frame(index = result.bayes$index,
                                     target = x$target))
    }
  }
  return(good)
}

isGoodSVMClique <- function(experimentResult, errorThresholds) {
  
  # Do assessment for each target column 
  dfs <- split(experimentResult, experimentResult$target)
  
  boolean <- FALSE
  best <- NULL
  
  provedTestError <- assessTestingError(dfs, errorThresholds)
  
  if(!is.null(provedTestError)) {
    provedTestError <- as.character(provedTestError[which.min(provedTestError$testerror), 'target'])
    provedF1Score <- assessF1Score(dfs)
    
    if(!is.null(provedF1Score)) {
      boolean <- TRUE
      provedF1Score <- as.character(provedF1Score[which.max(provedF1Score$f1score), 'target'])
    } 
  }
  
  if(boolean) {
    if(!provedTestError == provedF1Score) {
      best <- paste(provedTestError, provedF1Score, sep = ',')
    } else {
      best <- provedTestError
    }
  }
  
  list(boolean=boolean, target=best)
}

# Return list(boolean, target)
isGoodFactorClique <- function(experimentResult) {
  dfs <- split(experimentResult, experimentResult$target)
  boolean <- FALSE
  best <- NULL
  
  provedF1Score <- assessF1Score(dfs)
  
  if(!is.null(provedF1Score)) {
    # Get the best F1 score with its target column
    bestF1Score <- provedF1Score[which.max(provedF1Score$f1score),]
    
    provedMCC <- assessMatthewsCorrelationCoefficient(dfs)
    if(!is.null(provedMCC)) {
      boolean <- TRUE
      bestMCC <- provedMCC[which.max(provedMCC$mcc),]
    }
  }
  
  if(boolean) {
    best <- getBestTargetColumn(bestF1Score, bestMCC)
  }
  list(boolean=boolean, target=best)
}

# Return the best target(s)
getBestTargetColumn <- function(best1, best2) {
  best <- NULL
  if(!best1[,'target'] == best2[,'target']) {
    if(best1[,2] == best2[,2]) {
      best <- paste(best1[,'target'], best2[,'target'], sep = ',')
    } else if(best1[,2] > best2[,2]) {
      best <- best1[,'target']
    } else {
      best <- best2[,'target']
    }
  } else {
    best <- best2[,'target']
  }
  return(best)
}

assessTestingError <- function(result.list.by.target, errorThresholds) {
  # If predicted does not equal actual, mark an error.
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

# Return a data frame with 2 columns: target, f1score (proved)
assessF1Score <- function(result.list.by.target) {
  
  f1.threshold <- 0.8
  # For each data frame (w.r.t a target column), calculate F1 score.
  f1.list <- unlist(lapply(result.list.by.target,
                           function(x) computeF1ScoreMulticlass(x)))
  
  res <- NULL
  for(i in 1:length(f1.list)) {
    f <- f1.list[i]
    # F1 score could be NaN, because precision.u and recall.u could both be zero
    if(!is.nan(f)) {
      if(f >= f1.threshold & f <= 1) {
        res <- rbind(res, data.frame(target = result.list.by.target[[i]]$target[1],
                                     f1score = f))
      }
    }
  }
  return(res)
}

# Return a data frame with 2 columns: target, mcc (proved)
assessMatthewsCorrelationCoefficient <- function(result.list.by.target) {
  mcc.threshold <- 0.5
  mcc.list <- unlist(lapply(result.list.by.target,
                            function(x) computeMCCMutliclass(x)))
  res <- NULL
  for(i in 1:length(mcc.list)) {
    if(mcc.list[i] >= mcc.threshold & mcc.list[i] <= 1) {
      res <- rbind(res, data.frame(target = result.list.by.target[[i]]$target[1],
                                   mcc = mcc.list[i]))
    }
  }
  return(res)
}

# Delete NULL entries in a list
delete.Nulls <- function(aList) {
  aList[unlist(lapply(aList, length) != 0)]
}

# Return a data frame with 4 columns: TP, TN, FP, FN
computeConfusionMatrixOfOneLabel <- function(classLabel, data) {
  condition.pos <- subset(data, actual == classLabel)  ## data frame of condition positives
  condition.neg <- subset(data, !actual == classLabel) ## data frame of condition negatives
  
  tp <- length(condition.pos$predicted[condition.pos$predicted == classLabel])
  tn <- length(condition.neg$predicted[!condition.neg$predicted == classLabel])
  fp <- length(condition.neg$predicted[condition.neg$predicted == classLabel])
  fn <- length(condition.pos$predicted[!condition.pos$predicted == classLabel])
  
  data.frame(TP=tp, TN=tn, FP=fp, FN=fn)
}

computeF1ScoreMulticlass <- function(data) {
  # Correct the levels of factors in actual, predicted columns
  df <- correctFactorLevels(data)
  # Get all categories
  cat <- levels(df$actual)
  # Compute confusion matrix for each category
  lp <- lapply(cat,
               function(x, data=df) computeConfusionMatrixOfOneLabel(x, data=df))
  df <- Reduce(function(...) merge(..., all=TRUE), lp)
  
  precision.u <- sum(df$TP)/sum(df$TP + df$FP)
  recall.u <- sum(df$TP)/sum(df$TP + df$FN)
  2 * precision.u * recall.u / (precision.u + recall.u)
}

computeMCCMutliclass <- function(data) {
  # Correct the levels of factors in actual, predicted columns
  df <- correctFactorLevels(data)
  # Get all categories
  cat <- levels(df$actual)
  # Compute confusion matrix for each category
  lp <- lapply(cat,
               function(x, data=df) computeConfusionMatrixOfOneLabel(x, data=df))
  df <- Reduce(function(...) merge(..., all=TRUE), lp)
  
  dividend <- sum(df$TP)*sum(df$TN) - sum(df$FP)*sum(df$FN)
  #divisor <- sum(df$TP+df$FP) * sum(df$TP+df$FN) * sum(df$TN+df$FP) * sum(df$TN+df$FN)
  divisor <- sqrt(sum(df$TP+df$FP)) * sqrt(sum(df$TP+df$FN)) * sqrt(sum(df$TN+df$FP)) * sqrt(sum(df$TN+df$FN))
  dividend / divisor
}

correctFactorLevels <- function(df) {
  # Drop levels that do not appear.
  df$actual = factor(df$actual)
  # Relevel df$predicted and let them be the same as df$actual.
  df$predicted = factor(df$predicted, levels=levels(df$actual))
  return(df)
}

# Return a data frame with 2 columns: index, target (the one and only)
intersectGoodResults <- function(res1, res2) {
  # Inner join on index
  df <- merge(res1, res2, by = 'i')
  # Find common value in target
  #df$target <- Reduce(function(...) intersect(...), 
  #                    list(unlist(strsplit(as.character(df[,2]), ',')), 
  #                         unlist(strsplit(as.character(df[,3]), ','))))
  df$target <- unlist(apply(df, 1, 
                            function(x) {
                              y <- intersect(x[2], x[3])
                              if(length(y) == 0) {
                                NA
                              } else if(length(y) == 1) {
                                y
                              } else if(length(y) > 1) {
                                paste(y, collapse = ',')
                              }
                            }))
  
  # Remove NAs resulting from intersection
  df <- na.omit(df)
  # Delete target.x column
  df[,2] <- NULL
  # Delete target.y column
  df[,2] <- NULL
  return(df)
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

getRulesResults <- function(filenames) {
  dfExperimentsDetails <- NULL
  for(fn in filenames) {
    load(fn) # Will have a result.rules variable in the environment after loading a file
    dfExperimentsDetails <- rbind(dfExperimentsDetails, result.rules)
  }
  return(dfExperimentsDetails)
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