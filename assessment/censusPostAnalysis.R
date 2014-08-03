csvfp = 'census.csv'
colnamefp = 'census_colnames.txt'
dataset <- getCensusData(csvfp, colnamefp)

calculateQuality <- function(dataset, delta, alpha, fqsFile) {
  all.cliques <- readingQuasiCliques(fqsFile)
  result <- readExperimentResults(delta, alpha)
  good.rules <- findGoodCliquesFromRules(result$association.rules, dataset)  ## 1478 vs 1981?
  good.svm <- findGoodCliquesFromSVM(result$support.vector.machine) 
  good.cliques <- cliquesFulfillingAllCriterion(good.rules, good.svm)
  return(length(good.cliques) / length(all.cliques))
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