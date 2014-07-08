# get training model, errors and target, respectively
getTrainingResultsOfOneClique <- function(qs) {
  
  ## install.packages("kernlab")
  library(kernlab)
  trainError <- NULL
  crossError <- NULL
  tar <- NULL
  
  for(target in colnames(qs)) {   
    df <- takeSamples(qs, target)
    model <- trainSVMModel(df, target)
    trainError <- c(trainError, model$getTrainingError())
    crossError <- c(crossError, model$getCrossValidationError())
    tar <- c(tar, target)
  }
  
  avgTrainError <- getAvgError(trainError)
  # report <- paste('avg training error: ', avgTrainError)
  cliq <- paste(colnames(qs), collapse = '--')
  df <- data.frame(quasi_clique = c(cliq, rep(NA, ncol(qs) - 1)), training_error = trainError,
                                    crossvalidation_error = crossError, target_value = tar, 
                                    avg_train_error = c(avgTrainError, rep(NA, ncol(qs) - 1)))
  return(df)
}


# train a svm model for a specific target value
# input: preprocessed data frame (quasi clique)
# output: training and cross validation errors
trainSVMModel <- function(qs, target) {

  formulastr <- as.formula(paste(target, "~."))
  model <- ksvm(formulastr, data = qs, type = "C-bsvc", kernel = "rbfdot", kpar = list(sigma = 0.1), 
                C = 10, prob.model = TRUE)
  
  getTrainingError <- function() {
    error(model)
  }
  
  getCrossValidationError <- function() {
    cross(model)
  }
  
  list(model = model, getTrainingError = getTrainingError, getCrossValidationError = getCrossValidationError)
}

getAvgError <- function(error) {
  res <- 0
  for(i in error) {
    res <- res + i
  }
  res <- res / length(error)
  return(res)
}


