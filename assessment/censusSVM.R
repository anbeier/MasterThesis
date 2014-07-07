# get training model, errors and target, respectively
getTrainingResultsOfOneClique <- function(qs) {
  
  ## install.packages("kernlab")
  library(kernlab)
  cols <- colnames(qs)
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
  
  df <- data.frame("quasi_clique" = rep(qs, ncol(qs)), "training_error" = trainError, 
                   "crossvalidation_error" = crossError, "target_value" = tar)
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


