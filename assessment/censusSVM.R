# get training model, errors and target, respectively
getTrainingResultsOfOneClique <- function(qs) {
  
  ## install.packages("kernlab")
  library(kernlab)
  cols <- colnames(qs)
  trainErr <- NULL
  CVErr <- NULL
  target <- NULL
  
  for(targetcol in cols) {   
    model <- trainSVMModel(qs, targetcol)
    trainErr <- c(trainErr, model$getTrainingError())
    CVErr <- c(CVErr, model$getCrossValidationError())
    target <- c(target, targetcol)
  }
  
  df <- data.frame("quasi_clique" = rep(qs, ncol(qs)), "training_error" = trainErr, 
                   "crossvalidation_error" = CVErr, "target value" = target)
  return(df)
}


# train a svm model for a specific target value
# input: preprocessed data frame (quasi clique)
# output: training and cross validation errors
trainSVMModel <- function(qs, target) {

  formulastr <- as.formula(paste(target, "~."))
  model <- ksvm(formulastr, data = qs, type = "C-bsvc", kernel = "rbfdot", kpar = list(sigma = 10), 
                C = 10, cross = 5, prob.model = TRUE)
  
  getTrainingError <- function() {
    error(model)
  }
  
  getCrossValidationError <- function() {
    cross(model)
  }
  
  list(model = model, getTrainingError = getTrainingError, getCrossValidationError = getCrossValidationError)
}


