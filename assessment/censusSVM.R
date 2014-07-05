# train a svm model for a specific target value
# input: preprocessed data frame (quasi clique)
# output: training and cross validation errors
trainSVMModel <- function(qs, target) {

  target <- reviseTargetName(target)
  formulastr <- as.formula(paste(target, "~."))
  
  ## install.packages("kernlab")
  library(kernlab)
  model <- ksvm(formulastr, data = qs, type = "C-bsvc", kernel = "rbfdot", kpar = list(sigma = 10), 
                C = 10, cross = 5, prob.model = TRUE)
  
  getTrainingError <- function() {
    error(model)
  }
  
  getCrossValidationError <- function() {
    cross(model)
  }
  
  list(getTrainingError = getTrainingError, getCrossValidationError = getCrossValidationError)
}


