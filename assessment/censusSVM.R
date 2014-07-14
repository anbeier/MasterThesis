getResultsOfOneClique <- function(qs) {
  
  library(e1071)
  errVec <- NULL
  tarVec <- NULL
  
  for(target in colnames(qs)) {   
    data <- takeSamples(qs, target)
    training <- data$training
    testing <- data$testing   
    
    model <- trainSVMModel(training, target)
    err <- getTestingError(model, testing)
    
    # trainError <- c(trainError, model$getTrainingError())
    # crossError <- c(crossError, model$getCrossValidationError())
    errVec <- c(errVec, err)
    tarVec <- c(tarVec, target)
  }
  
  thres <- getErrorThreshold(errVec)
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
  
  # model <- ksvm(formulastr, data = qs, type = "C-bsvc", kernel = "rbfdot", kpar = list(sigma = 0.1), 
  #               C = 10, prob.model = TRUE)
  model <- svm(formulastr, data = qs)
  # getTrainingError <- function() {
  #   error(model)
  # }
  
  # getCrossValidationError <- function() {
  #   cross(model)
  # }
  
  # list(model = model, getTrainingError = getTrainingError, getCrossValidationError = getCrossValidationError)
  list(model = model, target = target)
}

getTestingError <- function(trainls, testing) {
  
  model <- trainls$model
  target <- trainls$target
  pred <- predict(model, newdata = testing)
  
  testing$predicted <- pred
  err <- 0
  for(i in 1:nrow(testing)) {
    if(testing[i, target] != testing[i, 'predicted']) {
      err <- err + 1
    }
  }
  return(err / nrow(testing))
}

getErrorThreshold <- function(errors) {
  err <- sort(errors)
  dif <- NULL
  i <- 1
  while(i < length(err)) {
    tmp <- err[i + 1] - err[i]
    dif <- c(dif, tmp)
    i = i + 1
  }
  t <- err[which.max(dif)]
  return(t)
}