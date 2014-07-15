loopTrainingTesting <- function(qs) {
  
  library(e1071)
  library(caTools)
  errVec <- NULL
  tarVec <- NULL
  
  print(names(qs))
  
  for(target in colnames(qs)) {   
    data <- takeSamples(qs, target)
    training <- data$training
    testing <- data$testing   
    
    model <- trainSVMModel(training, target)
    err <- getTestingError(model, testing)
    
    print(target)
    print(err)
    
    # trainError <- c(trainError, model$getTrainingError())
    # crossError <- c(crossError, model$getCrossValidationError())
    errVec <- c(errVec, err)
    tarVec <- c(tarVec, target)
  }
  
  thres <- getErrorThreshold(errVec)
  cliq <- paste(colnames(qs), collapse = '--') 
  df <- data.frame(target_column = tarVec, testing_error = errVec)
  list(clique = cliq, details = df, threshold = thres, avg_error = mean(errVec))
}


trainSVMModel <- function(training, target) {

  formulastr <- as.formula(paste(target, "~."))
  
  # model <- ksvm(formulastr, data = qs, type = "C-bsvc", kernel = "rbfdot", kpar = list(sigma = 0.1), 
  #               C = 10, prob.model = TRUE)
  # model <- svm(formulastr, data = qs)
  obj <- tune(svm, formulastr, data = training, ranges = list(gamma = c(0.1, 1, 10), cost = c(1, 10, 50)), 
              tunecontrol = tune.control(sampling = "fix"))
  
  list(model = obj$best.model, target = target)
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