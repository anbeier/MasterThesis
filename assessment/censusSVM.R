source('log.R')

# For-loop in a quasi clique; for each column as outcome value train a SVM model
# using training dataset and apply this model on testing dataset
loopTrainingTesting <- function(qs, index, delta, alpha) {
  fileNames <- makeFileNameForResultsFromSVM(index, delta, alpha)
  errVec <- NULL
  tarVec <- NULL
  levelVec <- NULL
  
  i <- 1
  l <- length(colnames(qs))
  for(target in colnames(qs)) {   
    log(paste(index, paste(i, l, sep='/'), target))
    data <- takeSamples(qs, target) 
    training <- data$training
    testing <- data$testing   
    
    model <- trainSVMModel(training, target)
    err <- getTestingError(model, testing)
    factorLevels <- (1 - 1/levels(qs[, target])) / 5
    levelVec <- c(levelVec, factorLevels)
    errVec <- c(errVec, err)
    tarVec <- c(tarVec, target)
    
    save(index, tarVec, errVec, levelVec, file = fileNames$temporaryName)
    i <- i + 1
  }
  
  # cliq <- paste(colnames(qs), collapse = '--') 
  thres <- getErrorThreshold(errVec)
  df <- data.frame(target_column = tarVec, testing_error = errVec)
  result.svm <- list(clique_index = index, details = df, threshold = thres, 
               min_error = min(errVec), avg_error = mean(errVec), expected_error_in_factor = levelVec)
  save(result.svm, file = fileNames$finalName)
  unlink(fileNames$temporaryName)
  log(paste(index, "done"))
}

makeFileNameForResultsFromSVM <- function(i, delta, alpha) {
  sharingPart <- makeFileName(i, delta, alpha)
  fn <- paste('svm', sharingPart, sep = '-')
  tfn <- paste('tmp', fn, sep='-')
  list(finalName=fn, temporaryName=tfn)
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