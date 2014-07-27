source('log.R')

# For-loop in a quasi clique; for each column as outcome value train a SVM model
# using training dataset and apply this model on testing dataset
loopTrainingTestingSVM <- function(qs, index, delta, alpha) {
  fileNames <- makeFileNameForResultsFromSVM(index, delta, alpha)
  errVec <- NULL
  tarVec <- NULL
  levelVec <- NULL
  
  i <- 1  ## log
  l <- length(colnames(qs))
  for(target in colnames(qs)) {   
    log(paste(index, paste(i, l, sep='/'), target))
    data <- takeSamples(qs, target) 
    model <- trainSVMModel(data$training, target)
    testingError <- getTestingError(model, data$testing)
    expected.level.ratio <- (1 - 1/length(levels(qs[, target]))) / 2
    level.vector <- c(levelVec, expected.level.ratio)
    error.vector <- c(errVec, testingError)
    targart.vector <- c(tarVec, target)
    save(index, targart.vector, error.vector, level.vector, file = fileNames$temporaryName)
    i <- i + 1
  }
  
  thres <- getErrorThreshold(error.vector)
  df <- data.frame(target_column = targart.vector, testing_error = error.vector, expected_error_in_factor = level.vector)
  result.svm <- list(clique_index = index, details = df, threshold = thres, 
                     min_error = min(error.vector), avg_error = mean(error.vector))
  save(result.svm, file = fileNames$finalName)  ## result.svm variable can be reloaded in post analysis
  unlink(fileNames$temporaryName)
  log(paste(index, "done"))
}

makeFileNameForResultsFromSVM <- function(i, delta, alpha) {
  sharingPart <- makeFileName(delta, alpha, i)
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