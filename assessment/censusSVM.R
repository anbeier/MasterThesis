source('log.R')

# For-loop in a quasi clique; for each column as outcome value train a SVM model
# using training dataset and apply this model on testing dataset
loopTrainingTestingSVM <- function(qs, index, delta, alpha) {
  fileNames <- makeFileNameForResultsFromSVM(index, delta, alpha)
  target.vector <- NULL
  acc.vector <- NULL
  dor.vector <- NULL
  f1.vector <- NULL
  # level.vector <- NULL
  
  i <- 1  ## log
  l <- length(colnames(qs))
  for(target in colnames(qs)) {   
    log(paste(index, paste(i, l, sep='/'), target))
    
    data <- takeSamples(qs, target) 
    model <- trainSupportVectorMachine(data$training, target) 
    pred <- predict(model, newdata = data$testing)
    stats <- getStatistic(data$testing, pred, target)
    # testingError <- getTestingError(model, data$testing)
    # expected.level.ratio <- (1 - 1/length(levels(qs[, target]))) / 2.5
    # level.vector <- c(level.vector, expected.level.ratio)
    # error.vector <- c(error.vector, testingError)
    target.vector <- c(target.vector, target)
    acc.vector <- c(acc.vector, stats$ACC)
    dor.vector <- c(dor.vector, stats$DOR)
    f1.vector <- c(f1.vector, stats$F1Score)
    # save(index, targart.vector, error.vector, level.vector, file = fileNames$temporaryName)
    save(index, target.vector, acc.vector, dor.vector, f1.vector, file = fileNames$temporaryName)
    i <- i + 1
  }
  
  # thres <- getErrorThreshold(error.vector)
  # df <- data.frame(target_column = targart.vector, testing_error = error.vector, expected_error_in_factor = level.vector)
  # result.svm <- list(clique_index = index, details = df, threshold = thres, 
  #                    min_error = min(error.vector), avg_error = mean(error.vector))
  result.svm <- data.frame(clique_index = index, target_column = target.vector, 
                           accurary = acc.vector, diagnostic_odds_ratio = dor.vector, f1_score = f1.vector)
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

trainSupportVectorMachine <- function(training, target) {
  formulastr <- as.formula(paste(target, "~.")) 
  # model <- ksvm(formulastr, data = qs, type = "C-bsvc", kernel = "rbfdot", kpar = list(sigma = 0.1), 
  #               C = 10, prob.model = TRUE)
  # model <- svm(formulastr, data = qs)
  obj <- tune(svm, formulastr, data = training, ranges = list(gamma = c(0.1, 1, 10), cost = c(1, 10, 50)), 
              tunecontrol = tune.control(sampling = "fix"))
  
  list(model = obj$best.model, target = target)
  return(obj$best.model)
}

# Tested!
getConfusionMatrix <- function(data, classLabel) {
  condition.pos <- data[data$actual == classLabel, ]  ## data frame of condition positives
  condition.neg <- data[!data$actual == classLabel, ]  ## data frame of condition negatives
  tp <- sum(condition.pos$predicted == classLabel)
  fn <- sum(!condition.pos$predicted == classLabel)
  tn <- sum(!condition.neg$predicted == classLabel)
  fp <- sum(condition.neg$predicted == classLabel)
  list(tp = tp, fn = fn, tn = tn, fp = fp)
}

getStatistic <- function(testset, predicted, targetColumn) {
  df <- data.frame(actual = testset[, targetColumn], predicted = predicted)
  classes <- as.list(levels(testset[, targetColumn]))
  
  ls.acc <- lapply(classes, 
                   function(x, data = df) {
                     stat <- getConfusionMatrix(data, x)
                     return((stat$tp + stat$tn) / nrow(data))
                   })
  avg.acc <- Reduce('+', ls.acc) / length(classes)
  
  ls.dor <- lapply(classes, 
                   function(x, data = df) {
                     stat <- getConfusionMatrix(data, x)
                     ## If both false negatives and false positives are zero, then the test is perfect, 
                     ## but if only one is, this ratio does not give a usable measure.
                     if((!stat$fn == 0 & !stat$fp == 0) | (stat$fn == 0 & stat$fp == 0)) {
                       if(!(stat$tp + stat$fn) == 0 & !(stat$tn + stat$fp) == 0) {
                         sensitivity <- stat$tp / (stat$tp + stat$fn)
                         specificity <- stat$tn / (stat$tn + stat$fp)
                         LR.pos <- sensitivity / (1 - specificity)  ## positive likelihood ratio
                         LR.neg <- (1 - sensitivity) / specificity   ## nagative likelihood ratio
                         return(LR.pos / LR.neg)  ## diagnostic odds ratio for class x
                       } else {
                         0
                       }
                     } else {
                       0
                     }
                   })
  # ls.dor[is.na(ls.dor)] <- NULL  ## remove NA values from the list
  avg.dor <- Reduce('+', ls.dor) / length(classes)  
  
  ls.f1 <- lapply(classes, 
                  function(x, data = df) {
                    stat <- getConfusionMatrix(data, x)
                    ## An F1 score reaches its best value at 1 and worst score at 0.
                    if(!(stat$tp + stat$fp) == 0 & !(stat$tp + stat$fn) == 0) {
                      precision <- stat$tp / (stat$tp + stat$fp)
                      recall <- stat$tp / (stat$tp + stat$fn)
                      f1 <- 2 * precision * recall / (precision + recall)
                      return(2 * precision * recall / (precision + recall))
                    } else {
                      0
                    }                  
                  })
  # ls.f1[is.na(ls.f1)] <- NULL
  avg.f1 <- Reduce('+', ls.f1) / length(classes)
  
  list(ACC = avg.acc, DOR = avg.dor, F1Score = avg.f1)
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