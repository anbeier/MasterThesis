# For-loop in one quasi clique
# For each column, as outcome value, train a SVM model and apply this model on testing set











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