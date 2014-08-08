loopTrainingTestingCART <- function(clique, index, delta, alpha) {
  data <- splitData(clique)
  vector.column <- NULL
  vector.predicted.rsquare <- NULL
  
  for(targetColumn in colnames(clique)) {
    tree <- trainRegressionTree(data$trainset, targetColumn)
    pred <- predict(tree, newdata = data$testset)
    rsquare <- getPredictedCoefficientDetermination(data$testset, pred, targetColumn)    
    
    vector.predicted.rsquare <- c(vector.predicted.rsquare, rsquare)
    vector.column <- c(vector.column, targetColumn)
  }
  
  result.cart <- data.frame(clique_index = index, outcome_column = vector.column, 
                            predicted_r_square = vector.predicted.rsquare)
  
  fn <- makeFileNameForResultsFromCART(index, delta, alpha)
  save(result.cart, file = fn)  
}

trainRegressionTree <- function(trainset, target) {
  formulastr <- as.formula(paste(target, '~.'))
  tr.control = trainControl(method = "cv", number = 10)  ## cross-valiadation with 10 folds
  cp.grid = expand.grid( .cp = (0:10)*0.001)  ## cp values
  tr = train(formulastr, data = data$trainset, method = 'rpart', 
             trControl = tr.control, tuneGrid = cp.grid)
  return(tr)
}

getPredictedCoefficientDetermination <- function(testset, prediction, target) {
  SSE <- sum((testset[, target] - prediction)^2)
  SST <- sum((testset[, target] - mean(testset[, target]))^2)
  return(1 - SSE/SST)
}

makeFileNameForResultsFromCART <- function(i, delta, alpha) {
  sharingPart <- makeFileName(delta, alpha, i)
  fn <- paste('cart', sharingPart, sep = '-')
  return(fn)
}