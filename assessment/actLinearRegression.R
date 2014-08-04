loopTrainingTestingLM <- function(clique, index, delta, alpha) {
  data <- splitData(clique)
  vector.column <- NULL
  vector.rsquare <- NULL
  vector.predicted.rsquare <- NULL
  
  for(targetColumn in colnames(clique)) {
    model <- trainLinearModel(data$trainset, targetColumn)
    predicted.r.square <- getCoefficientDetermination(data, model, targetColumn)
    vector.rsquare <- c(vector.rsquare, summary(model)$r.square)
    vector.predicted.rsquare <- c(vector.predicted.rsquare, predicted.r.square)
    vector.column <- c(vector.column, targetColumn)
  }
  result.lm <- data.frame(clique_index = index, outcome_column = vector.column, 
                           r_square = vector.rsquare, predicted_r_square = vector.predicted.rsquare)
  fn <- makeFileNameForResultsFromLM(index, delta, alpha)
  save(result.lm, file = fn)
}

trainLinearModel <- function(trainset, columnname) {
  formulastr <- as.formula(paste(columnname, '~.'))
  model <- lm(formulastr, data = trainset)
  return(model)
}

getCoefficientDetermination <- function(data, model, columnname) {
  trainset <- data$trainset
  testset <- data$testset
  y <- mean(trainset[, columnname])
  prediction <- predict(model, newdata = testset)
  SSE = sum((testset[, columnname] - prediction)^2)  ## Sum of Squared Errors
  SST = sum((testset[, columnname] - y)^2)  ## Total sum of squares
  return(1 - SSE/SST)
}

makeFileName <- function(delta, alpha, i) {
  prefix <- paste(paste('delta', delta, sep = ''), paste('alpha', alpha, sep = ''), sep = '-')
  string <- paste(prefix, paste('qs', i, sep = ''), sep = '-')
  string <- paste(string, 'RData', sep = '.')
  return(string)
}

makeFileNameForResultsFromLM <- function(i, delta, alpha) {
  sharingPart <- makeFileName(delta, alpha, i)
  fn <- paste('lm', sharingPart, sep = '-')
  return(fn)
}