loopTrainNaiveBayesForOneClique <- function(qs, index, fileIndicator) {
  
  fileName <- makeFileNameForExperimentResults(fileIndicator, 'bayes')
  
  for(target in colnames(qs)) {
    data <- takeSamples(qs, target)
    model <- trainNaiveBayes(data$training, target)
    pred <- predict(model, data$testing)
    df <- rbind(df, data.frame(target = target, 
                               actual = data$testing[, target], 
                               predicted = pred))
  }
  
  result.bayes <- list(index = index, result = df)
  save(result.bayes, file = fileName)
}

trainNaiveBayes <- function(trainset, targetColumn) {
  formulastr <- as.formula(paste(targetColumn, "~.")) 
  model <- naiveBayes(formulastr, data = trainset)
  return(model)
}

