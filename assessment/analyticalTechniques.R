# Naive bayes
loopTrainNaiveBayesForOneClique <- function(qs, index, fileIndicator) {
  
  fileName <- makeFileNameForExperimentResults(fileIndicator, 'bayes')
  
  df <- NULL
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
  return(result.bayes)
}

# Support vector machine
loopTrainSVMForOneClique <- function(qs, index, fileIndicator) {
  fileName <- makeFileNameForExperimentResults(fileIndicator, 'svm')
  df <- NULL
  # target.vector <- NULL
  
  for(target in colnames(qs)) {   
    data <- takeSmallSamples(qs, target) 
    model <- trainSupportVectorMachine(data$training, target) 
    pred <- predict(model, newdata = data$testing)
    # data frame with 3 columns: 
    # target (outcome value), actual (actual values), predicted (predicted values)
    df <- rbind(df, data.frame(target = target, 
                               actual = data$testing[, target], 
                               predicted = pred))
    # stats <- getStatistic(data$testing, pred, target)
    # testingError <- getTestingError(model, data$testing)
    # expected.level.ratio <- (1 - 1/length(levels(qs[, target]))) / 2.5
    # level.vector <- c(level.vector, expected.level.ratio)
    # error.vector <- c(error.vector, testingError)
    # target.vector <- c(target.vector, target)
    # save(index, targart.vector, error.vector, level.vector, file = fileNames$temporaryName)
    # save(index, target.vector, acc.vector, dor.vector, f1.vector, file = fileNames$temporaryName)
  }
  result.svm <- list(index = index, result = df)
  save(result.svm, file = fileName) 
  return(result.svm)
  # thres <- getErrorThreshold(error.vector)
  # df <- data.frame(target_column = targart.vector, testing_error = error.vector, expected_error_in_factor = level.vector)
  # result.svm <- list(clique_index = index, details = df, threshold = thres, 
  #                    min_error = min(error.vector), avg_error = mean(error.vector))
  #result.svm <- data.frame(clique_index = index, target_column = target.vector, 
  #                         accurary = acc.vector, diagnostic_odds_ratio = dor.vector, f1_score = f1.vector)
}

loopTrainClassificationRegressionTree <- function(qs, index, fileIndicator) {
  fileName <- makeFileNameForExperimentResults(fileIndicator, 'cart')
  df <- NULL
  for(target in colnames(qs)) {
    data <- takeSamples(qs, target)
    model <- trainTree(data$training, target)
    pred <- predict(model, data$testing)
    df <- rbind(df, data.frame(target = target, 
                               actual = data$testing[, target], 
                               predicted = pred))
  }
  result.cart <- list(index = index, result = df)
  save(result.cart, file = fileName) 
  return(result.cart)
}

# Linear regression
loopTrainLinearRegressionForOneClique <- function(clique, index, fileIndicator) {
  fileName <- makeFileNameForExperimentResults(fileIndicator, 'lm')
  df <- NULL
  for(targetColumn in colnames(clique)) {
    data <- takeSamples(clique, targetColumn)
    model <- trainLinearModel(data$training, targetColumn)
    pred <- predict(model, newdata = data$testing)
    df <- rbind(df, data.frame(target = targetColumn, 
                               actual = data$testing[, targetColumn], 
                               predicted = pred))
  }
  result.lm <- list(index = index, result = df)
  save(result.lm, file = fileName) 
  return(result.lm)
}

loopTrainMultilayerPerceptron <- function(qs, index, fileIndicator) {
  fileName <- makeFileNameForExperimentResults(fileIndicator, 'percp')
  df <- NULL
  for(target in colnames(qs)) {
    data <- prepareDataForPerceptron(qs, target)
    w <- monmlp.fit(data$independentVal, data$dependentVal, hidden1 = 10)
    p <- monmlp.predict(x = data$testset, weights = w)
    df <- rbind(df, data.frame(target = targetColumn, 
                               actual = data$actualVal, 
                               predicted = p))
  }
  result.percp <- list(index = index, result = df)
  save(result.percp, file = fileName) 
  return(result.percp)
}

trainSupportVectorMachine <- function(training, target) {
  formulastr <- as.formula(paste(target, "~.")) 
  # model <- ksvm(formulastr, data = qs, type = "C-bsvc", kernel = "rbfdot", kpar = list(sigma = 0.1), 
  #               C = 10, prob.model = TRUE)
  # model <- svm(formulastr, data = qs)
  obj <- tune(svm, formulastr, data = training, ranges = list(gamma = c(0.1, 1, 10), cost = c(1, 10, 50)), 
              tunecontrol = tune.control(sampling = "fix"))
  return(obj$best.model)
}

trainNaiveBayes <- function(trainset, target) {
  formulastr <- as.formula(paste(target, "~.")) 
  model <- naiveBayes(formulastr, data = trainset)
  return(model)
}

# With cross validation with 10 folds
trainTree <- function(trainset, target) {
  formulastr <- as.formula(paste(target, "~.")) 
  tr.control = trainControl(method = "cv", number = 10)  ## cross-valiadation with 10 folds
  cp.grid = expand.grid( .cp = (0:10)*0.001)  ## cp values
  tr = train(formulastr, data = trainset, method = 'rpart', 
             trControl = tr.control, tuneGrid = cp.grid) 
  tr <- train(formulastr, data = trainset, method = 'rpart')
  return(tr$finalModel)
}

trainLinearModel <- function(trainset, columnname) {
  formulastr <- as.formula(paste(columnname, '~.'))
  model <- lm(formulastr, data = trainset)
  return(model)
}

# Extract a few samples from a quasi clique w.r.t a specific target column
takeSmallSamples <- function(qs, targetcol) {
  # samples <- qs[sample(nrow(qs), replace = FALSE, size = 0.01 * nrow(qs)), ]  
  samples <- qs
  ratio <- 0.6
  alpha <- 0.5
  trainingSize <- nrow(samples)^alpha + 1000
  if (nrow(samples) > trainingSize) {
    ratio <- trainingSize / nrow(samples)
  }
  #log(paste("choosing ratio", ratio, "sample training size", trainingSize))
  split <- sample.split(samples[, targetcol], SplitRatio = ratio)
  training <- subset(samples, split == TRUE)
  testing <- subset(samples, split == FALSE) 
  list(training = training, testing = testing)
}

# Will take 60% of data as training set and 40% as testing set w.r.t a specific target column
takeSamples <- function(qs, targetcol) {
  inTrain <- createDataPartition(y = qs[, targetcol], p = 0.6, list=FALSE)
  list(training = qs[inTrain,], 
       testing = qs[-inTrain,])
}

prepareDataForPerceptron <- function(qs, targetColumn) {
  inTrain <- createDataPartition(y = qs[, targetColumn], p = 0.5, list=FALSE)
  trainset <- qs[inTrain,]
  testset <- qs[-inTrain,]
  
  if(nrow(trainset) > nrow(testset)) {
    diff <- nrow(trainset) - nrow(testset)
    trainset <- trainset[1: nrow(trainset) - diff, ]
  } else if(nrow(trainset) < nrow(testset)) {
    diff <- nrow(testset) - nrow(trainset)
    testset <- testset[1: nrow(testset) - diff, ]
  }
  
  if(nrow(trainset) == nrow(testset)) {
    dependentVal <- as.matrix(trainset[, targetColumn])
    trainset[, targetColumn] <- NULL
    independentVal <- as.matrix(trainset)
    actualVal <- testset[, targetColumn]
    testset[, targetColumn] <- NULL
    testset <- as.matrix(testset)
  }
  
  list(independentVal = independentVal, dependentVal = dependentVal, 
       testset = testset, actualVal = actualVal)
}

# Will return a sharing part of all file names
makeFileIndicator <- function(delta, alpha, i) {
  prefix <- paste(paste('delta', delta, sep = ''), paste('alpha', alpha, sep = ''), sep = '-')
  string <- paste(prefix, paste('qs', i, sep = ''), sep = '-')
  string <- paste(string, 'rdata', sep = '.')
  return(string)
}

makeFileNameForExperimentResults <- function(fileIndicator, method) {
  fn <- paste(method, fileIndicator, sep = '-')
  return(fn)
}