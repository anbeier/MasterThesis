library(e1071)
library(caTools)
library(caret)
#library(randomForest)

# Naive bayes
loopTrainNaiveBayesForOneClique <- function(qs, index, fileIndicator) {

  fileName <- makeFileNameForExperimentResults(fileIndicator, 'bayes')
  if (file.exists(fileName)) {
    log(paste('skipped bayes for clique', index, '. File', fileName, 'already exists'))
    return(NULL)
  }
  df <- NULL

  log(paste('training naive bayes and testing on quasi-clique:', index, sep=' '))

  for(target in colnames(qs)) {
    #data <- takeSmallSamples(qs, target)
    data <- takeSmallSamples(qs, target)
    model <- trainNaiveBayes(data$training, target)
    pred <- predict(model, data$testing)
    df <- rbind(df, data.frame(target = target,
                               actual = data$testing[, target],
                               predicted = pred))
  }

  result.bayes <- list(index = index, result = df)
  save(result.bayes, file = fileName)
  #return(result.bayes)
}

# Support vector machine
loopTrainSVMForOneClique <- function(qs, index, fileIndicator) {
  fileName <- makeFileNameForExperimentResults(fileIndicator, 'svm')
  if (file.exists(fileName)) {
    log(paste('skipped svm for clique', index, '. File', fileName, 'already exists'))
    return(NULL)
  }
  df <- NULL
  # target.vector <- NULL

  log(paste('training svm and testing on quasi-clique: ', index, sep=' '))

  for(target in colnames(qs)) {
    data <- takeSmallSamples(qs, target)
    log(paste('training svm clique: ', index, ' for target ', target, ' with ', nrow(data$training), '/', nrow(data$testing),' samples', sep=''))
    model <- trainSupportVectorMachine(data$training, target)
    log(paste('predicting svm clique: ', index, ' for target ', target, ' with ', nrow(data$training), '/', nrow(data$testing),' samples', sep=''))
    pred <- predict(model, newdata = data$testing)
    log(paste('saving svm clique: ', index, ' for target ', target, sep=''))
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

# Classification and Regression Tree
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
  #return(result.cart)
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

# Multi-layer Perceptron
loopTrainMultilayerPerceptron <- function(qs, index, fileIndicator) {
  fileName <- makeFileNameForExperimentResults(fileIndicator, 'percp')
  df <- NULL
  for(target in colnames(qs)) {
    data <- prepareDataForPerceptron(qs, target)
    w <- monmlp.fit(data$independentVal, data$dependentVal, hidden1 = 10)
    p <- monmlp.predict(x = data$testset, weights = w)
    df <- rbind(df, data.frame(target = target,
                               actual = data$actualVal,
                               predicted = p))
  }
  result.percp <- list(index = index, result = df)
  save(result.percp, file = fileName)
  return(result.percp)
}

# Bayesian Regularized Neural Networks
loopTrainNeuralNetwork <- function(qs, index, fileIndicator) {
  fileName <- makeFileNameForExperimentResults(fileIndicator, 'nn')
  df <- NULL
  for(targetColumn in colnames(qs)) {
    data <- takeSamples(qs, targetColumn)
    model <- trainNeuralNetwork(data$training, targetColumn)
    pred <- predictNeuralNetwork(model, data$testing, targetColumn)
    df <- rbind(df, data.frame(target = targetColumn,
                               actual = data$testing[, targetColumn],
                               predicted = pred))
  }
  result.nn <- list(index = index, result = df)
  save(result.nn, file = fileName)
  return(result.nn)
}

loopTrainRandomForest <- function(qs, index, fileIndicator) {
  fileName <- makeFileNameForExperimentResults(fileIndicator, 'rf')
  df <- NULL
  for(targetColumn in colnames(qs)) {
    data <- takeSamples(qs, targetColumn)
    model <- trainRandomForest(data$training, targetColumn)
    pred <- predictRF(model, data$testing, targetColumn)
    df <- rbind(df, data.frame(target = targetColumn,
                               actual = data$testing[, targetColumn],
                               predicted = pred))
  }
  result.rf <- list(index = index, result = df)
  save(result.rf, file = fileName)
  return(result.rf)
}

trainNeuralNetwork <- function(trainset, columnname) {
  #estimators <- paste(setdiff(colnames(trainset), columnname), collapse = '+')
  #formulastr <- as.formula(paste(columnname, estimators, sep='~'))
  formulastr <- as.formula(paste(columnname, '~.'))
  #nn <- neuralnet(formulastr, trainset, hidden = 10, algorithm = 'backprop',
  #                learningrate = 0.01,linear.output = FALSE)
  model <- train(formulastr, data = trainset, method = 'brnn')
  return(model$finalModel)
}

predictNeuralNetwork <- function(model, testset, target) {
  testset[, target] <- NULL
  predict(model, testset)
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
  #tr <- train(formulastr, data = trainset, method = 'rpart')
  return(tr$finalModel)
}

trainLinearModel <- function(trainset, columnname) {
  formulastr <- as.formula(paste(columnname, '~.'))
  model <- lm(formulastr, data = trainset)
  return(model)
}

trainRandomForest <- function(trainset, target) {
  formulastr <- as.formula(paste(target, '~.'))
  outcomeVal <- trainset[, target]
  independentVal <- trainset
  independentVal[, target] <- NULL
  bestmtry=tuneRF(independentVal, outcomeVal, ntreeTry=100, stepFactor=1.5,
                  improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
  rf <-randomForest(formulastr, data=trainset, mtry=2, ntree=1000,
                    keep.forest=TRUE, importance=TRUE, proximity=TRUE)
  return(rf)
}

predictRF <- function(model, testset, target) {
  testset[, target] <- NULL
  predict(model, newdata = testset)
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
  
  testing = takeSmallSamples2(testing, targetcol)
  
  list(training = training, testing = testing)
}

takeSmallSamples2 <- function(dat, targetcol) {
  samples <- dat
  ratio <- 0.6
  alpha <- 0.5
  trainingSize <- nrow(samples)^alpha + 1000
  if (nrow(samples) > trainingSize) {
    ratio <- trainingSize / nrow(samples)
  }
  #log(paste("choosing ratio", ratio, "sample training size", trainingSize))
  split <- sample.split(samples[, targetcol], SplitRatio = ratio)
  training <- subset(samples, split == TRUE)
  return(training)
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
    trainset <- trainset[1: (nrow(trainset) - diff), ]
  } else if(nrow(trainset) < nrow(testset)) {
    diff <- nrow(testset) - nrow(trainset)
    testset <- testset[1: (nrow(testset) - diff), ]
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

makeFileIndicatorForPruning <- function(fileIndicator, removedColumnInd) {
  toAdd = paste(paste('-c', removedColumnInd, sep=''), '.rdata', sep='')
  newFileInd = gsub('.rdata', toAdd, fileIndicator, fixed = T)
  return(newFileInd)
}

transferToCsv <- function(folderName, method) {
  fileNames <- list.files(paste(folderName, method, sep='/'), full.names = TRUE)

  for(fn in fileNames) {
    load(fn)
    filestring = unlist(strsplit(fn, split='qs'))[1]
    newfn = paste(filestring, paste(result.svm$index, '.csv', sep=''), sep='')
    write.csv(result.svm$result, file=newfn, quote=F, row.names=F)
  }
}

makeFileNameForExperimentResults <- function(fileIndicator, method) {
  fn <- paste(method, fileIndicator, sep = '-')
  return(fn)
}

pruneColumns <- function(cliqueIndex, clique, targetColumn, cliqueMCC, classifier, checkedColumns=NULL) {
  if (ncol(clique) == 2) {
    return(NULL)
  }
  targetIndex = grep(targetColumn, names(clique))
  candidateIndices = 1:ncol(clique)
  candidateIndices = candidateIndices[candidateIndices != targetIndex]

  prunedColumns = NULL
  minimalColumns = NULL
  modelFunction = NULL
  if(classifier == 'bayes') {
    modelFunction = trainNaiveBayes
  } else if (classifier == 'svm') {
    modelFunction = trainSupportVectorMachine
  }

  for(i in candidateIndices) {
    df = clique
    df[,i] = NULL
    colString = paste(sort(names(df)), collapse = '|')
#     if (colString %in% checkedColumns) {
#       print(paste("########skipping", colString))
#       next
#     }

    data <- takeSmallSamples(df, targetColumn)
    #print(paste('train with', nrow(data$training), 'rows', sep=' '))
    model <- modelFunction(data$training, targetColumn)
    
    # reduce numbers of testing data, use training data as testing data
    testData = takeSmallSamples(data$testing, targetColumn)$training
    
    #print(paste('predict with', nrow(data$training), 'rows', sep=' '))
    pred <- predict(model, testData)
    mcc = computeMCCExtern(data.frame(actual = testData[, targetColumn],
                                      predicted = pred))
    
    checkedColumns = c(checkedColumns, colString)
    
    if(mcc >= cliqueMCC) {
      print(paste("mcc ok for:", colString))
      super = pruneColumns(cliqueIndex, df, targetColumn, cliqueMCC, classifier, checkedColumns = checkedColumns)
      if (!is.null(super$prunedColumns)) {
        prunedColumns = c(prunedColumns, super$prunedColumns)
        minimalColumns = c(minimalColumns, super$minimalColumns)
        checkedColumns = super$checkedColumns
      } else {
        prunedColumns = c(prunedColumns, sort(names(df)))
        minimalColumns = c(minimalColumns, colString)
      }
    } else {
      print(paste("mcc reduced for:", colString))
    }
  }
  
  if (!is.null(prunedColumns)) {
    prunedColumns = sort(unique(prunedColumns))
  }
  
  if (!is.null(minimalColumns)) {
    minimalColumns = sort(unique(minimalColumns))
  }
  
  return(list(index=cliqueIndex,
              target=targetColumn,
              prunedColumns=prunedColumns,
              minimalColumns=minimalColumns,
              checkedColumns=checkedColumns))
}

directOutput <- function(cliqueIndex, targetColumn, df) {
  data.frame(index=cliqueIndex,
             target=targetColumn,
             pruned=paste(sort(names(df)), sep='|'))
}

loopPruneColumns <- function(originQS, dfIdentifiedCols, classifier) {
  df = NULL
  for(i in 1:nrow(dfIdentifiedCols)) {
    print(paste("examine target column", as.character(dfIdentifiedCols[i,"target"])))
    ls = pruneColumns(dfIdentifiedCols[i,"index"], originQS,
                      as.character(dfIdentifiedCols[i,"target"]),
                      as.numeric(dfIdentifiedCols[i,"mcc"]),
                      classifier)

    pruned = ls$prunedColumns
    minimal = ls$minimalColumns
    pruningPossible = !is.null(pruned)
    
    if(is.null(pruned)) {
      pruned = names(originQS)
    }
    
    if(is.null(minimal)) {
      minimal = paste(names(originQS), collapse="|")
    }

    df = rbind(df, data.frame(index=ls$index,
                              target=ls$target,
                              pruningPossible=pruningPossible,
                              pruned=paste(pruned, collapse="|"),
                              minimal=paste(minimal, collapse=",")))
  }
  
  return(df)
}

launchPruning_census <- function(datasetName, data, delta, qs_index, classifier) {
  cliques = NULL
  if(datasetName == 'census') {
    fqsFile = paste('census_fqs_delta', paste(delta, '_alpha0.5.txt', sep=''), sep='')
    cliques <- readingQuasiCliques(fqsFile) 
  } else if(datasetName == 'tpch') {
    x = paste(0, delta*10, sep='')
    fqsFile = paste('DOCCO_FQS_', x, '_yichi.txt', sep='')
    cliques <- readTPCHCliques(fqsFile)
  }
  
  qs = getOneClique(data, cliques, qs_index)
  
  ls = readMCCResults(datasetName, delta)
  df = NULL
  if(classifier=='bayes') {
    df = ls$bayes[ls$bayes$index == qs_index,]
  }
  if(classifier=='svm') {
    df = ls$svm[ls$svm$index == qs_index,]
  }
  df$mcc = round(as.numeric(as.character(df$mcc)), 4)
  
  loopPruneColumns(qs, df, classifier)
}
