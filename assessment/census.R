library(parallel)
library(e1071)
library(caTools)
library(arules)

source('censusPreprocess.R')
source('analyticalTechniques.R')
source('censusPostAnalysis.R')
source('log.R')

csvfp = 'census.csv'
colnamefp = 'census_colnames.txt'
fqsfp = 'census_fqs_delta0.7_alpha0.5.txt'
d = 0.7
a = 0.5

tryDifferentTechniques <- function(csvFile = csvfp, colnameFile = colnamefp,
                              fqsFile = fqsfp, delta = d, alpha = a) {
  census <- getCensusData(csvFile, colnameFile)
  cliqueGroup <- readingQuasiCliques(fqsFile) 
  # Take the 1st quasi clique as sample data
  sampleClique <- getOneClique(census, cliqueGroup, 1) 
  fileIndicator <- makeFileIndicator(delta, alpha, 1)
  
  result.svm <- loopTrainSVMForOneClique(sampleClique, 1, fileIndicator)
  result.bayes <- loopTrainNaiveBayesForOneClique(sampleClique, 1, fileIndicator)
  
  mcc.svm <- returnMCCTable(result.svm)
  mcc.bayes <- returnMCCTable(result.bayes)
  mcc <- merge(mcc.svm, mcc.bayes, by = 'Column')
  return(mcc)
}

main <- function(cliqueIndex, delta = d, alpha = a, 
                 csvFile = csvfp, colnameFile = colnamefp, fqsFile = fqsfp) {
  
  data <- getCensusData(csvFile, colnameFile)
  allCliques <- readingQuasiCliques(fqsFile) 
  # A loop where models for each clique, i.e. for each column in this clique are built.
  clique <- getOneClique(data, allCliques, cliqueIndex)  
  # Save result of each clique in a 'rdata' file
  fileIndicator <- makeFileIndicator(delta, alpha, cliqueIndex)
  loopTrainSVMForOneClique(clique, cliqueIndex, fileIndicator)
  loopTrainNaiveBayesForOneClique(clique, cliqueIndex, fileIndicator)
  rm(list = ls())  ## clear workspace
}

worker <- function(input) {
  log(paste("processing", input$index))
  tryCatch({
    #loopTrainSVMForOneClique(input$clique, input$index, input$filename)
    loopTrainNaiveBayesForOneClique(input$clique, input$index, input$filename)
  }, error=function(e) {
    log(paste("error processing", input$index, e))
    e
  })
}

parallelMain <- function(indexStart, indexEnd, delta = d, alpha = a, 
                         csvFile = csvfp, colnameFile = colnamefp, fqsFile = fqsfp) {
  
  census <- getCensusData(csvFile, colnameFile)
  cliques <- readingQuasiCliques(fqsFile)
  
  inputs <- lapply(seq(indexStart, indexEnd), function(i) {
    log(paste("prepare input for", i))
    
    list(index = i, 
         clique = getOneClique(census, cliques, i), 
         filename = makeFileIndicator(delta, alpha, i))
  })
  
  cores <- 2 # be friendly and dont use all cores
  
  res <- mclapply(inputs, worker, mc.cores = cores)
  return(res)
}
