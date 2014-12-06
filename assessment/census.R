library(parallel)

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
                              fqsFile = fqsfp, delta = d, alpha = a, instance) {
  census <- getCensusData(csvFile, colnameFile)
  cliqueGroup <- readingQuasiCliques(fqsFile) 
  # Take the Xst quasi clique as sample data
  sampleClique <- getOneClique(census, cliqueGroup, instance) 
  fileIndicator <- makeFileIndicator(delta, alpha, instance)
  
  result.svm <- loopTrainSVMForOneClique(sampleClique, instance, fileIndicator)
  result.bayes <- loopTrainNaiveBayesForOneClique(sampleClique, instance, fileIndicator)
  
  mcc.svm <- returnMCCTable(result.svm$result)
  mcc.bayes <- returnMCCTable(result.bayes$result)
  
  mcc <- merge(mcc.svm, mcc.bayes, by = 'TargetColumn')
  names(mcc) = c('TargetColumn', 'MCC_SVM', 'MCC_Bayes')
  mcc$QuasiClique = instance
  subset(mcc, select=c('QuasiClique','TargetColumn','MCC_SVM','MCC_Bayes'))
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
  #loopTrainNaiveBayesForOneClique(clique, cliqueIndex, fileIndicator)
  rm(list = ls())  ## clear workspace
}

worker <- function(input) {
  log(paste("processing", input$index))
  tryCatch({
    loopTrainSVMForOneClique(input$clique, input$index, input$filename)
    #loopTrainNaiveBayesForOneClique(input$clique, input$index, input$filename)
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
  
  cores <- 1 # be friendly and dont use all cores
  
  res <- mclapply(inputs, worker, mc.cores = cores)
  return(res)
}

saveOutputInCSV <- function(folderName, method) {
  fileNames <- list.files(paste(folderName, method, sep='/'), full.names = TRUE) 
  
  for(fn in fileNames) {
    load(fn)
    filestring = unlist(strsplit(fn, split='qs'))[1]
    newfn = paste(filestring, paste(result.svm$index, '.csv', sep=''), sep='')
    write.csv(result.svm$result, file=newfn, quote=F, row.names=F)
  }
}
