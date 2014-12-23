library(parallel)

source('censusPreprocess.R')
source('analyticalTechniques.R')
source('censusPostAnalysis.R')
source('log.R')

source('tpch.R')

d = 0.7
a = 0.5

setCensusParameters <- function() {
  list(csvFile = 'census.csv',
       colnameFile = 'census_colnames.txt',
       fqsFile = 'census_fqs_delta0.7_alpha0.5.txt')
}

setTPCHParameters <- function() {
  load('tpch.rdata')
  list(data = tpch,
       fqsFile = 'DOCCO_FQS_07.txt')
}

tryDifferentTechniques <- function(csvFile = setCensusParameters()$csvFile, 
                                   colnameFile = setCensusParameters()$colnameFile,
                                   fqsFile = setCensusParameters()$fqsFile,
                                   delta = d, alpha = a, instance) {
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
    #loopTrainSVMForOneClique(input$clique, input$index, input$filename)
    loopTrainNaiveBayesForOneClique(input$clique, input$index, input$filename)
    log(paste("done processing", input$index))
  }, error=function(e) {
    log(paste("error processing", input$index, e))
    e
  })
}

parallelMainCensus <- function(indexStart, indexEnd, cores=2, delta = d, alpha = a, 
                         csvFile = setCensusParameters()$csvFile, 
                         colnameFile = setCensusParameters()$colnameFile,
                         fqsFile = setCensusParameters()$fqsFile) {
  
  log(paste('parallelMain', indexStart, '->', indexEnd, 'using', cores, 'cores'))
  census <- getCensusData(csvFile, colnameFile)
  cliques <- readingQuasiCliques(fqsFile)

  if (indexEnd > length(cliques)) {
    indexEnd <- length(cliques)
    log(paste('indexEnd changed to ', indexEnd, sep=''))
  }
  
  inputs <- lapply(seq(indexStart, indexEnd), function(i) {
    log(paste("prepare input for", i))
    
    clique <- getOneClique(census, cliques, i)
    log(paste('clique: ', i, ' size:', nrow(clique), 'x', ncol(clique), ' items: ', nrow(clique)*ncol(clique), sep=''))
    list(index = i,
         clique = clique,
         filename = makeFileIndicator(delta, alpha, i))
  })
  
  res <- mclapply(inputs, worker, mc.cores = cores)
  return(res)
}

parallelMainTPCH <- function(indexStart, indexEnd, cores=2, delta = d, alpha = a,
                             tpchData = setTPCHParameters()$data
                             fqsFile = setTPCHParameters()$fqsFile) {
  
  log(paste('parallelMain', indexStart, '->', indexEnd, 'using', cores, 'cores'))
  
  cliques <- readTPCHCliques(fqsFile)
  
  if (indexEnd > length(cliques)) {
    indexEnd <- length(cliques)
    log(paste('indexEnd changed to ', indexEnd, sep=''))
  }
  
  inputs <- lapply(seq(indexStart, indexEnd), function(i) {
    log(paste("prepare input for", i))
    
    clique <- getOneClique(tpchData, cliques, i)
    log(paste('clique: ', i, ' size:', nrow(clique), 'x', ncol(clique), ' items: ', nrow(clique)*ncol(clique), sep=''))
    list(index = i,
         clique = clique,
         filename = makeFileIndicator(delta, alpha, i))
  })
  
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

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
    idx <- as.integer(args[1])
    parallelMain(idx, idx)
}
