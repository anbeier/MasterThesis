library(parallel)
library(e1071)
library(caTools)
library(arules)
source('censusPreprocess.R')
source('censusSVM.R')
source('censusAssociationRules.R')
source('log.R')

csvfp = 'census.csv'
colnamefp = 'census_colnames.txt'
fqsfp = 'census_fqs_delta0.7_alpha0.5.txt'
d = 0.7
a = 0.5


main <- function(cliqueIndex, delta = d, alpha = a, 
                 csvFile = csvfp, colnameFile = colnamefp, fqsFile = fqsfp) {
  
  data <- getCensusData(csvFile, colnameFile)
  allCliques <- readingQuasiCliques(fqsFile) 
  # A loop where models for each clique, i.e. for each column in this clique are built.
  clique <- getOneClique(data, allCliques, cliqueIndex)  
  # Save result of each clique in a 'rdata' file
  fileIndicator <- makeFileIndicator(delta, alpha, cliqueIndex)
  loopTrainSVMForOneClique(clique, cliqueIndex, fileIndicator)
  loopAssociationRules(clique, cliqueIndex, delta, alpha)
  rm(list = ls())  ## clear workspace
}

worker <- function(input) {
  #log(paste("processing", input$index))
  tryCatch({
    loopTrainSVMForOneClique(input$clique, input$index, input$filename)
    #loopAssociationRules(input$clique, input$index, input$delta, input$alpha)
  }, error=function(e) {
    #log(paste("error processing", input$index, e))
    e
  })
}

parallelMain <- function(indexStart, indexEnd, delta = d, alpha = a, 
                         csvFile = csvfp, colnameFile = colnamefp, fqsFile = fqsfp) {
  
  census <- getCensusData(csvFile, colnameFile)
  cliques <- readingQuasiCliques(fqsFile)
  
  inputs <- lapply(seq(indexStart, indexEnd), function(i) {
    #log(paste("prepare input for", i))
    
    list(index = i, 
         clique = getOneClique(census, cliques, i), 
         filename = makeFileIndicator(delta, alpha, i))
  })
  
  cores <- 2 # be friendly and dont use all cores
  
  res <- mclapply(inputs, worker, mc.cores = cores)
  return(res)
}
