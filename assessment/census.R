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

main <- function(cliqueIndex, delta = 0.7, alpha = 0.5, 
                 csvFile = csvfp, colnameFile = colnamefp, fqsFile = fqsfp) {
  
  data <- getCensusData(csvFile, colnameFile)
  allCliques <- readingQuasiCliques(fqsFile) 
  # A loop where models for each clique, i.e. for each column in this clique are built.
  clique <- getOneClique(data, allCliques, cliqueIndex)  
  # save result of each clique in a RData file
  loopTrainingTestingSVM(clique, index, delta, alpha)
  loopAssociationRules(clique, index, delta, alpha)
  rm(list = ls())  ## clear workspace
}

worker <- function(input) {
  log(paste("processing", input$index))
  tryCatch({
    loopTrainingTestingSVM(input$clique, input$index, input$delta, input$alpha)
    loopAssociationRules(input$clique, input$index, input$delta, input$alpha)
  }, error=function(e) {
    log(paste("error processing", input$index, e))
    e
  })
}

parallelMain <- function(indexStart, indexEnd, delta = 0.7, alpha = 0.5, 
                         csvFile = csvfp, colnameFile = colnamefp, fqsFile = fqsfp) {
  
  census <- getCensusData(csvFile, colnameFile)
  cliques <- readingQuasiCliques(fqsFile)
  
  # A loop where models for each clique, i.e. for each column in this clique are built.
  inputs <- lapply(seq(indexStart, indexEnd), function(i) {
    log(paste("prepare input for", i))
    list(index=i, clique=getOneClique(census, cliques, i), delta=delta, alpha=alpha)
  })

  cores <- 2 # be friendly and dont use all cores
  
  res <- mclapply(inputs, worker, mc.cores = cores)
  return(res)
}
