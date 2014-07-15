# returns a quasi clique by giving its index
library(parallel)
source('censusPreprocess.R')
source('censusSVM.R')

getCsvFile <- function() { "census.csv" }
getColnameFile <- function() { "census_colnames.txt" }
getFqsFile <- function() { "census_fqs_delta0.7_alpha0.5.txt" }

main <- function(index, alpha = 0.5, csvFile = getCsvFile(), colnameFile = getColnameFile(), fqsFile = getFqsFile()) {
  
  census <- getDataset(csvFile, colnameFile, alpha)
  cliques <- readingQuasiCliques(census, fqsFile)
  
  # A loop where models for each clique, i.e. for each column in this clique are built.
  clique <- getOneClique(census, cliques, index)  
  res <- loopTrainingTesting(clique)
  return(res)
}

worker <- function(input) {
  print(paste("processing", input$index))
  source('censusPreprocess.R')
  source('censusSVM.R')
  ret <- tryCatch({
    result <- loopTrainingTesting(input$clique)
    list(index=input$index, result=result)
  }, error=function(e) {
    print(paste("error processing", input$index, e))
  })
  ret
}

parallelMain <- function(indexStart, indexEnd, alpha = 0.5, csvFile = getCsvFile(), colnameFile = getColnameFile(), fqsFile = getFqsFile()) {
  
  census <- getDataset(csvFile, colnameFile, alpha)
  cliques <- readingQuasiCliques(census, fqsFile)
  
  # A loop where models for each clique, i.e. for each column in this clique are built.
  inputs <- lapply(seq(indexStart, indexEnd), function(i) {
    print(paste("prepare input for", i))
    list(index=i, clique=getOneClique(census, cliques, i))
  })

  cores <- detectCores() - 2 # be friendly and dont use all cores
  
  res <- mclapply(inputs, worker, mc.cores = cores)
  return(res)
}
